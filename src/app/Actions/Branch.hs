{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE ViewPatterns #-}

module Actions.Branch (branch, BranchOptions(..)) where

import           Prelude                   hiding (intercalate)

import           Control.Monad.Catch       (MonadCatch)
import           Control.Monad.Error.Class (MonadError (throwError))
import           Control.Monad.Log.Class   (MonadLog)
import           Data.List.Monoid          (intercalate)
import qualified Data.Map                  as Map
import qualified Data.Text.IO.Class        as T
import           External.Git              (Branch (..), BranchName,
                                            IsBranch (branchId, branchName),
                                            LocalBranch (..), RemoteBranch (..),
                                            asLocal, isLocal, showBranchId)
import           External.Git.Commands     (RemoteInclusion (WithRemotes))
import qualified External.Git.Commands     as Git
import qualified External.Git              as G
import           Text.Regex.PCRE           (match)
import           Text.Regex.PCRE.Text      (Regex)
import           UI.Selection              (choose, confirmWith, dichotomous)


data BranchOptions = BranchOptions
    { searchPattern  :: Maybe Regex
    , includeRemotes :: Bool
    }


data BranchAction
    = Switch
    | Rename BranchName
    | Delete DeletionMode

data DeletionMode
  = DeleteLocal
  | DeleteIncludingTracking


displayName :: Branch -> Text
displayName (Local l) =
    branchName l
displayName (RemoteTracking b@(RemoteBranch _ r)) =
    intercalate "/" [r, branchName b]


branch :: ( MonadIO m, MonadFail m, MonadError Text m
          , MonadCatch m, MonadLog Text m)
       => BranchOptions
       -> m ()
branch opts = void $ runMaybeT $ do
    branches <- Git.list WithRemotes

    branchChoices <- hoistMaybe $ nonEmpty $ filterBranches branches

    chosenBranch <- choose listItemBranch branchChoices

    chosenAction <- decideAction chosenBranch

    executeAction chosenAction chosenBranch branches

  where

    listItemBranch :: Branch -> Text
    listItemBranch l@(Local (LocalBranch a _ r))
        =  bool "  " "* " a
        <> displayName l
        <> case r of
             Nothing  -> mempty
             Just (G.rtIdentifier -> rId) -> " (" <> showBranchId rId <> ")"

    listItemBranch r@(RemoteTracking (RemoteBranch _ _)) = "  " <> displayName r


    filterBranches :: [Branch] -> [Branch]
    filterBranches xs = foldr filter xs filters
      where
        filters = catMaybes
          [ (\p b -> p `match` displayName b)          <$> searchPattern opts
          , (\  b -> includeRemotes opts || isLocal b) <$ Just ()
          ]



executeAction :: forall m
               . (MonadIO m, MonadError Text m)
              => BranchAction
              -> Branch
              -> [Branch]
              -> m ()
executeAction action target branches =
    case (action, target) of
      (Switch, Local targetBranch) ->
          Git.switch targetBranch


      (Switch, RemoteTracking targetBranch) ->
          Git.switchRemote targetBranch
          $ Map.fromList
          $ flip mapMaybe branches $ \b -> do
              loc <- asLocal b
              track <- remoteTrack loc
              pure (G.rtIdentifier track, loc)


      (Delete _, RemoteTracking _) ->
          Git.delete target


      (Delete deletionMode, Local localBranch) ->
          void $ runMaybeT $ do
                Git.delete target
                case deletionMode of
                    DeleteLocal
                      -> pure ()

                    DeleteIncludingTracking
                      -> deleteRemoteTrackOf localBranch

        where

          deleteRemoteTrackOf :: LocalBranch -> MaybeT m ()
          deleteRemoteTrackOf b = do
              localRemote <- hoistMaybe $ do
                  track <- remoteTrack b
                  find ((G.rtIdentifier track ==) . branchId) branches

              Git.delete localRemote

              T.putStrLn $ "Deleted remote tracking branch "
                        <> showBranchId (branchId localRemote)
                        <> "."


      _ -> throwError "not implemented"



decideAction :: forall m
              . (MonadIO m, MonadCatch m, MonadError Text m, MonadFail m)
             => Branch
             -> MaybeT m BranchAction
decideAction b = do
    availableOptions <- hoistMaybe $ nonEmpty options
    (_, chosenAct) <- choose fst availableOptions
    chosenAct

  where
    options :: [(Text, MaybeT m BranchAction)]
    options
        = catMaybes
        $ [ ("switch", ) <$> Just (pure Switch)
          , ("rename", ) <$> getRenameAction
          , ("delete", ) <$> getDeleteAction
          ]

    getRenameAction :: Maybe (MaybeT m BranchAction)
    getRenameAction = Just $ do
        T.putStr $ branchName b <> " -> "
        Rename <$> getLine


    getDeleteAction :: Maybe (MaybeT m BranchAction)
    getDeleteAction = do
        guard (maybe True (not . active) $ asLocal b)
        Just $ fmap Delete $ do
            T.putStrLn $ "Delete " <> displayName b <> "?"
            guard =<< dichotomous "Are you sure?"
            case b of
                Local (LocalBranch _ _ (Just track))
                  -> do
                      includingRemote <-
                          confirmWith "Are you sure?"
                              $ dichotomous
                              $  "Also delete remote tracking branch "
                              <> showBranchId (G.rtIdentifier track)
                              <> "?"

                      pure $ if includingRemote
                                then DeleteIncludingTracking
                                else DeleteLocal

                _ -> pure DeleteLocal

