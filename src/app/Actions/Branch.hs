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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Actions.Branch (branch, BranchOptions(..)) where

import           Prelude                   hiding (intercalate)

import           Control.Monad.Catch       (MonadCatch)
import           Control.Monad.Error.Class (MonadError (throwError))
import           Control.Monad.Log.Class   (MonadLog)
import           Data.List.Monoid          (intercalate)
import qualified Data.Map                  as Map
import qualified Data.Text.IO.Class        as T
import           External.Git              (Branch (..), BranchName,
                                            branchId, branchName,
                                            BranchKind(..), remoteTrack,
                                            asLocal, isLocal, showBranchId,
                                            RemoteTrack (..))
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


data BranchAction k
    = Switch (Branch k)
    | Rename (Branch 'Local) BranchName
    | Delete (Branch k)


displayName :: Branch k -> Text
displayName l@(LocalBranch {}) =
    branchName l
displayName b@(RemoteBranch _ r) =
    intercalate "/" [r, branchName b]


branch :: ( MonadIO m, MonadFail m, MonadError Text m
          , MonadCatch m, MonadLog Text m)
       => BranchOptions
       -> m ()
branch opts = void $ runMaybeT $ do
    branches <- Git.list WithRemotes

    branchChoices <- hoistMaybe $ nonEmpty $ filterBranches branches

    chosenBranch <- choose (G.onSomeBranch listItemBranch) branchChoices

    G.withSomeBranch chosenBranch $ \cb -> do
        chosenAction <- decideAction cb

        runReaderT (executeAction chosenAction) branches
  where

    listItemBranch :: Branch k -> Text
    listItemBranch l@(LocalBranch a _ r)
        =  bool "  " "* " a
        <> displayName l
        <> case r of
             Nothing  -> mempty
             Just (G.rtIdentifier -> rId) -> " (" <> showBranchId rId <> ")"

    listItemBranch r@(RemoteBranch _ _) = "  " <> displayName r


    filterBranches :: [G.SomeBranch] -> [G.SomeBranch]
    filterBranches xs = foldr filter xs filters
      where
        filters = catMaybes
          [ (\p b -> p `match` G.onSomeBranch displayName b)          <$> searchPattern opts
          , (\  b -> includeRemotes opts || G.onSomeBranch isLocal b) <$ Just ()
          ]



executeAction :: forall m k
               . (MonadIO m, MonadError Text m)
              => BranchAction k
              -> ReaderT [G.SomeBranch] m ()
executeAction action =
    case action of
      (Switch targetBranch@(LocalBranch {})) ->
          Git.switch targetBranch

      (Switch targetBranch@(RemoteBranch {})) -> do
          branches <- ask
          Git.switchRemote targetBranch
            $ Map.fromList
            $ flip mapMaybe branches $ \b -> do
                loc <- G.onSomeBranch asLocal b
                track <- remoteTrack loc
                pure (G.rtIdentifier track, loc)

      (Delete b) -> void $ runMaybeT $ do
          Git.delete b
          r <- MaybeT $ reciprocal b
          guard =<< confirmWith "Are you sure?"
             ( dichotomous
             $ "Also delete reciprocal branch "
            <> showBranchId (branchId r)
            <> "?"
             )
          Git.delete r

        where
          reciprocal :: Branch k
                     -> ReaderT [G.SomeBranch] m (Maybe (Branch (G.ReciprocalKind k)))
          reciprocal b' = do
              branches <- ask
              case b' of
                lb@(LocalBranch {})
                 | Just rt <- remoteTrack lb
                  -> pure
                   $ find
                       ((rtIdentifier rt ==) . branchId)
                       (mapMaybe (G.onSomeBranch G.asRemote) branches)

                rb@(RemoteBranch {})
                 | rbId <- branchId rb
                 -> let isMatch b'' = fromMaybe False $ do
                          rt <- remoteTrack b''
                          pure $ rbId == rtIdentifier rt
                        localBranches =
                          mapMaybe
                            (G.onSomeBranch G.asLocal)
                            branches
                     in pure $ find isMatch localBranches

                _ -> do
                    pure Nothing

      _ -> throwError "not implemented"



decideAction :: forall m k
              . (MonadIO m, MonadCatch m, MonadError Text m, MonadFail m)
             => Branch k
             -> MaybeT m (BranchAction k)
decideAction b = do
    availableOptions <- hoistMaybe $ nonEmpty options
    (_, chosenAct) <- choose fst availableOptions
    chosenAct

  where
    options :: [(Text, MaybeT m (BranchAction k))]
    options
        = catMaybes
        $ [ ("switch", ) <$> Just (pure $ Switch b)
          , ("rename", ) <$> getRenameAction
          , ("delete", ) <$> getDeleteAction
          ]

    getRenameAction :: Maybe (MaybeT m (BranchAction k))
    getRenameAction = do
        localB <- asLocal b
        pure $ do
            T.putStr $ branchName b <> " -> "
            Rename localB <$> getLine


    getDeleteAction :: Maybe (MaybeT m (BranchAction k))
    getDeleteAction = do
        guard (maybe True (not . G.active) $ asLocal b)
        Just $ fmap Delete $ do
            T.putStrLn $ "Delete " <> displayName b <> "?"
            guard =<< dichotomous "Are you sure?"
            pure b

