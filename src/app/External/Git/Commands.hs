{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module External.Git.Commands
    ( switch
    , switchRemote
    , delete
    , list
    , repoName
    , add
    , commit
    , push
    , currentBranch
    , createBranch
    , grep
    , update
    , RemoteInclusion(..)
    )
    where

import           Control.Monad.Catch       (MonadThrow)
import           Control.Monad.Error.Class (MonadError (throwError),
                                            modifyError)
import           Control.Monad.Log.Class   (MonadLog (writeLog))
import           Data.Map                  ((!?))
import           Data.Text                 (unpack)
import qualified Data.Text                 as T
import qualified Data.Text.IO.Class        as T
import           External.Git              (Branch (..), BranchKind(..),
                                            branchId, branchName,
                                            hasRemoteTrack, showBranchId)
import           External.Git.Grep         (grep)
import           External.Git.Internal     (git, output)
import qualified External.Git.Parsec       as GP (branch, restOfLine)
import           System.OsPath             (encodeUtf, takeBaseName)
import           System.OsPath.Text        (osPathToText)
import qualified Text.Parsec               as P
import qualified Text.Parsec.Control       as P
import           Text.Parsec.Trans.Natural (hoistParsecT)
import qualified External.Git as G


data RemoteInclusion
    = NoRemotes
    | WithRemotes


switch :: (MonadIO m, MonadError Text m) => Branch 'Local -> m ()
switch (LocalBranch _ n _) = void $ git "switch" [T.unpack n] & output

switchRemote :: (MonadIO m, MonadError Text m)
             => Branch 'RemoteTracking
             -> Map G.BranchId (Branch 'Local)
             -> m ()
switchRemote r locals = do
    case locals !? branchId r of
      Nothing -> void $ output $
          git "switch"
              ["--track", T.unpack $ showBranchId $ branchId r]

      Just b  -> switch b

delete :: (MonadIO m, MonadError Text m) => Branch k -> m ()
delete (LocalBranch _ n _) = output cmd >>= T.putStrLn
  where
    cmd = git "branch" ["-d", T.unpack n]
delete (RemoteBranch n r)
    = output cmd >>= T.putStrLn
  where
    cmd = git "push" [T.unpack r, "--delete", T.unpack n]


repoName :: (MonadIO m, MonadError Text m, MonadThrow m) => m Text
repoName = do
    rn <- git "rev-parse" ["--show-toplevel"] & output
    path <- takeBaseName <$> encodeUtf (unpack rn)
    T.strip <$> osPathToText path

add :: (MonadIO m, MonadError Text m) => m ()
add = void $ git "add" ["-A"] & output

push :: (MonadIO m, MonadError Text m) => m ()
push = void $ git "push" [] & output

commit :: (MonadIO m, MonadError Text m) => Text -> m ()
commit msg = void $ git "commit" ["-m", unpack msg] & output

currentBranch :: (MonadIO m, MonadError Text m) => m G.SomeBranch
currentBranch = do
    branchRes <- git "branch" ["--show-current"] & output
    modifyError show $ P.runParserError GP.branch () "" branchRes

createBranch :: (MonadIO m, MonadError Text m) => Text -> m (Branch 'Local)
createBranch name = do
    void $ git "branch" [unpack name] & output
    pure $ LocalBranch False name Nothing

list :: (MonadIO m, MonadError Text m, MonadLog Text m)
     => RemoteInclusion
     -> m [G.SomeBranch]
list includeRemotes = do
    gitResults <- output $
        git "branch" $
            ["-vv"] <> case includeRemotes of
                         NoRemotes   -> []
                         WithRemotes -> ["-a"]

    parseResult <-
            P.runParserT
              (some $ P.choice [ hoistParsecT $ P.try (Just <$> GP.branch)
                               , do rest <- hoistParsecT $ GP.restOfLine
                                    lift $ writeLog $  "Failed to parse line: "
                                                    <> show rest
                                    pure $ Nothing
                               ])
              ()
              ""
              gitResults
    case parseResult of
      Left err       -> throwError $ show err
      Right branches -> pure $ catMaybes branches

update :: (MonadIO m, MonadError Text m) => Branch 'Local -> m ()
update branch = do
    validate (hasRemoteTrack branch) $  branchName branch
                                     <> " has no remote track"

validate :: MonadError e m => Bool -> e -> m ()
validate False err = throwError err
validate True  _   = pure ()

