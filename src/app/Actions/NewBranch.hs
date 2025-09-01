{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actions.NewBranch (newBranch, NewBranchOptions(..)) where

import           Prelude                   hiding (intercalate)

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Extra       (untilJustM)
import           Data.List.Monoid          (intercalate)
import qualified Data.Text                 as T
import qualified Data.Text.IO.Class        as T
import qualified External.Git              as Git
import qualified External.Git.Commands     as Git
import qualified Text.Parsec               as P
import qualified Text.Parsec.Control       as P
import           UI.Selection              (dichotomous)


data NewBranchOptions = NewBranchOptions
    {
    }

newBranch :: forall m
           . (MonadIO m, MonadError Text m)
          => NewBranchOptions
          -> m ()
newBranch _ = do
    issue <- getIssue
    name  <- getName
    branch <- createBranch issue name
    Git.switch branch & whenM (dichotomous "Switch to new branch?")
  where
    getIssue :: m (Maybe Text)
    getIssue = untilJustM $ runMaybeT $ do
        userResponse <- putStr "Issue ID: " *> getLine
        case userResponse of
          ""  -> Nothing <$  pure ()
          txt -> Just    <$> P.runParserFail issueParser () "" txt
      where
        issueParser = do
          prefix <- some P.letter
          sep <- P.char '-'
          issueNr <- some P.digit
          pure $ T.pack $ prefix <> [sep] <> issueNr

    getName :: m Text
    getName = putStr "Branch name: " *> getLine

    createBranch :: Maybe Text -> Text -> m Git.LocalBranch
    createBranch issue name = do
        let branchName = intercalate "_" parts
        T.putStrLn $ "Creating branch: " <> branchName
        Git.createBranch branchName
      where
        parts = maybe [] pure issue <> [name]

