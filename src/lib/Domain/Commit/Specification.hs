{-# LANGUAGE OverloadedStrings #-}

module Domain.Commit.Specification where

import Control.Monad.Writer (execWriter, MonadWriter (tell))
import qualified Data.Text as T

data CommitSpec = CommitSpec
    { commitType  :: CommitType
    , issue       :: Maybe Text
    , description :: Text
    , scope       :: Maybe Text
    , flags       :: Set CommitFlag
    }

data CommitType
    = Feature
    | Fix
    | Refactor
    | Performance
    | Style
    | Test
    | Documentation
    | Build
    | Operations
    | Chore

displayType :: CommitType -> Text
displayType t =
    case t of
      Feature       -> "feat"
      Fix           -> "fix"
      Refactor      -> "refactor"
      Performance   -> "perf"
      Style         -> "style"
      Test          -> "test"
      Documentation -> "docs"
      Build         -> "build"
      Operations    -> "ops"
      Chore         -> "chore"

data CommitFlag
    = Breaking
  deriving (Eq, Ord)

displayFlag :: CommitFlag -> Text
displayFlag f =
    case f of
      Breaking -> "!"

renderMsg :: CommitSpec -> Text
renderMsg spec = execWriter $ do
    whenJust (issue spec) $ \i -> do
        tell $ i <> " | "
    tell $ displayType (commitType spec) <> ": "
    whenJust (scope spec) $ \s -> do
        tell $ "(" <> s <> ")"
    tell $ T.concat . map displayFlag $ toList $ flags spec
    tell $ description spec

