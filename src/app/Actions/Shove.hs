{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Actions.Shove where
import           Control.Monad.Error.Class (MonadError)
import qualified Data.Text.IO.Class        as T
import           External.Git              (IsBranch (branchName))
import           External.Git.Commands     (add, commit, currentBranch, push)
import           Text.Regex.PCRE           ((=~~))
import qualified Text.Regex.PCRE.Text      ()
import Data.Function.Suffix ((£))
import Domain.Commit.Specification (CommitSpec, CommitType, renderMsg)
import qualified Domain.Commit.Specification as S

data ShoveOptions = ShoveOptions
    { specification :: CommitOptions
    , autoPush      :: Bool
    , specific      :: Bool
    }

data CommitOptions = CommitOptions
    { commitType  :: CommitType
    , description :: Maybe Text
    }

shove :: forall m. (MonadIO m, MonadError Text m) => ShoveOptions -> m ()
shove opts = do
    add £ when $ not (specific opts)
    commit . renderMsg =<< (specify <$> collectPrefix <*> getDesc)
    push £ when $ autoPush opts
  where
    collectPrefix :: m (Maybe Text)
    collectPrefix = do
        n <- branchName <$> currentBranch
        pure $ n =~~ ("\\w+-\\d+" :: Text)

    getDesc :: m Text
    getDesc = do
        askUser £ whenNothing $ description $ specification opts
      where
        askUser :: m Text
        askUser = T.putStr "Description: " *> getLine

    specify :: Maybe Text -> Text -> CommitSpec
    specify i d = S.CommitSpec
        { S.issue = i
        , S.flags = mempty
        , S.scope = mempty
        , S.description = d
        , S.commitType = commitType $ specification opts
        }

