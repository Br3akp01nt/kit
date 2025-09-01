{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module External.Git.Internal
    ( GitCommand
    , git
    , output
    , handleJustWith
    )
    where

import           Control.Monad.Error.Class (MonadError (throwError))
import           Data.Text                 (pack)
import           GHC.IO.Exception          (ExitCode (..))
import           System.Process.Text       (readProcessWithExitCode)


newtype CmdName = CmdName { unCmdName :: Text }


data GitCommand m a = GitCommand
    { runGitCommand :: m (ExitCode, a, Text)
    , cmdName       :: CmdName
    }

instance Functor m => Functor (GitCommand m) where
    fmap f cmd = cmd
        { runGitCommand = (\(c, x, e) -> (c, f x, e)) <$> runGitCommand cmd
        }


git :: (MonadError Text m, MonadIO m) => String -> [String] -> GitCommand m Text
git cmd args = GitCommand
    (liftIO $ readProcessWithExitCode "git" (cmd:args) "")
    (CmdName $ pack cmd)


output :: (MonadError Text m, MonadIO m) => GitCommand m a -> m a
output cmd = do
    res <- runGitCommand cmd
    case res of
      (ExitSuccess, out, _)      -> pure out
      (ExitFailure code, _, err) -> defaultErrorHandler code err cmd.cmdName


defaultErrorHandler :: (MonadError Text m)
                    => Int
                    -> Text
                    -> CmdName
                    -> m a
defaultErrorHandler code err name
    = throwError
    $ unwords
    $ [ "git"
      , unCmdName name
      , "failed with code"
      , pack $ show code
      , "and stderr: "
      , err
      ]


handleJustWith :: (MonadError Text m, MonadIO m)
               => (Int -> Text -> Maybe a)
               -> GitCommand m a
               -> m a
handleJustWith h cmd = do
    res <- runGitCommand cmd
    case res of
      (ExitSuccess, out, _) -> pure out
      (ExitFailure code, _, err) ->
          case h code err of
            Just handled -> pure handled
            Nothing      -> defaultErrorHandler code err cmd.cmdName

