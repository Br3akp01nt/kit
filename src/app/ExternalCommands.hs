{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalCommands where

import           Prelude                   hiding (intercalate)

import           Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.List.NonEmpty        as NE
import           Data.Text                 (pack)
import qualified Data.Text                 as T
import           GHC.IO.Exception          (ExitCode (..))
import           System.Directory          (findExecutable)
import           System.Info.Extra         (isWindows)
import           System.Process.Text       (readProcessWithExitCode)


hasProgram :: MonadIO m => String -> m Bool
hasProgram = liftIO . fmap (maybe False (const True)) . findExecutable

fzf :: (MonadError Text m, MonadIO m)
    => NonEmpty Text
    -> [String]
    -> m (Maybe Text)
fzf inpt args
 | isWindows = fzfUnix -- Todo
 | otherwise = fzfUnix
  where
    fzfUnix = do
        res <- liftIO $ readProcessWithExitCode
                          "fzf"
                          args
                          (T.unlines $ NE.toList inpt)
        case res of
          (ExitSuccess, out, _) -> pure $ Just out
          (ExitFailure 130, _, _) -> pure Nothing
          (ExitFailure code, _, err) -> throwError $ unwords $
              [ "fzf"
              , "failed with code"
              , pack $ show code
              , "and stderr: "
              , err
              ]

