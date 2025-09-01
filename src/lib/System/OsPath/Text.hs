module System.OsPath.Text (osPathToText, osPathFromText) where

import           Control.Monad.Catch (MonadThrow)
import qualified Data.Text           as T
import           System.OsPath       (OsPath, decodeUtf, encodeUtf)

osPathToText :: (MonadThrow m) => OsPath -> m Text
osPathToText = fmap T.pack . decodeUtf

osPathFromText :: (MonadThrow m) => Text -> m OsPath
osPathFromText = encodeUtf . T.unpack

