module Data.Text.IO.Class (putStr, putStrLn) where

import qualified Data.Text.IO as T
import           Prelude      hiding (putStr, putStrLn)

putStr :: MonadIO m => Text -> m ()
putStr = liftIO . T.putStr

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . T.putStrLn

