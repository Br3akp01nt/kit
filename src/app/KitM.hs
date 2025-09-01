{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module KitM where

import           Control.Monad.Catch       (MonadThrow (..))
import           Control.Monad.Catch.Pure  (MonadCatch (..))
import           Control.Monad.Error.Class (MonadError (..))

newtype KitM e a = KitM { unKitM :: ExceptT e IO a }
  deriving (Functor, Applicative, Monad)

instance MonadError e (KitM e) where
  throwError = KitM . throwError
  catchError m h = KitM $ catchError (unKitM m) (unKitM . h)

instance MonadThrow (KitM e) where
  throwM = KitM . throwM

instance MonadCatch (KitM e) where
  catch m h = KitM $ catch (unKitM m) (unKitM . h)

instance MonadFail (KitM e) where
  fail = KitM . fail

instance MonadIO (KitM e) where
  liftIO = KitM . liftIO

runKitM :: KitM e a -> (e -> IO a) -> IO a
runKitM k h = unKitM k
            & (runExceptT >=> either h pure)

