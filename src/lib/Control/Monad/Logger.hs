{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Control.Monad.Logger (LoggerT, runLoggerT, mapLoggerT) where

import           Control.Monad.Catch     (MonadCatch (..), MonadThrow (..))
import           Control.Monad.Except    (MonadError (..))
import           Control.Monad.Log.Class (MonadLog (writeLog))
import           Control.Monad.Reader    (mapReaderT)
import           Control.Natural         (type (~>))

newtype LoggerT l m a = LoggerT { unLoggerT :: ReaderT (l -> m ()) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runLoggerT :: (l -> m ()) -> LoggerT l m a -> m a
runLoggerT f = flip runReaderT f . unLoggerT

mapLoggerT :: forall m n l a
            . (m ~> n)
           -> (n ~> m)
           -> LoggerT l m a
           -> LoggerT l n a
mapLoggerT nat unNat =
  LoggerT . withReaderT (unNat .) . mapReaderT nat . unLoggerT

instance MonadTrans (LoggerT l) where
  lift = LoggerT . lift

instance MonadReader r m => MonadReader r (LoggerT l m) where
  ask = lift ask
  local f = LoggerT . mapReaderT (local f) . unLoggerT

instance MonadError e m => MonadError e (LoggerT l m) where
  throwError = lift . throwError
  catchError a f = LoggerT $ catchError (unLoggerT a) (unLoggerT . f)

instance MonadThrow m => MonadThrow (LoggerT l m) where
   throwM e = lift $ throwM e

instance MonadCatch m => MonadCatch (LoggerT l m) where
  catch a f = LoggerT $ catch (unLoggerT a) (unLoggerT . f)

instance MonadFail m => MonadFail (LoggerT l m) where
  fail = LoggerT . fail

instance Monad m => MonadLog l (LoggerT l m) where
  writeLog l = LoggerT $ ask >>= (lift . ($ l))

