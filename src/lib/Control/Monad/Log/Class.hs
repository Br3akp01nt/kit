{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Log.Class where

class Monad m => MonadLog l m | m -> l where
  writeLog :: l -> m ()

instance MonadLog l m => MonadLog l (ReaderT e m) where
  writeLog = lift . writeLog

instance MonadLog l m => MonadLog l (MaybeT m) where
  writeLog = lift . writeLog

