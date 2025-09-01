{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Text.Parsec.Trans.Natural where
import           Control.Natural (type (~>))
import           Text.Parsec     (Consumed (..), Parsec, ParsecT, mkPT,
                                  runParsecT)

mapParsecTM :: (Monad m, Monad n)
            => (m ~> n)
            -> ParsecT s u m a
            -> ParsecT s u n a
mapParsecTM nat p = mkPT $ \s -> nat $ mapConsumed nat <$> runParsecT p s
  where
    mapConsumed f (Consumed a) = Consumed $ f a
    mapConsumed f (Empty    a) = Empty $ f a

hoistParsecT :: Monad m => Parsec s u a -> ParsecT s u m a
hoistParsecT = mapParsecTM $ pure . runIdentity

