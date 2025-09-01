{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Control
  ( runParserFail
  , runParserTFail
  , runParserError
  , runParserTError
  )
  where

import           Control.Monad.Error.Class (MonadError (throwError))
import qualified Text.Parsec               as P

runParserFail :: (P.Stream s Identity t, MonadFail m)
              => P.Parsec s u a
              -> u
              -> P.SourceName
              -> s
              -> m a
runParserFail p u n s = either (fail . show) pure $ P.runParser p u n s

runParserTFail :: (P.Stream s m t, MonadFail m)
               => P.ParsecT s u m a
               -> u
               -> P.SourceName
               -> s
               -> m a
runParserTFail p u n s = P.runParserT p u n s >>= either (fail . show) pure

runParserError :: (P.Stream s Identity t, MonadError P.ParseError m)
               => P.Parsec s u a
               -> u
               -> P.SourceName
               -> s
               -> m a
runParserError p u n s = either throwError pure $ P.runParser p u n s

runParserTError :: (P.Stream s m t, MonadError P.ParseError m)
                => P.ParsecT s u m a
                -> u
                -> P.SourceName
                -> s
                -> m a
runParserTError p u n s = P.runParserT p u n s >>= either throwError pure

