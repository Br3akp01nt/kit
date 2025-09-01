{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module External.Git.Grep
  ( GrepOptions(..)
  , GrepResult
  , ResultLine(..)
  , GrepResultLine
  , CaseSensitivity(..)
  , grep
  , resultLineText
  , multiLineEntries
  )
  where

import           Control.Monad.Catch       (MonadThrow)
import           Control.Monad.Error.Class (MonadError)
import           Data.Char                 (isSeparator)
import           Data.Composition          ((.:))
import           Data.List                 (nub)
import           Data.List.Extra           ((!!))
import qualified Data.List.NonEmpty        as NE
import           Data.List.NonEmpty.Extra  (minimum1)
import           Data.Text                 (pack, unpack)
import qualified Data.Text                 as T
import           External.Git.Internal     (git, handleJustWith)
import           System.OsPath.Text        (osPathFromText, osPathToText)
import           System.OsPath.Types       (OsPath)
import qualified Text.Parsec               as P
import           Text.Parsec.Trans.Natural (hoistParsecT)
import           Text.Regex.PCRE           (match)
import           Text.Regex.PCRE.Text      (Regex)

data CaseSensitivity
  = Sensitive
  | Insensitive

data GrepOptions = GrepOptions
  { pattern                :: Text
  , fileGlobs              :: [Text]
  , fileRegexes            :: [Regex]
  , includeFunctionContext :: Bool
  , caseSensitivity        :: CaseSensitivity
  , fileExclusionRegex     :: [Regex]
  }

multiLineEntries :: GrepOptions -> Bool
multiLineEntries o = or [o.includeFunctionContext]

type GrepResult = (OsPath, [GrepResultLine])

data ResultLine a
  = ContextHeader a
  | GrepMatch     a
  | ContextLine   a
  deriving Functor

type GrepResultLine = ResultLine Text

resultLineText :: GrepResultLine -> Text
resultLineText (ContextHeader t) = t
resultLineText (GrepMatch     t) = t
resultLineText (ContextLine   t) = t

grep :: forall m
      . (MonadError Text m, MonadThrow m, MonadIO m)
     => GrepOptions
     -> m [GrepResult]
grep opts = filterResults =<< gitGrep
  where
    filterResults :: [GrepResult] -> m [GrepResult]
    filterResults = flip (foldlM $ \rs f -> f rs) filters
      where
        filters = concat
            [ (filterM . onPath . (not .: match)) <$> opts.fileExclusionRegex
            , (filterM . onPath . match) <$> opts.fileRegexes
            ]
        onPath f = fmap f . osPathToText . fst


    gitGrep :: m [GrepResult]
    gitGrep = do
        grepOutput <- (git "grep" $ unpack <$> concat grepArgs)
                      & handleJustWith (\_ err ->
                          case err of
                            "" -> Just ""
                            _  -> Nothing
                        )

        pure $ mapMaybe parseEntry $ splitEntries grepOutput
      where
        grepArgs = catMaybes
                 $ [ case opts.caseSensitivity of
                       Sensitive   -> Nothing
                       Insensitive -> Just ["-i"]
                   , guard opts.includeFunctionContext $> ["-W"]
                   , pure ["-P", opts.pattern]
                   , (("--" :) . NE.toList) `viaNonEmpty` opts.fileGlobs
                   ]

        splitEntries :: Text -> [Text]
        splitEntries = bool T.lines (T.splitOn "\n--\n") $ multiLineEntries opts

    parseEntry :: Text -> Maybe GrepResult
    parseEntry = either (fail . show) pure <=< P.runParserT entryParser () ""

    entryParser :: P.ParsecT Text () Maybe GrepResult
    entryParser = do
        lns <- hoistParsecT $ some $ do
            x <- P.choice $ [ contextHeader
                            , grepMatch
                            , contextLine
                            ]
            pure x
        let paths = nub $ fst <$> lns
        guard $ length paths == 1
        path <- lift $ osPathFromText $ paths !! 0
        pure (path, stripBlock $ snd <$> lns)
      where
        contextHeader = lineParser ContextHeader (P.char '=')
        grepMatch     = lineParser GrepMatch     (P.char ':')
        contextLine   = lineParser ContextLine   (P.char '-')

        lineParser :: (Text -> GrepResultLine)
                   -> P.Parsec Text () b
                   -> P.Parsec Text () (Text, GrepResultLine)
        lineParser f p = do
            path <- P.try $ P.manyTill (P.satisfy $ not . isSeparator)
                                       (P.try $ P.lookAhead p)
            void p
            l <- restOfLine
            pure (pack path, f l)

        restOfLine :: P.Parsec Text () Text
        restOfLine = fmap pack
                   $ P.manyTill P.anyChar
                   $ P.eof <|> void P.endOfLine

        stripBlock :: [GrepResultLine] -> [GrepResultLine]
        stripBlock lns =
            let minTrim = fromMaybe 0
                        $  (  viaNonEmpty minimum1
                          <=< mapM (T.findIndex $ not . isSeparator)
                           )
                        $ map resultLineText lns
             in fmap (T.drop minTrim) <$> lns

