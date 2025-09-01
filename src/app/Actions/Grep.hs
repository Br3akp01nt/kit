{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Actions.Grep (GrepOptions(..), PresentationMode(..), grep) where

import           Control.Monad.Catch       (MonadThrow)
import           Control.Monad.Error.Class (MonadError)
import           Data.List.Monoid          (intercalate)
import qualified Data.Text                 as T
import qualified Data.Text.IO.Class        as T
import           Data.Traversable          (for)
import           Data.Tree                 (Forest, Tree (..), unfoldForest)
import           Data.Tree.Merge           (mergeTreesBy)
import           Data.Tree.Text            (drawListTree)
import qualified External.Git.Commands     as Git
import           External.Git.Grep         (GrepResult, GrepResultLine,
                                            ResultLine (..), multiLineEntries,
                                            resultLineText)
import qualified External.Git.Grep
import           Prelude                   hiding (intercalate)
import           System.OsPath             (OsPath, splitPath)
import           System.OsPath.Text        (osPathToText)
import           Text.Format.Justify       (justify)
import           Text.Regex.PCRE.Text      (Regex)

data GrepOptions = GrepOptions
  { pattern                :: Text
  , fileGlobs              :: [Text]
  , fileRegexes            :: [Regex]
  , presentationMode       :: PresentationMode
  , includeFunctionContext :: Bool
  , caseSensitivity        :: External.Git.Grep.CaseSensitivity
  , fileExclusionRegex     :: [Regex]
  }

grepOpts :: GrepOptions -> External.Git.Grep.GrepOptions
grepOpts opts = External.Git.Grep.GrepOptions
                    opts.pattern
                    opts.fileGlobs
                    opts.fileRegexes
                    opts.includeFunctionContext
                    opts.caseSensitivity
                    opts.fileExclusionRegex

data PresentationMode = ListMode | TreeMode

resultLineMarker :: GrepResultLine -> Text
resultLineMarker (ContextHeader _) = ">>>"
resultLineMarker (ContextLine   _) = "  #"
resultLineMarker (GrepMatch     _) = "==="

grep :: (MonadError Text m, MonadThrow m, MonadIO m) => GrepOptions -> m ()
grep opts = Git.grep (grepOpts opts) >>= (present >=> T.putStrLn)
  where
    present rs = case opts.presentationMode of
      ListMode -> listPresentation opts rs
      TreeMode -> treePresentation opts rs


listPresentation :: (MonadThrow m) => GrepOptions -> [GrepResult] -> m Text
listPresentation _ [] = pure ""
listPresentation opts results = do
    groups <- for results $ \(path, resultLines) -> do
        decodedPath <- osPathToText path
        pure $ map (showResultLine decodedPath) resultLines

    let formattedGroups = justify $ intercalate [[spacer, "", ""]] groups

    pure $ T.unlines $ T.unwords <$> formattedGroups

  where

    showResultLine path resultLine =
      [path, resultLineMarker resultLine, resultLineText resultLine]

    spacer = if multiLineEntries $ grepOpts opts
                then "--"
                else T.empty


treePresentation :: forall m
                  . (MonadError Text m, MonadIO m, MonadThrow m)
                 => GrepOptions
                 -> [GrepResult]
                 -> m Text
treePresentation _ [] = pure ""
treePresentation opts results = do
    rn <- Git.repoName
    entryTree <- buildEntryTree rn results
    pure $ drawListTree $ entryTree
  where
    buildEntryTree :: Text -> [GrepResult] -> m (Tree [Text])
    buildEntryTree rn rs = do
        let entries = entryForest rs
        textTrees <- for entries $ traverse showNode
        pure $ Node [rn] textTrees
      where
        entryForest :: [GrepResult]
                    -> [Tree (Either [GrepResultLine] OsPath)]
        entryForest = maybe [] toList
                    . viaNonEmpty (mergeTreesBy equivalentPath)
                    . unfoldEntryForest

        showNode :: MonadThrow m => Either [GrepResultLine] OsPath -> m [Text]
        showNode (Right p) = pure <$> osPathToText p
        showNode (Left ls) = do
            pure $ map (\l -> prefix l <> resultLineText l) ls

        prefix l = if multiLineEntries $ grepOpts opts
                           then resultLineMarker l <> " "
                           else ""

        unfoldEntryForest :: [GrepResult]
                          -> Forest (Either [GrepResultLine] OsPath)
        unfoldEntryForest
             =  map (first splitPath)
            >>> unfoldForest (\case
                  ([],   val) -> (Left val, [])
                  (p:ps, val) -> (Right p, [(ps, val)]))

        equivalentPath :: Either [GrepResultLine] OsPath
                       -> Either [GrepResultLine] OsPath
                       -> Bool
        equivalentPath a b = fromRight False $ (==) <$> a <*> b

