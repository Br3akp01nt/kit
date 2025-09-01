{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.Text (drawTree, drawListTree) where

import           Data.Composition ((.:))
import qualified Data.List.Monoid as M
import           Data.Tree        (Tree, foldTree)


drawTree :: forall s. (Monoid s, IsString s) => Tree s -> s
drawTree = drawListTree . fmap pure


drawListTree :: forall s. (Monoid s, IsString s) => Tree [s] -> s
drawListTree = M.intercalate "\n" .: foldTree $ \n ts -> case n of
    []     -> "" : addIndent ts
    (x:xs) -> x : xs <> addIndent ts
  where
    addIndent :: [[s]] -> [s]
    addIndent []     = []
    addIndent [g]    = "|" : shift "`- " "   " g
    addIndent (g:gs) = "|" : shift "+- " "|  " g <> addIndent gs

    shift top other = zipWith (<>) (top:repeat other)

