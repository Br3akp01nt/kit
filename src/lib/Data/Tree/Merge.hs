module Data.Tree.Merge (mergeTrees, mergeTreesBy) where

import           Data.List.NonEmpty.Ord (groupAllWith')
import           Data.Tree              (Tree (..))

mergeTrees :: Ord a => NonEmpty (Tree a) -> NonEmpty (Tree a)
mergeTrees = mergeTreesBy (==)

mergeTreesBy :: (a -> a -> Bool) -> NonEmpty (Tree a) -> NonEmpty (Tree a)
mergeTreesBy f = fmap mergeGroup
               . groupAllWith' (f `on` rootLabel)
  where
    mergeGroup (t:|ts) = Node (rootLabel t)
                       $ maybe [] (toList . mergeTreesBy f)
                       $ nonEmpty
                       $ subForest =<< (t:ts)

