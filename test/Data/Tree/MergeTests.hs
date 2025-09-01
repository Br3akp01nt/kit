module Data.Tree.MergeTests (tests) where

import           Data.Function                      ((&))
import           Data.List                          (minimum)
import           Data.List.NonEmpty                 (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Set                           as Set
import           Data.Tree                          (Tree (..), flatten,
                                                     foldTree, unfoldTree)
import           Data.Tree.Merge                    (mergeTrees)
import           Debug.Trace                        (traceWith)
import           Relude.Extra                       (Foldable1 (maximum1))
import           Test.QuickCheck                    (Property, quickCheck,
                                                     (===))
import           Test.QuickCheck.Arbitrary.NonEmpty
import           Test.QuickCheck.Test               (test)


tests :: [IO ()]
tests =
    [ quickCheck (mergeTreesPreservesDistinctValues
                  :: Tree Int -> Tree Int -> Property)
    ]


mergeTreesPreservesDistinctValues :: (Ord a, Eq a, Show a)
                                  => Tree a
                                  -> Tree a
                                  -> Property
mergeTreesPreservesDistinctValues a b =
    (((===) `on` distinctValueSet) =<< mergeTrees) trees
  where
    trees = a :| [b]
    distinctValueSet ts = Set.fromList $ flatten =<< NE.toList ts

