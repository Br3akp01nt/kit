module Data.Tree.TextTests where
import           Data.Tree       (Tree)
import           Data.Tree.Text  (drawListTree, drawTree)
import           Test.QuickCheck (Property, quickCheck, (===))


tests :: [IO ()]
tests =
    [ quickCheck (drawListTreeEquivalenceWithDrawTree
                  :: Tree String -> Property)
    ]


drawListTreeEquivalenceWithDrawTree :: (Monoid a, Show a, Eq a, IsString a)
                                    => Tree a
                                    -> Property
drawListTreeEquivalenceWithDrawTree =
    (drawListTree . fmap pure) `equals` drawTree
  where
    equals f g = \t -> ((===) `on` ($ t)) f g

