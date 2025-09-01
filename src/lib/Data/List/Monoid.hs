module Data.List.Monoid (intersperse, intercalate) where

import           Prelude hiding (intercalate, intersperse)

intercalate :: Monoid a => a -> [a] -> a
intercalate a = mconcat . intersperse a

intersperse :: Monoid a => a -> [a] -> [a]
intersperse _ []     = mempty
intersperse _ [x]    = [x]
intersperse a (x:xs) = x : a : intersperse a xs

