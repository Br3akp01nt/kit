module Data.List.Index where

indexed :: [b] -> [(Int, b)]
indexed = zipWith (,) [0..]

