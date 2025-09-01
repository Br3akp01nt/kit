{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Tree.MergeTests as MergeTests
import qualified Data.Tree.TextTests  as TreeTextTests
import           Test.QuickCheck


main :: IO ()
main = sequence_ tests
  where
    tests = concat
      [ MergeTests.tests
      , TreeTextTests.tests
      ]

