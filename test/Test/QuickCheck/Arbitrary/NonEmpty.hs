module Test.QuickCheck.Arbitrary.NonEmpty where

import           Test.QuickCheck    (Arbitrary (..), suchThatMap)

import qualified Data.List.NonEmpty as NE


instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = arbitrary `suchThatMap` NE.nonEmpty


