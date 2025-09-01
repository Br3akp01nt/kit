{-# LANGUAGE ScopedTypeVariables #-}

module Data.List.NonEmpty.Ord (groupAllWith') where

import           Data.Maybe (fromJust)

groupAllWith' :: forall a
               . (a -> a -> Bool)
              -> NonEmpty a
              -> NonEmpty (NonEmpty a)
groupAllWith' f = fromJust . nonEmpty
                . flip execState []
                . mapM_ (modify' . placeItem)
  where
    placeItem :: a -> [NonEmpty a] -> [NonEmpty a]
    placeItem i [] = [pure i]
    placeItem i (bin@(j :| _): bins) =
      if f i j
         then [bin <> pure i] <> bins
         else bin : placeItem i bins

