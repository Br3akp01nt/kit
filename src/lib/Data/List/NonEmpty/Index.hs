module Data.List.NonEmpty.Index where
import qualified Data.List.NonEmpty as NE

indexed :: NonEmpty a -> NonEmpty (Int, a)
indexed = NE.zipWith (,) (0 :| [1..])

