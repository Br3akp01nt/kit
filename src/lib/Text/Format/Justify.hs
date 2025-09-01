module Text.Format.Justify (justify) where

import           Data.List.NonEmpty.Extra (maximum1)
import qualified Data.Text                as T
import           Text.Format.Pad          (padRight)

justify :: [[Text]] -> [[Text]]
justify = transpose . map justifyCol . transpose
  where
    justifyCol [] = []
    justifyCol rs = map (padRight (maxLength rs) ' ') rs

    maxLength rs = maybe 0 maximum1 $ nonEmpty $ map T.length rs

