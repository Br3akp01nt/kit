module Text.Format.Pad (padRight) where

import qualified Data.Text as T

padRight :: Int -> Char -> Text -> Text
padRight n a xs = mconcat $ xs : replicate (n - T.length xs) (T.singleton a)

