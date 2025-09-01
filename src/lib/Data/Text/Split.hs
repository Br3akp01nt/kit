module Data.Text.Split (splitOnMany) where
import           Data.Text (splitOn)

splitOnMany :: [Text] -> Text -> [Text]
splitOnMany [] t     = [t]
splitOnMany (x:xs) t = splitOn x t >>= splitOnMany xs

