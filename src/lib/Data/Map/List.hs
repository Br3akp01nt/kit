module Data.Map.List (fromListOn, keyedBy) where
import           Relude.Extra (toFst)

fromListOn :: Ord k => (a -> k) -> [a] -> Map k a
fromListOn f = fromList . fmap (toFst f)

keyedBy :: Ord k => [a] -> (a -> k) -> Map k a
keyedBy = flip fromListOn

