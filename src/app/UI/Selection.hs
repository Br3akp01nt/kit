{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Selection (listChoice, dichotomous, choose, confirmWith) where

import           Control.Monad.Catch       (MonadCatch, MonadThrow, onException)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Extra       (fromMaybeM, untilJustM)
import           Data.List.NonEmpty.Extra  ((!?))
import           Data.List.NonEmpty.Index  (indexed)
import           Data.Text                 (toLower)
import qualified Data.Text                 as T
import qualified Data.Text.IO.Class        as T
import           ExternalCommands          (fzf, hasProgram)
import           System.Info.Extra         (isWindows)

listChoice :: forall m a. (MonadFail m, MonadThrow m, MonadCatch m, MonadIO m)
           => (a -> Text) -> NonEmpty a -> m a
listChoice s xs = untilJustM $ runMaybeT $ do
    forM_ (indexed xs) $ \(i, x) -> do
        T.putStrLn $ show i <> ") " <> s x

    i :: Int <- MaybeT $ readMaybe . T.unpack <$> getUserLine

    case xs !? i of
      Nothing -> putStrLn "Invalid index" *> empty
      Just x  -> pure x
  where
    getUserLine :: m Text
    getUserLine = getLine `onException` (fail "Operation canceled")


dichotomous :: MonadIO m => Text -> m Bool
dichotomous prompt = untilJustM $ runMaybeT $ do
    userResponse <- T.putStr (prompt <> " (y/n): ") *> getLine
    MaybeT $ pure $ case toLower userResponse of
      "y" -> Just True
      "n" -> Just False
      _   -> Nothing


confirmWith :: MonadIO m => Text -> m Bool -> m Bool
confirmWith prompt a = fromMaybeM (pure False) $ runMaybeT $ do
    guard =<< lift a
    guard =<< dichotomous prompt
    pure True


choose :: ( MonadFail m, MonadError Text m, MonadThrow m
          , MonadCatch m, MonadIO m)
       => (a -> Text) -> NonEmpty a -> MaybeT m a
choose s xs = hasFzf >>= bool (lift $ listChoice s xs) fuzzyChoose
  where
    hasFzf = hasProgram "fzf" <&> (&& (not isWindows))

    fuzzyChoose = do
        res <- fmap T.strip $ MaybeT $ fzf (s <$> xs) ["--no-sort"]
        hoistMaybe $ find ((== res) . T.strip . s) xs

