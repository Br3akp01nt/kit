{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actions.Shove where
import           Control.Monad.Error.Class (MonadError)
import qualified Data.Text.IO.Class        as T
import           External.Git              (IsBranch (branchName))
import           External.Git.Commands     (add, commit, currentBranch, push)
import           Text.Regex.PCRE           ((=~~))
import qualified Text.Regex.PCRE.Text      ()
import Data.Function.Suffix ((£))

data ShoveOptions = ShoveOptions
    { message  :: Maybe Text
    , autoPush :: Bool
    , specific :: Bool
    }

shove :: forall m. (MonadIO m, MonadError Text m) => ShoveOptions -> m ()
shove opts = do
    add £ when $ not (specific opts)
    commit =<< getMsg =<< collectPrefix
    push £ when $ autoPush opts
  where
    collectPrefix :: m (Maybe Text)
    collectPrefix = runMaybeT $ do
        n <- branchName <$> currentBranch
        ticketId <- hoistMaybe $ n =~~ ("BLIK-\\d+" :: String)
        pure $ ticketId <> " | "

    getMsg :: Maybe Text -> m Text
    getMsg prfx = do
        msg <- askUser £ whenNothing $ message opts
        pure $ fromMaybe "" prfx <> msg
      where
        askUser :: m Text
        askUser = do
            T.putStr $ "Commit message: " <> fromMaybe "" prfx
            getLine

