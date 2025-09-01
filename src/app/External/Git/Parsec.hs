module External.Git.Parsec (branch, restOfLine) where

import           Data.Char    (isSeparator)
import qualified Data.Text    as T
import qualified External.Git as G
import qualified Text.Parsec  as P
import           Text.Parsec  ((<?>))
import qualified Relude.Unsafe as US


type GitParser a = P.Parsec Text () a

data TrackInfo = Gone | Divergence G.Divergence

branch :: GitParser G.Branch
branch = P.choice $ P.try <$> [ G.RemoteTracking <$> remoteBranch <?> "remote branch"
                              , G.Local  <$> localBranch  <?> "local branch"
                              ]
  where
    localBranch :: GitParser (G.LocalBranch)
    localBranch = do
        isActive <- fmap isJust $ P.optionMaybe $ P.char '*'
        spaces_
        n <- branchName
        spaces_
        (_, r, _) <- branchDetails
        pure $ G.LocalBranch isActive n r

    remoteBranch :: GitParser G.RemoteBranch
    remoteBranch = do
        spaces_
        P.optional $ P.string "remotes/"
        r <- remoteName
        void $ P.char '/'
        n <- branchName
        void $ spaces *> restOfLine
        pure $ G.RemoteBranch n r

    branchDetails :: GitParser (G.CommitHash, Maybe G.RemoteTrack, Text)
    branchDetails = do
        h <- gitHash
        spaces_
        r <- fmap join $ P.optionMaybe $ betweenSquareBrackets $ do
            rId <- remoteId
            spaces_
            ti <- P.optionMaybe $ P.try $ trackInfo
            case ti of
                Nothing -> pure Nothing
                Just Gone -> pure Nothing
                Just (Divergence d) -> pure $ Just $ G.RemoteTrack rId d
        spaces_
        t <- fmap T.pack $ P.manyTill P.anyChar P.endOfLine
        pure (h, r, t)
      where
        trackInfo = do
          void $ P.char ':'
          spaces_
          gone <|> divergence

        divergence = fmap Divergence $ P.choice
            [ G.Divergence <$> (P.try ahead)
                           <*> (P.option 0 $ P.try $ P.string ", " *> behind)

            , G.Divergence 0 <$> P.try behind
            ]

        gone = P.string "gone" $> Gone
        ahead  = P.string "ahead"  *> spaces_ *> (US.read <$> some P.digit)
        behind = P.string "behind" *> spaces_ *> (US.read <$> some P.digit)

    remoteId :: GitParser G.BranchId
    remoteId = do
        rn <- remoteName
        void $ spaces *> P.char '/'
        bn <- branchName
        pure (Just rn, bn)

    remoteName :: GitParser G.Remote
    remoteName = T.pack <$> some branchChar

    branchName :: GitParser G.BranchName
    branchName = T.pack <$> some branchChar

    branchChar = P.try $ P.satisfy $ \c -> not $ or $ [ isSeparator c
                                                      , c `elem` "/()[]:\n"
                                                      ]

    gitHash :: GitParser G.CommitHash
    gitHash = T.pack <$> many (P.digit <|> P.letter)

spaces :: GitParser Text
spaces = fmap T.pack $ many $ P.char ' '

spaces_ :: GitParser ()
spaces_ = void spaces

betweenSquareBrackets :: GitParser a -> GitParser a
betweenSquareBrackets = P.between (P.char '[') (P.char ']')

restOfLine :: GitParser Text
restOfLine = T.pack <$> P.manyTill P.anyChar P.endOfLine

