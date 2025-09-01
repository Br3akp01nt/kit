{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module External.Git
    ( IsBranch(..)
    , Remote
    , CommitHash
    , BranchName
    , Branch(..)
    , LocalBranch(..)
    , RemoteBranch(..)
    , BranchId
    , RemoteTrack(..)
    , Divergence(..)
    , hasRemoteTrack
    , showBranchId
    , isLocal
    , isRemote
    , asLocal
    , asRemote
    )
    where

type Remote = Text

type BranchName = Text

type CommitHash = Text

class IsBranch a where
    branchId   :: a -> BranchId
    branchName :: a -> BranchName

data Branch = Local LocalBranch
            | RemoteTracking RemoteBranch

asLocal :: Branch -> Maybe LocalBranch
asLocal (Local b) = Just b
asLocal _         = Nothing

asRemote :: Branch -> Maybe RemoteBranch
asRemote (RemoteTracking b) = Just b
asRemote _          = Nothing

isRemote :: Branch -> Bool
isRemote (Local  _) = False
isRemote (RemoteTracking _) = True

isLocal :: Branch -> Bool
isLocal = not . isRemote

instance IsBranch Branch where
    branchName (Local l)  = branchName l
    branchName (RemoteTracking r) = branchName r
    branchId (Local l)  = branchId l
    branchId (RemoteTracking r) = branchId r


data RemoteTrack = RemoteTrack
    { rtIdentifier :: BranchId
    , rtDivergence :: Divergence
    } deriving Show

data Divergence = Divergence
    { ahead :: Int
    , behind :: Int
    } deriving Show

data LocalBranch = LocalBranch
    { active      :: Bool
    , localName   :: BranchName
    , remoteTrack :: Maybe RemoteTrack
    } deriving (Show)

hasRemoteTrack :: LocalBranch -> Bool
hasRemoteTrack = isJust . remoteTrack

instance IsBranch LocalBranch where
    branchName = localName
    branchId l = (Nothing, l.localName)


data RemoteBranch = RemoteBranch
    { remoteBranchName :: BranchName
    , remote           :: Remote
    } deriving (Show)

instance IsBranch RemoteBranch where
    branchName = remoteBranchName
    branchId r = (Just r.remote, r.remoteBranchName)

type BranchId = (Maybe Remote, BranchName)

showBranchId :: BranchId -> Text
showBranchId (Nothing, remBranch)         = remBranch
showBranchId (Just remoteName, remBranch) = remoteName <> "/" <> remBranch

