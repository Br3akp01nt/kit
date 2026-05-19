{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies #-}

module External.Git
    ( Remote
    , CommitHash
    , BranchName
    , Branch(..)
    , BranchKind(..)
    , BranchId
    , RemoteTrack(..)
    , Divergence(..)
    , SomeBranch(..)
    , ReciprocalKind
    , hasRemoteTrack
    , showBranchId
    , isLocal
    , isRemote
    , asLocal
    , asRemote
    , active
    , localName
    , branchId
    , branchName
    , onSomeBranch
    , withSomeBranch
    , remoteTrack
    )
    where

type Remote = Text

type BranchName = Text

type CommitHash = Text

data SomeBranch where
  SomeBranch :: Branch k -> SomeBranch

onSomeBranch :: (forall k. Branch k -> a) -> SomeBranch -> a
onSomeBranch f (SomeBranch b) = f b

withSomeBranch :: SomeBranch -> (forall k. Branch k -> a) -> a
withSomeBranch (SomeBranch b) f = f b

data BranchKind = Local | RemoteTracking

type family ReciprocalKind (k :: BranchKind) where
    ReciprocalKind 'Local = 'RemoteTracking
    ReciprocalKind 'RemoteTracking = 'Local

data Branch (k :: BranchKind) where
    LocalBranch  :: Bool -> BranchName -> Maybe RemoteTrack -> Branch Local
    RemoteBranch :: BranchName -> Remote -> Branch RemoteTracking

branchId   :: Branch k -> BranchId
branchId (LocalBranch _ n rt) = (rt >>= fst . rtIdentifier, n)
branchId (RemoteBranch n r) = (Just r, n)

branchName :: Branch k -> BranchName
branchName (LocalBranch _ n _) = n
branchName (RemoteBranch n _) = n

active :: Branch 'Local -> Bool
active (LocalBranch a _ _) = a

localName :: Branch 'Local -> BranchName
localName (LocalBranch _ n _) = n

remoteTrack :: Branch 'Local -> Maybe RemoteTrack
remoteTrack (LocalBranch _ _ r) = r

hasRemoteTrack :: Branch 'Local -> Bool
hasRemoteTrack = isJust . remoteTrack

asLocal :: Branch k -> Maybe (Branch 'Local)
asLocal b@(LocalBranch {}) = Just b
asLocal _                  = Nothing

asRemote :: Branch k -> Maybe (Branch 'RemoteTracking)
asRemote b@(RemoteBranch {}) = Just b
asRemote _                   = Nothing

isRemote :: Branch k -> Bool
isRemote (LocalBranch {}) = False
isRemote (RemoteBranch {}) = True

isLocal :: Branch k -> Bool
isLocal = not . isRemote

data RemoteTrack = RemoteTrack
    { rtIdentifier :: BranchId
    , rtDivergence :: Divergence
    } deriving Show

data Divergence = Divergence
    { ahead :: Int
    , behind :: Int
    } deriving Show

type BranchId = (Maybe Remote, BranchName)

showBranchId :: BranchId -> Text
showBranchId (Nothing, remBranch)         = remBranch
showBranchId (Just remoteName, remBranch) = remoteName <> "/" <> remBranch

