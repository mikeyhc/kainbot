module Kain.Travis.Internal where

import qualified Data.ByteString as B
import qualified Data.Map as M

type RepoName = B.ByteString

data RepoState = RepoState
    { repoOwner   :: B.ByteString
    , repoAddress :: B.ByteString
    }

data TravisState = TravisState
    { travisRepos :: M.Map RepoName RepoState }
