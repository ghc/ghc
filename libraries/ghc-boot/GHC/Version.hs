{-# LANGUAGE CPP #-}
module GHC.Version where

#ifndef GIT_COMMIT_ID
#define GIT_COMMIT_ID 000000000000000000000000000000000000000
#endif

import PackageInfo_ghc_boot (version)
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import Data.Version (showVersion, versionBranch)

import Prelude -- See Note [Why do we import Prelude here?]

cProjectGitCommitId   :: String
cProjectGitCommitId   = "GIT_COMMIT_ID"

cProjectVersion       :: String
cProjectVersion       = showVersion version

cProjectVersionInt    :: String
cProjectVersionInt    = concatMap show (versionBranch version)

cProjectPatchLevel    :: String
cProjectPatchLevel    = case (versionBranch version !? 2, versionBranch version !? 3) of
                          (Just pl1, Just pl2) -> show pl1 ++ show pl2
                          (Just pl1, Nothing)  -> show pl1
                          _                    -> "0"

cProjectPatchLevel1   :: String
cProjectPatchLevel1   = show $ fromMaybe 0 (versionBranch version !? 2)

cProjectPatchLevel2   :: String
cProjectPatchLevel2   = show $ fromMaybe 0 (versionBranch version !? 3)
