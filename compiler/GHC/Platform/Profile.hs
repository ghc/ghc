-- | Platform profiles
module GHC.Platform.Profile
   ( Profile (..)
   , profileBuildTag
   , profileConstants
   , profileIsProfiling
   , profileWordSizeInBytes
   )
where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Ways

-- | A platform profile fully describes the kind of objects that are generated
-- for a platform.
--
-- 'Platform' doesn't fully describe the ABI of an object. Compiler ways
-- (profiling, debug, dynamic) also modify the ABI.
--
data Profile = Profile
   { profilePlatform :: !Platform -- ^ Platform
   , profileWays     :: !Ways     -- ^ Ways
   }
  deriving (Eq, Ord, Show, Read)

-- | Get platform constants
profileConstants :: Profile -> PlatformConstants
{-# INLINE profileConstants #-}
profileConstants profile = platformConstants (profilePlatform profile)

-- | Is profiling enabled
profileIsProfiling :: Profile -> Bool
{-# INLINE profileIsProfiling #-}
profileIsProfiling profile = profileWays profile `hasWay` WayProf

-- | Word size in bytes
profileWordSizeInBytes :: Profile -> Int
{-# INLINE profileWordSizeInBytes #-}
profileWordSizeInBytes profile = platformWordSizeInBytes (profilePlatform profile)

-- | Unique build tag for the profile
profileBuildTag :: Profile -> String
profileBuildTag profile
    -- profiles using unregisterised convention are not binary compatible with
    -- those that don't. Make sure to make it apparent in the tag so that our
    -- interface files can't be mismatched by mistake.
  | platformUnregisterised platform = 'u':wayTag
  | otherwise                       =     wayTag
  where
   platform = profilePlatform profile
   wayTag   = waysBuildTag (profileWays profile)
