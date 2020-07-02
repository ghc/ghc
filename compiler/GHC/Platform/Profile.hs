-- | Platform profiles
module GHC.Platform.Profile
   ( Profile (..)
   , profileBuildTag
   )
where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Ways

import Data.Set

-- | A platform profile fully describes the kind of objects that are generated
-- for a platform.
--
-- 'Platform' doesn't fully describe the ABI of an object. Compiler ways
-- (profiling, debug, dynamic) also modify the ABI.
--
data Profile = Profile
   { profilePlatform :: !Platform  -- ^ Platform
   , profileWays     :: !(Set Way) -- ^ Ways
   }

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
