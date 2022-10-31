-- | Platform profiles
module GHC.Platform.Profile.Class
   ( ContainsPlatformProfile (..)
   )
where

import GHC.Platform.Profile

class ContainsPlatformProfile c where
  platformProfile :: c -> Profile
