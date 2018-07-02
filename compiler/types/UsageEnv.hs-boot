module UsageEnv where

import Weight

data IsSubweight = Smaller | Larger | Unknown

subweightMaybe :: Rig -> Rig -> IsSubweight
