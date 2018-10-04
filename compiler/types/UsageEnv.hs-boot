module UsageEnv where

import Weight

data IsSubweight = Smaller | Larger | Unknown

subweightMaybe :: GMult t -> GMult t -> IsSubweight
