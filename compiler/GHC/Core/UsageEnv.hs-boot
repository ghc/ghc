module GHC.Core.UsageEnv where

import Control.Applicative
import {-# SOURCE #-} GHC.Core.TyCo.Rep (Mult)

data Usage -- = Zero | Bottom | MUsage Mult
data UsageEnv -- = UsageEnv !(NameEnv Mult) Bool

nonDetMults :: UsageEnv -> [Mult]
mapUE :: (Mult -> Mult) -> UsageEnv -> UsageEnv
mapUEM :: Applicative m => (Mult -> m Mult) -> UsageEnv -> m UsageEnv
