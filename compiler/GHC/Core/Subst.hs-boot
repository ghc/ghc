module GHC.Core.Subst where

import {-# SOURCE #-} GHC.Core (CoreBind)

deShadowBinds :: [CoreBind] -> [CoreBind]
