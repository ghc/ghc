module GHC.Core.Subst where

import {-# SOURCE #-} GHC.Core (CoreBind, CoreCompUnit)

deShadowBinds :: [CoreBind] -> [CoreBind]
deShadowCompUnits :: [CoreCompUnit] -> [CoreCompUnit]
