module GHC.Core.Subst where

import {-# SOURCE #-} GHC.Core (CoreProgram)

deShadowBinds :: CoreProgram -> CoreProgram
