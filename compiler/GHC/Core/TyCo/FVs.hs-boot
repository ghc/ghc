module GHC.Core.TyCo.FVs where

import GHC.Prelude ( Bool )
import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type )

noFreeVarsOfType :: Type -> Bool
