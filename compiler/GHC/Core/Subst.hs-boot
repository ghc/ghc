module GHC.Core.Subst where

import {-# SOURCE #-} GHC.Core ( CoreExpr )
import {-# SOURCE #-} GHC.Core.TyCo.Subst ( Subst )
import GHC.Utils.Misc ( HasDebugCallStack )

substExpr :: HasDebugCallStack => Subst -> CoreExpr -> CoreExpr
