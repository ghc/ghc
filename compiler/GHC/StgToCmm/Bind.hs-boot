module GHC.StgToCmm.Bind where

import GHC.StgToCmm.Monad( FCode )
import GHC.Stg.Syntax( CgStgBinding )
import qualified GHC.Utils.Misc as S (HasDebugCallStack)

cgBind :: S.HasDebugCallStack => CgStgBinding -> FCode ()
