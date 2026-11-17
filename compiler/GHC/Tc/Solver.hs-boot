module GHC.Tc.Solver where

import GHC.Prelude
import GHC.Core.Reduction ( Reduction )
import GHC.Tc.Types ( TcM )
import GHC.Tc.Types.CtLoc ( CtLoc )
import GHC.Tc.Utils.TcType ( TcType )

-- See Note [Bounded frontend family views] in GHC.Tc.Solver.
tcRewriteFamilyAxioms :: CtLoc -> TcType -> TcM Reduction
