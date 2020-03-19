-- This boot file is in place to break the loop where:
-- + GHC.Tc.Simplify calls 'GHC.Tc.Errors.reportUnsolved',
-- + which calls 'GHC.Tc.Hole.findValidHoleFits`
-- + which calls 'GHC.Tc.Simplify.simpl_top'
module GHC.Tc.Hole where

import GHC.Tc.Utils  ( TcM )
import GHC.Tc.Utils.Constraint ( Ct, Implication )
import Outputable ( SDoc )
import GHC.Types.Var.Env ( TidyEnv )

findValidHoleFits :: TidyEnv -> [Implication] -> [Ct] -> Ct
                  -> TcM (TidyEnv, SDoc)
