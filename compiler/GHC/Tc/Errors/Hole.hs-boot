-- This boot file is in place to break the loop where:
-- + GHC.Tc.Solver calls 'GHC.Tc.Errors.reportUnsolved',
-- + which calls 'GHC.Tc.Errors.Hole.findValidHoleFits`
-- + which calls 'GHC.Tc.Solver.simpl_top'
module GHC.Tc.Errors.Hole where

import GHC.Tc.Types  ( TcM )
import GHC.Tc.Types.Constraint ( Ct, Hole, Implication )
import GHC.Utils.Outputable ( SDoc )
import GHC.Types.Var.Env ( TidyEnv )

findValidHoleFits :: TidyEnv -> [Implication] -> [Ct] -> Hole
                  -> TcM (TidyEnv, SDoc)
