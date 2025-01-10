-- This boot file is in place to break the loop where:
-- + GHC.Tc.Solver calls 'GHC.Tc.Errors.reportUnsolved',
-- + which calls 'GHC.Tc.Errors.Hole.findValidHoleFits`
-- + which calls 'GHC.Tc.Solver.simpl_top'
module GHC.Tc.Errors.Hole where

import GHC.Tc.Errors.Types ( HoleFitDispConfig, ValidHoleFits )
import GHC.Tc.Types  ( TcM )
import GHC.Tc.Types.Constraint ( CtEvidence, Hole, Implication )
import GHC.Types.Var.Env ( TidyEnv )

findValidHoleFits :: TidyEnv -> [Implication] -> [CtEvidence] -> Hole
                  -> TcM (TidyEnv, ValidHoleFits)

getHoleFitDispConfig :: TcM HoleFitDispConfig

