module GHC.Tc.Solver.Solve where

import GHC.Tc.Solver.Monad
import GHC.Tc.Types.Constraint

solveSimpleWanteds :: Cts -> TcS WantedConstraints
