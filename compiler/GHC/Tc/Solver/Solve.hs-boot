module GHC.Tc.Solver.Solve where

import GHC.Tc.Solver.Monad( TcS )
import GHC.Tc.Types.Constraint( Cts, WantedConstraints )

solveSimpleWanteds :: Cts -> TcS WantedConstraints
