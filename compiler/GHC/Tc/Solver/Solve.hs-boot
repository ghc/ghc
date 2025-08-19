module GHC.Tc.Solver.Solve where

import Prelude( Bool )
import GHC.Tc.Solver.Monad( TcS )
import GHC.Tc.Types.Constraint( Cts, Implication, WantedConstraints )

solveSimpleWanteds :: Cts -> TcS WantedConstraints
trySolveImplication :: Implication -> TcS Bool
