module GHC.Tc.Solver.Solve where

import Prelude( Bool )
import GHC.Tc.Solver.Monad( TcS )
import GHC.Tc.Types.Constraint( Cts, Implication )

solveSimpleWanteds :: Cts -> TcS Cts
trySolveImplication :: Implication -> TcS Bool
