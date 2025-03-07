module GHC.Tc.Solver.Solve where

import GHC.Tc.Solver.Monad
  ( StopOrContinue, TcS )
import GHC.Tc.Types.Constraint
  ( Ct )

solveCompletelyIfRequired :: Ct -> TcS (StopOrContinue a) -> TcS (StopOrContinue a)
