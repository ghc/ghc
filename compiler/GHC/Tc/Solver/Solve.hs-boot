module GHC.Tc.Solver.Solve where

import GHC.Prelude
  ( Either )
import GHC.Tc.Solver.Monad
  ( StopOrContinue, TcS )
import GHC.Tc.Types.Constraint
  ( CtEvidence, DictCt )

solveCompletelyIfRequired
  :: Either CtEvidence DictCt -> TcS (StopOrContinue a) -> TcS (StopOrContinue a)
