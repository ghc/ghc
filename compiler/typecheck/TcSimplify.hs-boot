module TcSimplify where

import GhcPrelude
import TcRnTypes  ( TcM, Cts )
import TcType ( TcSigmaType )

-- This boot file exists solely to make tcCheckHoleFit and tcSubsumes avaialble
-- in TcErrors

tcSubsumes :: TcSigmaType -> TcSigmaType -> TcM Bool
tcCheckHoleFit :: Cts -> TcSigmaType -> TcSigmaType -> TcM Bool
