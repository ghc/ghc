module TcSimplify where

import GhcPrelude
import TcRnTypes  ( TcM, Cts )
import TcType ( TcSigmaType )

-- This boot file exists solely to make tcCheckHoleFit avaialble in TcErrors

tcCheckHoleFit :: Cts -> TcSigmaType -> TcSigmaType -> TcM Bool
