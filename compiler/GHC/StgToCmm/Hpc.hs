-----------------------------------------------------------------------------
--
-- Code generation for coverage
--
-- (c) Galois Connections, Inc. 2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.Hpc ( initHpc, mkTickBox ) where

import GHC.Prelude
import GHC.Platform

import GHC.Driver.Session

import GHC.StgToCmm.Monad
import GHC.StgToCmm.Utils

import GHC.Cmm.Graph
import GHC.Cmm.Expr
import GHC.Cmm.CLabel
import GHC.Cmm.Utils

import GHC.Unit.Module
import GHC.Types.HpcInfo

import Control.Monad

mkTickBox :: Platform -> Module -> Int -> CmmAGraph
mkTickBox platform mod n
  = mkStore tick_box (CmmMachOp (MO_Add W64)
                                [ CmmLoad tick_box b64 NaturallyAligned
                                , CmmLit (CmmInt 1 W64)
                                ])
  where
    tick_box = cmmIndex platform W64
                        (CmmLit $ CmmLabel $ mkHpcTicksLabel $ mod)
                        n

-- | Emit top-level tables for HPC and return code to initialise
initHpc :: Module -> HpcInfo -> FCode ()
initHpc _ (NoHpcInfo {})
  = return ()
initHpc this_mod (HpcInfo tickCount _hashNo)
  = do dflags <- getDynFlags
       when (gopt Opt_Hpc dflags) $
           emitDataLits (mkHpcTicksLabel this_mod)
                        [ (CmmInt 0 W64)
                        | _ <- take tickCount [0 :: Int ..]
                        ]

