-----------------------------------------------------------------------------
--
-- Code generation for coverage
--
-- (c) Galois Connections, Inc. 2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.Hpc ( initHpc, mkTickBox ) where

import GhcPrelude

import GHC.StgToCmm.Monad

import GHC.Platform
import GHC.Cmm.Graph
import GHC.Cmm.Expr
import GHC.Cmm.CLabel
import GHC.Types.Module
import GHC.Cmm.Utils
import GHC.StgToCmm.Utils
import GHC.Driver.Types
import GHC.Driver.Session

import Control.Monad

mkTickBox :: Platform -> Module -> Int -> CmmAGraph
mkTickBox platform mod n
  = mkStore tick_box (CmmMachOp (MO_Add W64)
                                [ CmmLoad tick_box b64
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

