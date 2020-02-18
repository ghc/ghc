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

import GHC.Cmm.Graph
import GHC.Cmm.Expr
import GHC.Cmm.CLabel
import Module
import GHC.Cmm.Utils
import GHC.StgToCmm.Utils
import GHC.Driver.Types
import GHC.Driver.Session

import Control.Monad

mkTickBox :: DynFlags -> Module -> Int -> CmmAGraph
mkTickBox dflags mod n
  = mkStore tick_box (CmmMachOp (MO_Add W64)
                                [ CmmLoad tick_box b64
                                , CmmLit (CmmInt 1 W64)
                                ])
  where
    tick_box = cmmIndex dflags W64
                        (CmmLit $ CmmLabel $ mkHpcTicksLabel $ mod)
                        n

initHpc :: Module -> HpcInfo -> FCode ()
-- Emit top-level tables for HPC and return code to initialise
initHpc _ (NoHpcInfo {})
  = return ()
initHpc this_mod (HpcInfo tickCount _hashNo)
  = do dflags <- getDynFlags
       when (gopt Opt_Hpc dflags) $
           emitRawDataLits (mkHpcTicksLabel this_mod)
                           [ (CmmInt 0 W64)
                           | _ <- take tickCount [0 :: Int ..]
                           ]

