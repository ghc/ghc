-----------------------------------------------------------------------------
--
-- Code generation for coverage
--
-- (c) Galois Connections, Inc. 2006
--
-----------------------------------------------------------------------------

{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module StgCmmHpc ( initHpc, mkTickBox ) where

import StgCmmUtils
import StgCmmMonad
import StgCmmForeign
import StgCmmClosure

import MkZipCfgCmm
import Cmm
import CLabel
import Module
import CmmUtils
import ForeignCall
import FastString
import HscTypes
import Char
import StaticFlags
import PackageConfig 

mkTickBox :: Module -> Int -> CmmAGraph
mkTickBox mod n 
  = mkStore tick_box (CmmMachOp (MO_Add W64)
                                [ CmmLoad tick_box b64
                                , CmmLit (CmmInt 1 W64)
                                ])
  where
    tick_box = cmmIndex W64
                        (CmmLit $ CmmLabel $ mkHpcTicksLabel $ mod)
                        (fromIntegral n)

initHpc :: Module -> HpcInfo -> FCode CmmAGraph
-- Emit top-level tables for HPC and return code to initialise
initHpc this_mod (NoHpcInfo {}) 
  = return mkNop
initHpc this_mod (HpcInfo tickCount hashNo)
  = getCode $ whenC opt_Hpc $
    do	{ emitData ReadOnlyData
              [ CmmDataLabel mkHpcModuleNameLabel
              , CmmString $ map (fromIntegral . ord)
                               (full_name_str)
                            ++ [0]
              ]
        ; emitData Data $ [ CmmDataLabel (mkHpcTicksLabel this_mod)
              ] ++
              [ CmmStaticLit (CmmInt 0 W64)
              | _ <- take tickCount [0::Int ..]
              ]

    	; id <- newTemp bWord -- TODO FIXME NOW
        ; emitCCall
               [(id,NoHint)]
               (CmmLit $ CmmLabel $ mkForeignLabel mod_alloc Nothing False)
               [ (mkLblExpr mkHpcModuleNameLabel,AddrHint)
               , (CmmLit $ mkIntCLit tickCount,NoHint)
               , (CmmLit $ mkIntCLit hashNo,NoHint)
               , (CmmLit $ CmmLabel $ mkHpcTicksLabel $ this_mod,AddrHint)
               ]
       }
  where
    mod_alloc = mkFastString "hs_hpc_module"
    module_name_str = moduleNameString (Module.moduleName this_mod)
    full_name_str   = if modulePackageId this_mod == mainPackageId 
		      then module_name_str
		      else packageIdString (modulePackageId this_mod) ++ "/" ++
			   module_name_str


         
