{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- Code generation for coverage
--
-- (c) Galois Connections, Inc. 2006
--
-----------------------------------------------------------------------------

module CgHpc (cgTickBox, initHpc, hpcTable) where

import Cmm
import CLabel
import Module
import MachOp
import CmmUtils
import CgUtils
import CgMonad
import CgForeignCall
import ForeignCall
import ClosureInfo
import FastString
import HscTypes
import Char
import StaticFlags
import PackageConfig 

import Data.Word

cgTickBox :: Module -> Int -> Code
cgTickBox mod n = do
       let tick_box = (cmmIndex I64
                       (CmmLit $ CmmLabel $ mkHpcTicksLabel $ mod)
                       (fromIntegral n)
                      )
       stmtsC [ CmmStore tick_box
                         (CmmMachOp (MO_Add I64)
                                               [ CmmLoad tick_box I64
                                               , CmmLit (CmmInt 1 I64)
                                               ])
              ] 

hpcTable :: Module -> HpcInfo -> Code
hpcTable this_mod (HpcInfo hpc_tickCount _) = do
                        emitData ReadOnlyData
                                        [ CmmDataLabel mkHpcModuleNameLabel
                                        , CmmString $ map (fromIntegral . ord)
                                                         (full_name_str)
                                                      ++ [0]
                                        ]
                        emitData Data $ [ CmmDataLabel (mkHpcTicksLabel this_mod)
                                        ] ++
                                        [ CmmStaticLit (CmmInt 0 I64)
                                        | _ <- take hpc_tickCount [0::Int ..]
                                        ]
  where
    module_name_str = moduleNameString (Module.moduleName this_mod)
    full_name_str   = if modulePackageId this_mod == mainPackageId 
		      then module_name_str
		      else packageIdString (modulePackageId this_mod) ++ "/" ++
			   module_name_str

hpcTable this_mod (NoHpcInfo {}) = error "TODO: impossible"

initHpc :: Module -> HpcInfo -> Code
initHpc this_mod (HpcInfo tickCount hashNo)
  = do { id <- newNonPtrTemp wordRep -- TODO FIXME NOW
       ; emitForeignCall'
               PlayRisky
               [(id,NoHint)]
               (CmmCallee
                 (CmmLit $ CmmLabel $ mkForeignLabel mod_alloc Nothing False)
                  CCallConv
               )
               [ (mkLblExpr mkHpcModuleNameLabel,PtrHint)
               , (word32 tickCount, NoHint)
               , (word32 hashNo,    NoHint)
               , (CmmLit $ CmmLabel $ mkHpcTicksLabel $ this_mod,PtrHint)
               ]
               (Just [])
               NoC_SRT -- No SRT b/c we PlayRisky
               CmmMayReturn
       }
  where
       word32 i = CmmLit (CmmInt (fromIntegral (fromIntegral i :: Word32)) I32)
       mod_alloc = mkFastString "hs_hpc_module"

