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
import CmmUtils
import CgUtils
import CgMonad
import CgForeignCall
import ForeignCall
import ClosureInfo
import FastString
import HscTypes
import Panic
import BasicTypes

import Data.Char
import Data.Word

cgTickBox :: Module -> Int -> Code
cgTickBox mod n = do
       let tick_box = (cmmIndex W64
                       (CmmLit $ CmmLabel $ mkHpcTicksLabel $ mod)
                       n
                      )
       stmtsC [ CmmStore tick_box
                         (CmmMachOp (MO_Add W64)
                                               [ CmmLoad tick_box b64
                                               , CmmLit (CmmInt 1 W64)
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
                                        [ CmmStaticLit (CmmInt 0 W64)
                                        | _ <- take hpc_tickCount [0::Int ..]
                                        ]
  where
    module_name_str = moduleNameString (Module.moduleName this_mod)
    full_name_str   = if modulePackageId this_mod == mainPackageId 
		      then module_name_str
		      else packageIdString (modulePackageId this_mod) ++ "/" ++
			   module_name_str

hpcTable _ (NoHpcInfo {}) = error "TODO: impossible"

initHpc :: Module -> HpcInfo -> Code
initHpc this_mod (HpcInfo tickCount hashNo)
  = do { id <- newTemp bWord
       ; emitForeignCall'
               PlayRisky
               [CmmHinted id NoHint]
               (CmmCallee
                 (CmmLit $ CmmLabel $ mkForeignLabel mod_alloc Nothing ForeignLabelInThisPackage IsFunction)
                  CCallConv
               )
               [ CmmHinted (mkLblExpr mkHpcModuleNameLabel) AddrHint
               , CmmHinted (word32 tickCount) NoHint
               , CmmHinted (word32 hashNo)    NoHint
               , CmmHinted (CmmLit $ CmmLabel $ mkHpcTicksLabel $ this_mod) AddrHint
               ]
               (Just [])
               NoC_SRT -- No SRT b/c we PlayRisky
               CmmMayReturn
       }
  where
       word32 i = CmmLit (CmmInt (fromIntegral (fromIntegral i :: Word32)) W32)
       mod_alloc = mkFastString "hs_hpc_module"
initHpc _ (NoHpcInfo {}) = panic "initHpc: NoHpcInfo"

