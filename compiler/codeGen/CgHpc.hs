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
import FastString
import HscTypes
import Char
import StaticFlags

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
   where
      visible_tick = mkFastString "hs_hpc_tick"

hpcTable :: Module -> HpcInfo -> Code
hpcTable this_mod (HpcInfo hpc_tickCount _) = do
                        emitData ReadOnlyData
                                        [ CmmDataLabel mkHpcModuleNameLabel
                                        , CmmString $ map (fromIntegral . ord)
                                                         (module_name_str)
                                                      ++ [0]
                                        ]
                        emitData Data $ [ CmmDataLabel (mkHpcTicksLabel this_mod)
                                        ] ++
                                        [ CmmStaticLit (CmmInt 0 I64)
                                        | _ <- take hpc_tickCount [0..]
                                        ]
  where
    module_name_str = moduleNameString (Module.moduleName this_mod)
hpcTable this_mod (NoHpcInfo) = error "TODO: impossible"

initHpc :: Module -> HpcInfo -> Code
initHpc this_mod (HpcInfo tickCount hashNo)
  = do { id <- newNonPtrTemp wordRep -- TODO FIXME NOW
       ; emitForeignCall'
               PlayRisky
               [(id,NoHint)]
               (CmmForeignCall
                 (CmmLit $ CmmLabel $ mkForeignLabel mod_alloc Nothing False)
                  CCallConv
               )
               [ (mkLblExpr mkHpcModuleNameLabel,PtrHint)
               , (CmmLit $ mkIntCLit tickCount,NoHint)
               , (CmmLit $ mkIntCLit hashNo,NoHint)
               , (CmmLit $ CmmLabel $ mkHpcTicksLabel $ this_mod,PtrHint)
               ]
               (Just [])
       }
  where
       mod_alloc = mkFastString "hs_hpc_module"

