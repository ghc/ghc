-- ----------------------------------------------------------------------------
-- | Pretty print helpers for the LLVM Code generator.
--

module LlvmCodeGen.Ppr (
        pprLlvmHeader, pprLlvmCmmDecl, pprLlvmData, infoSection, iTableSuf
    ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base
import LlvmCodeGen.Data

import CLabel
import Cmm
import Platform

import FastString
import Outputable
import Unique


-- ----------------------------------------------------------------------------
-- * Top level
--

-- | Header code for LLVM modules
pprLlvmHeader :: SDoc
pprLlvmHeader = moduleLayout


-- | LLVM module layout description for the host target
moduleLayout :: SDoc
moduleLayout = sdocWithPlatform $ \platform ->
    case platform of
    Platform { platformArch = ArchX86, platformOS = OSDarwin } ->
        text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:128:128-n8:16:32\""
        $+$ text "target triple = \"i386-apple-darwin9.8\""
    Platform { platformArch = ArchX86, platformOS = OSMinGW32 } ->
        text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-f80:128:128-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32\""
        $+$ text "target triple = \"i686-pc-win32\""
    Platform { platformArch = ArchX86, platformOS = OSLinux } ->
        text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32\""
        $+$ text "target triple = \"i386-pc-linux-gnu\""
    Platform { platformArch = ArchX86_64, platformOS = OSDarwin } ->
        text "target datalayout = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64\""
        $+$ text "target triple = \"x86_64-apple-darwin10.0.0\""
    Platform { platformArch = ArchX86_64, platformOS = OSLinux } ->
        text "target datalayout = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64\""
        $+$ text "target triple = \"x86_64-linux-gnu\""
    Platform { platformArch = ArchARM {}, platformOS = OSLinux } ->
        text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32\""
        $+$ text "target triple = \"arm-unknown-linux-gnueabi\""
    Platform { platformArch = ArchARM {}, platformOS = OSAndroid } ->
        text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32\""
        $+$ text "target triple = \"arm-unknown-linux-androideabi\""
    Platform { platformArch = ArchARM {}, platformOS = OSQNXNTO } ->
        text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32\""
        $+$ text "target triple = \"arm-unknown-nto-qnx8.0.0eabi\""
    Platform { platformArch = ArchARM {}, platformOS = OSiOS } ->
        text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32\""
        $+$ text "target triple = \"arm-apple-darwin10\""
    _ ->
        -- FIX: Other targets
        empty


-- | Pretty print LLVM data code
pprLlvmData :: LlvmData -> SDoc
pprLlvmData (globals, types) =
    let ppLlvmTys (LMAlias    a) = ppLlvmAlias a
        ppLlvmTys (LMFunction f) = ppLlvmFunctionDecl f
        ppLlvmTys _other         = empty

        types'   = vcat $ map ppLlvmTys types
        globals' = ppLlvmGlobals globals
    in types' $+$ globals'


-- | Pretty print LLVM code
pprLlvmCmmDecl :: Int -> LlvmCmmDecl -> LlvmM (SDoc, [LlvmVar])
pprLlvmCmmDecl _ (CmmData _ lmdata)
  = return (vcat $ map pprLlvmData lmdata, [])

pprLlvmCmmDecl count (CmmProc mb_info entry_lbl live (ListGraph blks))
  = do (idoc, ivar) <- case mb_info of
                        Nothing -> return (empty, [])
                        Just (Statics info_lbl dat)
                         -> pprInfoTable count info_lbl (Statics entry_lbl dat)

       let sec = mkLayoutSection (count + 1)
           (lbl',sec') = case mb_info of
                           Nothing                   -> (entry_lbl, Nothing)
                           Just (Statics info_lbl _) -> (info_lbl,  sec)
           link = if externallyVisibleCLabel lbl'
                      then ExternallyVisible
                      else Internal
           lmblocks = map (\(BasicBlock id stmts) ->
                                LlvmBlock (getUnique id) stmts) blks

       fun <- mkLlvmFunc live lbl' link  sec' lmblocks

       return (idoc $+$ ppLlvmFunction fun, ivar)


-- | Pretty print CmmStatic
pprInfoTable :: Int -> CLabel -> CmmStatics -> LlvmM (SDoc, [LlvmVar])
pprInfoTable count info_lbl stat
  = do (ldata, ltypes) <- genLlvmData (Text, stat)

       dflags <- getDynFlags
       let setSection (LMGlobal (LMGlobalVar _ ty l _ _ c) d) = do
             lbl <- strCLabel_llvm info_lbl
             let sec = mkLayoutSection count
                 ilabel = lbl `appendFS` fsLit iTableSuf
                 gv = LMGlobalVar ilabel ty l sec (llvmInfAlign dflags) c
                 v = if l == Internal then [gv] else []
             funInsert ilabel ty
             return (LMGlobal gv d, v)
           setSection v = return (v,[])

       (ldata', llvmUsed) <- setSection (last ldata)
       if length ldata /= 1
          then Outputable.panic "LlvmCodeGen.Ppr: invalid info table!"
          else return (pprLlvmData ([ldata'], ltypes), llvmUsed)


-- | We generate labels for info tables by converting them to the same label
-- as for the entry code but adding this string as a suffix.
iTableSuf :: String
iTableSuf = "_itable"


-- | Create a specially crafted section declaration that encodes the order this
-- section should be in the final object code.
-- 
-- The LlvmMangler.llvmFixupAsm pass over the assembly produced by LLVM uses
-- this section declaration to do its processing.
mkLayoutSection :: Int -> LMSection
mkLayoutSection n
  = Just (fsLit $ infoSection ++ show n)


-- | The section we are putting info tables and their entry code into, should
-- be unique since we process the assembly pattern matching this.
infoSection :: String
infoSection = "X98A__STRIP,__me"

