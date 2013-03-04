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
import LlvmCodeGen.Regs

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
pprLlvmHeader = sdocWithDynFlags $ \dflags ->
    moduleLayout
    $+$ text ""
    $+$ ppLlvmFunctionDecls (map snd (ghcInternalFunctions dflags))
    $+$ ppLlvmMetas stgTBAA
    $+$ text ""


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
    Platform { platformArch = ArchARM {}, platformOS = OSiOS } ->
        text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32\""
        $+$ text "target triple = \"arm-apple-darwin10\""
    _ ->
        -- FIX: Other targets
        empty


-- | Pretty print LLVM data code
pprLlvmData :: LlvmData -> SDoc
pprLlvmData (globals, types) =
    let tryConst (v, Just s )   = ppLlvmGlobal (v, Just s)
        tryConst g@(_, Nothing) = ppLlvmGlobal g

        ppLlvmTys (LMAlias    a) = ppLlvmAlias a
        ppLlvmTys (LMFunction f) = ppLlvmFunctionDecl f
        ppLlvmTys _other         = empty

        types'   = vcat $ map ppLlvmTys types
        globals' = vcat $ map tryConst globals
    in types' $+$ globals'


-- | Pretty print LLVM code
pprLlvmCmmDecl :: LlvmEnv -> Int -> LlvmCmmDecl -> (SDoc, [LlvmVar])
pprLlvmCmmDecl _ _ (CmmData _ lmdata)
  = (vcat $ map pprLlvmData lmdata, [])

pprLlvmCmmDecl env count (CmmProc mb_info entry_lbl live (ListGraph blks))
  = let (idoc, ivar) = case mb_info of
                        Nothing -> (empty, [])
                        Just (Statics info_lbl dat)
                         -> pprInfoTable env count info_lbl (Statics entry_lbl dat)
    in (idoc $+$ (
        let sec = mkLayoutSection (count + 1)
            (lbl',sec') = case mb_info of
                           Nothing                   -> (entry_lbl, Nothing)
                           Just (Statics info_lbl _) -> (info_lbl,  sec)
            link = if externallyVisibleCLabel lbl'
                      then ExternallyVisible
                      else Internal
            lmblocks = map (\(BasicBlock id stmts) ->
                                LlvmBlock (getUnique id) stmts) blks
            fun = mkLlvmFunc env live lbl' link  sec' lmblocks
        in ppLlvmFunction fun
    ), ivar)


-- | Pretty print CmmStatic
pprInfoTable :: LlvmEnv -> Int -> CLabel -> CmmStatics -> (SDoc, [LlvmVar])
pprInfoTable env count info_lbl stat
  = let dflags = getDflags env
        unres = genLlvmData env (Text, stat)
        (_, (ldata, ltypes)) = resolveLlvmData env unres

        setSection ((LMGlobalVar _ ty l _ _ c), d)
            = let sec = mkLayoutSection count
                  ilabel = strCLabel_llvm env info_lbl
                              `appendFS` fsLit iTableSuf
                  gv = LMGlobalVar ilabel ty l sec (llvmInfAlign dflags) c
                  v = if l == Internal then [gv] else []
              in ((gv, d), v)
        setSection v = (v,[])

        (ldata', llvmUsed) = setSection (last ldata)
    in if length ldata /= 1
          then Outputable.panic "LlvmCodeGen.Ppr: invalid info table!"
          else (pprLlvmData ([ldata'], ltypes), llvmUsed)


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

