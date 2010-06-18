-- ----------------------------------------------------------------------------
-- | Pretty print helpers for the LLVM Code generator.
--

module LlvmCodeGen.Ppr (
        pprLlvmHeader, pprLlvmCmmTop, pprLlvmData
    ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base
import LlvmCodeGen.Data

import CLabel
import Cmm

import DynFlags
import FastString
import Pretty
import Unique
import Util

-- ----------------------------------------------------------------------------
-- * Top level
--

-- | LLVM module layout description for the host target
moduleLayout :: Doc
moduleLayout =
#ifdef i386_TARGET_ARCH

#ifdef darwin_TARGET_OS
    text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:128:128\""
    $+$ text "target triple = \"i386-apple-darwin9.8\""
#else
    text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32\""
    $+$ text "target triple = \"i386-linux-gnu\""
#endif

#else

#ifdef x86_64_TARGET_ARCH
    text "target datalayout = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128\""
    $+$ text "target triple = \"x86_64-linux-gnu\""

#else /* Not i386 */
    -- FIX: Other targets
    empty
#endif

#endif


-- | Header code for LLVM modules
pprLlvmHeader :: Doc
pprLlvmHeader = moduleLayout


-- | Pretty print LLVM code
pprLlvmCmmTop :: DynFlags -> LlvmEnv -> Int -> LlvmCmmTop -> (Doc, [LlvmVar])
pprLlvmCmmTop dflags _ _ (CmmData _ lmdata)
  = (vcat $ map (pprLlvmData dflags) lmdata, [])

pprLlvmCmmTop dflags env count (CmmProc info lbl _ (ListGraph blks))
  = let static = CmmDataLabel lbl : info
        (idoc, ivar) = if not (null info)
                          then pprCmmStatic dflags env count static
                          else (empty, [])
    in (idoc $+$ (
        let sec = mkLayoutSection (count + 1)
            (lbl',sec') = if not (null info)
                            then (entryLblToInfoLbl lbl, sec)
                            else (lbl, Nothing)
            link = if externallyVisibleCLabel lbl'
                      then ExternallyVisible
                      else Internal
            funDec = llvmFunSig lbl' link
            lmblocks = map (\(BasicBlock id stmts) ->
                                LlvmBlock (getUnique id) stmts) blks
            fun = LlvmFunction funDec [NoUnwind] sec' lmblocks
        in ppLlvmFunction fun
    ), ivar)


-- | Pretty print LLVM data code
pprLlvmData :: DynFlags -> LlvmData -> Doc
pprLlvmData _ (globals, types) =
    let globals' = ppLlvmGlobals globals
        types'   = ppLlvmTypes types
    in types' $+$ globals'


-- | Pretty print CmmStatic
pprCmmStatic :: DynFlags -> LlvmEnv -> Int -> [CmmStatic] -> (Doc, [LlvmVar])
pprCmmStatic dflags env count stat
  = let unres = genLlvmData dflags (Data,stat)
        (_, (ldata, ltypes)) = resolveLlvmData dflags env unres

        setSection (gv@(LMGlobalVar s ty l _ _), d)
            = let v = if l == Internal then [gv] else []
                  sec = mkLayoutSection count
              in ((LMGlobalVar s ty l sec llvmInfAlign, d), v)
        setSection v = (v,[])

        (ldata', llvmUsed) = mapAndUnzip setSection ldata
    in (pprLlvmData dflags (ldata', ltypes), concat llvmUsed)


-- | Create an appropriate section declaration for subsection <n> of text
-- WARNING: This technique could fail as gas documentation says it only
-- supports up to 8192 subsections per section. Inspection of the source
-- code and some test programs seem to suggest it supports more than this
-- so we are hoping it does.
mkLayoutSection :: Int -> LMSection
mkLayoutSection n
  = Just (fsLit $ ".text;.text " ++ show n ++ " #")

