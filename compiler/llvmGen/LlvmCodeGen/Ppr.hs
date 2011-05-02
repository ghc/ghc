-- ----------------------------------------------------------------------------
-- | Pretty print helpers for the LLVM Code generator.
--

module LlvmCodeGen.Ppr (
        pprLlvmHeader, pprLlvmCmmTop, pprLlvmData, infoSection, iTableSuf
    ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base
import LlvmCodeGen.Data

import CLabel
import OldCmm

import FastString
import qualified Outputable
import Pretty
import Unique


-- ----------------------------------------------------------------------------
-- * Top level
--

-- | LLVM module layout description for the host target
moduleLayout :: Doc
moduleLayout =
#if i386_TARGET_ARCH

#if darwin_TARGET_OS
    text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:128:128-n8:16:32\""
    $+$ text "target triple = \"i386-apple-darwin9.8\""
#elif mingw32_TARGET_OS
    text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-f80:128:128-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32\""
    $+$ text "target triple = \"i686-pc-win32\""
#else /* Linux */
    text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32\""
    $+$ text "target triple = \"i386-pc-linux-gnu\""
#endif

#elif x86_64_TARGET_ARCH

#if darwin_TARGET_OS
    text "target datalayout = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64\""
    $+$ text "target triple = \"x86_64-apple-darwin10.0.0\""
#else /* Linux */
    text "target datalayout = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64\""
    $+$ text "target triple = \"x86_64-linux-gnu\""
#endif

#else /* Not x86 */
    -- FIX: Other targets
    empty
#endif


-- | Header code for LLVM modules
pprLlvmHeader :: Doc
pprLlvmHeader = moduleLayout


-- | Pretty print LLVM data code
pprLlvmData :: LlvmData -> Doc
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
pprLlvmCmmTop :: LlvmEnv -> Int -> LlvmCmmTop -> (Doc, [LlvmVar])
pprLlvmCmmTop _ _ (CmmData _ lmdata)
  = (vcat $ map pprLlvmData lmdata, [])

pprLlvmCmmTop env count (CmmProc info lbl (ListGraph blks))
  = let static = CmmDataLabel lbl : info
        (idoc, ivar) = if not (null info)
                          then pprInfoTable env count lbl static
                          else (empty, [])
    in (idoc $+$ (
        let sec = mkLayoutSection (count + 1)
            (lbl',sec') = if not (null info)
                            then (entryLblToInfoLbl lbl, sec)
                            else (lbl, Nothing)
            link = if externallyVisibleCLabel lbl'
                      then ExternallyVisible
                      else Internal
            lmblocks = map (\(BasicBlock id stmts) ->
                                LlvmBlock (getUnique id) stmts) blks
            fun = mkLlvmFunc lbl' link  sec' lmblocks
        in ppLlvmFunction fun
    ), ivar)


-- | Pretty print CmmStatic
pprInfoTable :: LlvmEnv -> Int -> CLabel -> [CmmStatic] -> (Doc, [LlvmVar])
pprInfoTable env count lbl stat
  = let unres = genLlvmData (Text, stat)
        (_, (ldata, ltypes)) = resolveLlvmData env unres

        setSection ((LMGlobalVar _ ty l _ _ c), d)
            = let sec = mkLayoutSection count
                  ilabel = strCLabel_llvm (entryLblToInfoLbl lbl)
                              `appendFS` fsLit iTableSuf
                  gv = LMGlobalVar ilabel ty l sec llvmInfAlign c
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

