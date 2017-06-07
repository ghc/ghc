{-# LANGUAGE CPP #-}

-- ----------------------------------------------------------------------------
-- | Pretty print helpers for the LLVM Code generator.
--
module LlvmCodeGen.Ppr (
        pprLlvmHeader, pprLlvmCmmDecl, pprLlvmData, infoSection
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

import DynFlags (targetPlatform)

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
        $+$ text "target triple = \"armv6-unknown-linux-gnueabihf\""
    Platform { platformArch = ArchARM {}, platformOS = OSAndroid } ->
        text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32\""
        $+$ text "target triple = \"arm-unknown-linux-androideabi\""
    Platform { platformArch = ArchARM {}, platformOS = OSQNXNTO } ->
        text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32\""
        $+$ text "target triple = \"arm-unknown-nto-qnx8.0.0eabi\""
    Platform { platformArch = ArchARM {}, platformOS = OSiOS } ->
        text "target datalayout = \"e-m:o-p:32:32-f64:32:64-v64:32:64-v128:32:128-a:0:32-n32-S32\""
        $+$ text "target triple = \"thumbv7-apple-ios7.0.0\""
    Platform { platformArch = ArchARM64, platformOS = OSiOS } ->
        text "target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128\""
        $+$ text "target triple = \"arm64-apple-ios7.0.0\""
    Platform { platformArch = ArchX86, platformOS = OSiOS } ->
        text "target datalayout = \"e-m:o-p:32:32-f64:32:64-f80:128-n8:16:32-S128\""
        $+$ text "target triple = \"i386-apple-ios7.0.0\""
    Platform { platformArch = ArchX86_64, platformOS = OSiOS } ->
        text "target datalayout = \"e-m:o-i64:64-f80:128-n8:16:32:64-S128\""
        $+$ text "target triple = \"x86_64-apple-ios7.0.0\""
    Platform { platformArch = ArchARM64, platformOS = OSLinux } ->
        text "target datalayout = \"e-m:e-i64:64-i128:128-n32:64-S128\""
        $+$ text "target triple = \"aarch64-unknown-linux-gnu\""
    _ ->
        if platformIsCrossCompiling platform
            then panic "LlvmCodeGen.Ppr: Cross compiling without valid target info."
            else empty
        -- If you see the above panic, GHC is missing the required target datalayout
        -- and triple information. You can obtain this info by compiling a simple
        -- 'hello world' C program with the clang C compiler eg:
        --     clang -S hello.c -emit-llvm -o -
        -- and the first two lines of hello.ll should provide the 'target datalayout'
        -- and 'target triple' lines required.


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
pprLlvmCmmDecl :: LlvmCmmDecl -> LlvmM (SDoc, [LlvmVar])
pprLlvmCmmDecl (CmmData _ lmdata)
  = return (vcat $ map pprLlvmData lmdata, [])

pprLlvmCmmDecl (CmmProc mb_info entry_lbl live (ListGraph blks))
  = do let lbl = case mb_info of
                     Nothing                   -> entry_lbl
                     Just (Statics info_lbl _) -> info_lbl
           link = if externallyVisibleCLabel lbl
                      then ExternallyVisible
                      else Internal
           lmblocks = map (\(BasicBlock id stmts) ->
                                LlvmBlock (getUnique id) stmts) blks

       funDec <- llvmFunSig live lbl link
       dflags <- getDynFlags
       let buildArg = fsLit . showSDoc dflags . ppPlainName
           funArgs = map buildArg (llvmFunArgs dflags live)
           funSect = llvmFunSection dflags (decName funDec)

       -- generate the info table
       prefix <- case mb_info of
                     Nothing -> return Nothing
                     Just (Statics _ statics) -> do
                       infoStatics <- mapM genData statics
                       let infoTy = LMStruct $ map getStatType infoStatics
                       return $ Just $ LMStaticStruc infoStatics infoTy


       let fun = LlvmFunction funDec funArgs llvmStdFunDefAttrs funSect
                              prefix lmblocks
           name = decName $ funcDecl fun
           defName = name `appendFS` fsLit "$def"
           funcDecl' = (funcDecl fun) { decName = defName }
           fun' = fun { funcDecl = funcDecl' }
           funTy = LMFunction funcDecl'
           funVar = LMGlobalVar name
                                (LMPointer funTy)
                                link
                                Nothing
                                Nothing
                                Alias
           defVar = LMGlobalVar defName
                                (LMPointer funTy)
                                (funcLinkage funcDecl')
                                (funcSect fun)
                                (funcAlign funcDecl')
                                Alias
           alias = LMGlobal funVar
                            (Just $ LMBitc (LMStaticPointer defVar)
                                           (LMPointer $ LMInt 8))
       -- our beloved dead_strip preventer.
       -- the idea here is to inject
       --
       -- module asm "_symbol$dsp = _symbol-24" -- assuming prefix
       --                                          of <{i64, i64, i64}>
       -- module asm ".no_dead_strip _symbol$dsp"
       --
       -- and thereby generating a second symbol
       -- at the start of the info table, which is dead strip prevented.
       --
       -- ideally, llvm should generate these for us, but as
       -- things stand, this is the least hacky solution to
       -- prevent dead_stripping of the prefix data, while
       -- retaining dead stripping in general.
       --
       -- The general layout of the above code results in the following:
       --
       --   .------------. <- @<name>$def$dsp
       --   | Info Table |
       --   |------------| <- @<name>, @<name>$def
       --   | Fn Body    |
       --   '------------'
       --
       -- Why this @<name> and @<name>$def?  As the textual llvm ir
       -- generator is only handed typeless labes, it often does not
       -- know the type of the label (e.g. function to call), until
       -- the actual call happens.  However, llvm requires symbol
       -- lookups to be typed.  Therfore we create the actual function
       -- as @<name>$def, and alias a bitcast to i8* as @<name>.
       --   Any subsequent lookup can lookup @<name> as i8* and
       -- bitcast it to the required type once we want to call it.
       --
       -- Why .no_dead_strip? Doesn't this prevent the linker from
       -- -dead_strip'ing anything? Yes, it does. And we'll have to
       -- live with this wart until a better solution is found that
       -- ensures that all symbols that are used directly or
       -- indirectly are marked used.
       --
       -- This is all rather annoying. ghc 8.2 uses the infamous
       -- Mangler to drop the .subsections_via_symbols directive
       -- from the assembly.  LLVM ingeniously emits said directive
       -- unconditionally for mach-o files.  To lift the need for
       -- extra mangler step, we explicitly mark every symbol
       -- .no_dead_strip.
       --
       -- We are making a few assumptions here:
       -- - the symbols end up being name _<symbol> in the final
       --   assembly file.
       --
       dsp <- case mb_info of
         Nothing -> pure empty
         Just (Statics _ statics)
           | platformHasSubsectionsViaSymbols (targetPlatform dflags) -> do
               infoStatics <- mapM genData statics
               -- remember, the prefix_size is in bits!
               let prefix_size = sum (map (llvmWidthInBits dflags . getStatType)
                                          infoStatics)
                   dspName = defName `appendFS` fsLit "$dsp"
                   defSymbol = text "_" <> ftext defName
                   dspSymbol = text "_" <> ftext dspName
                   moduleAsm s = text "module asm" <+> doubleQuotes s
               return $ text "; insert dead_strip preventer"
                      $+$ moduleAsm (dspSymbol <+> text "=" <+> defSymbol
                                     <> text "-" <> int (prefix_size `div` 8))
                      $+$ moduleAsm (text ".no_dead_strip" <+> dspSymbol)
                      $+$ text "; end dead_strip preventer"
           | otherwise -> pure empty

       return (ppLlvmGlobal alias $+$ ppLlvmFunction fun' $+$ dsp, [])


-- | The section we are putting info tables and their entry code into, should
-- be unique since we process the assembly pattern matching this.
infoSection :: String
infoSection = "X98A__STRIP,__me"
