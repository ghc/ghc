{-# LANGUAGE CPP #-}

-- ----------------------------------------------------------------------------
-- | Pretty print helpers for the LLVM Code generator.
--
module LlvmCodeGen.Ppr (
        pprLlvmCmmDecl, pprLlvmData, infoSection
    ) where

#include "HsVersions.h"

import GhcPrelude

import Llvm
import LlvmCodeGen.Base
import LlvmCodeGen.Data

import CLabel
import Cmm

import FastString
import Outputable
import Unique

-- ----------------------------------------------------------------------------
-- * Top level
--

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


       let fun = LlvmFunction funDec funArgs llvmStdFunAttrs funSect
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

       return (ppLlvmGlobal alias $+$ ppLlvmFunction fun', [])


-- | The section we are putting info tables and their entry code into, should
-- be unique since we process the assembly pattern matching this.
infoSection :: String
infoSection = "X98A__STRIP,__me"
