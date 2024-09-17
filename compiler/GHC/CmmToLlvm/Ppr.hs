

-- ----------------------------------------------------------------------------
-- | Pretty print helpers for the LLVM Code generator.
--
module GHC.CmmToLlvm.Ppr (
        pprLlvmCmmDecl, pprLlvmData, infoSection
    ) where

import GHC.Prelude

import GHC.Llvm
import GHC.CmmToLlvm.Base
import GHC.CmmToLlvm.Data
import GHC.CmmToLlvm.Config

import GHC.Cmm.CLabel
import GHC.Cmm

import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Types.Unique

-- ----------------------------------------------------------------------------
-- * Top level
--

-- | Pretty print LLVM data code
pprLlvmData :: IsDoc doc => LlvmCgConfig -> LlvmData -> doc
pprLlvmData cfg (globals, types) =
    let ppLlvmTys (LMAlias    a) = line $ ppLlvmAlias a
        ppLlvmTys (LMFunction f) = ppLlvmFunctionDecl f
        ppLlvmTys _other         = empty

        types'   = vcat $ map ppLlvmTys types
        globals' = ppLlvmGlobals cfg globals
    in types' $$ globals'
{-# SPECIALIZE pprLlvmData :: LlvmCgConfig -> LlvmData -> SDoc #-}
{-# SPECIALIZE pprLlvmData :: LlvmCgConfig -> LlvmData -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


-- | Pretty print LLVM code
-- The HDoc we return is used to produce the final LLVM file, with the
-- SDoc being returned alongside for use when @Opt_D_dump_llvm@ is set
-- as we can't (currently) dump HDocs.
pprLlvmCmmDecl :: LlvmCmmDecl -> LlvmM (HDoc, SDoc)
pprLlvmCmmDecl (CmmData _ lmdata) = do
  opts <- getConfig
  return ( vcat $ map (pprLlvmData opts) lmdata
         , vcat $ map (pprLlvmData opts) lmdata)

pprLlvmCmmDecl (CmmProc mb_info entry_lbl liveWithUses (ListGraph blks))
  = do let live = map globalRegUse_reg liveWithUses
           lbl = case mb_info of
                     Nothing -> entry_lbl
                     Just (CmmStaticsRaw info_lbl _) -> info_lbl
           link = if externallyVisibleCLabel lbl
                      then ExternallyVisible
                      else Internal
           lmblocks = map (\(BasicBlock id stmts) ->
                                LlvmBlock (getUnique id) stmts) blks

       funDec   <- llvmFunSig live lbl link
       cfg      <- getConfig
       platform <- getPlatform
       let buildArg = fsLit . showSDocOneLine (llvmCgContext cfg). ppPlainName cfg
           funArgs = map buildArg (llvmFunArgs platform live)
           funSect = llvmFunSection cfg (decName funDec)

       -- generate the info table
       prefix <- case mb_info of
                     Nothing -> return Nothing
                     Just (CmmStaticsRaw _ statics) -> do
                       infoStatics <- mapM genData statics
                       let infoTy = LMStruct $ map getStatType infoStatics
                       return $ Just $ LMStaticStruc infoStatics infoTy


       let fun = LlvmFunction funDec funArgs llvmStdFunAttrs funSect
                              prefix lmblocks
           name = decName $ funcDecl fun
           defName = llvmDefLabel name
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
                                           i8Ptr)

       return ( vcat [line $ ppLlvmGlobal cfg alias, ppLlvmFunction cfg fun']
              , vcat [line $ ppLlvmGlobal cfg alias, ppLlvmFunction cfg fun'])


-- | The section we are putting info tables and their entry code into, should
-- be unique since we process the assembly pattern matching this.
infoSection :: String
infoSection = "X98A__STRIP,__me"
