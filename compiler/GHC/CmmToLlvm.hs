{-# LANGUAGE CPP, TypeFamilies, ViewPatterns, OverloadedStrings #-}

-- -----------------------------------------------------------------------------
-- | This is the top-level module in the LLVM code generator.
--
module GHC.CmmToLlvm
   ( LlvmVersion
   , llvmVersionList
   , llvmCodeGen
   , llvmFixupAsm
   )
where

#include "HsVersions.h"

import GhcPrelude

import GHC.Llvm
import GHC.CmmToLlvm.Base
import GHC.CmmToLlvm.CodeGen
import GHC.CmmToLlvm.Data
import GHC.CmmToLlvm.Ppr
import GHC.CmmToLlvm.Regs
import GHC.CmmToLlvm.Mangler

import GHC.StgToCmm.CgUtils ( fixStgRegisters )
import GHC.Cmm
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Ppr

import BufWrite
import GHC.Driver.Session
import GHC.Platform ( platformArch, Arch(..) )
import ErrUtils
import FastString
import Outputable
import SysTools ( figureLlvmVersion )
import qualified Stream

import Control.Monad ( when, forM_ )
import Data.Maybe ( fromMaybe, catMaybes )
import System.IO

-- -----------------------------------------------------------------------------
-- | Top-level of the LLVM Code generator
--
llvmCodeGen :: DynFlags -> Handle
               -> Stream.Stream IO RawCmmGroup a
               -> IO a
llvmCodeGen dflags h cmm_stream
  = withTiming dflags (text "LLVM CodeGen") (const ()) $ do
       bufh <- newBufHandle h

       -- Pass header
       showPass dflags "LLVM CodeGen"

       -- get llvm version, cache for later use
       mb_ver <- figureLlvmVersion dflags

       -- warn if unsupported
       forM_ mb_ver $ \ver -> do
         debugTraceMsg dflags 2
              (text "Using LLVM version:" <+> text (llvmVersionStr ver))
         let doWarn = wopt Opt_WarnUnsupportedLlvmVersion dflags
         when (not (llvmVersionSupported ver) && doWarn) $ putMsg dflags $
           "You are using an unsupported version of LLVM!" $$
           "Currently only " <> text (llvmVersionStr supportedLlvmVersion) <> " is supported." <+>
           "System LLVM version: " <> text (llvmVersionStr ver) $$
           "We will try though..."
         let isS390X = platformArch (targetPlatform dflags) == ArchS390X
         let major_ver = head . llvmVersionList $ ver
         when (isS390X && major_ver < 10 && doWarn) $ putMsg dflags $
           "Warning: For s390x the GHC calling convention is only supported since LLVM version 10." <+>
           "You are using LLVM version: " <> text (llvmVersionStr ver)

       -- run code generation
       a <- runLlvm dflags (fromMaybe supportedLlvmVersion mb_ver) bufh $
         llvmCodeGen' (liftStream cmm_stream)

       bFlush bufh

       return a

llvmCodeGen' :: Stream.Stream LlvmM RawCmmGroup a -> LlvmM a
llvmCodeGen' cmm_stream
  = do  -- Preamble
        renderLlvm header
        ghcInternalFunctions
        cmmMetaLlvmPrelude

        -- Procedures
        a <- Stream.consume cmm_stream llvmGroupLlvmGens

        -- Declare aliases for forward references
        renderLlvm . pprLlvmData =<< generateExternDecls

        -- Postamble
        cmmUsedLlvmGens

        return a
  where
    header :: SDoc
    header = sdocWithDynFlags $ \dflags ->
      let target = platformMisc_llvmTarget $ platformMisc dflags
      in     text ("target datalayout = \"" ++ getDataLayout dflags target ++ "\"")
         $+$ text ("target triple = \"" ++ target ++ "\"")

    getDataLayout :: DynFlags -> String -> String
    getDataLayout dflags target =
      case lookup target (llvmTargets $ llvmConfig dflags) of
        Just (LlvmTarget {lDataLayout=dl}) -> dl
        Nothing -> pprPanic "Failed to lookup LLVM data layout" $
                   text "Target:" <+> text target $$
                   hang (text "Available targets:") 4
                        (vcat $ map (text . fst) $ llvmTargets $ llvmConfig dflags)

llvmGroupLlvmGens :: RawCmmGroup -> LlvmM ()
llvmGroupLlvmGens cmm = do

        -- Insert functions into map, collect data
        let split (CmmData s d' )     = return $ Just (s, d')
            split (CmmProc h l live g) = do
              -- Set function type
              let l' = case mapLookup (g_entry g) h :: Maybe RawCmmStatics of
                         Nothing                   -> l
                         Just (CmmStaticsRaw info_lbl _) -> info_lbl
              lml <- strCLabel_llvm l'
              funInsert lml =<< llvmFunTy live
              return Nothing
        cdata <- fmap catMaybes $ mapM split cmm

        {-# SCC "llvm_datas_gen" #-}
          cmmDataLlvmGens cdata
        {-# SCC "llvm_procs_gen" #-}
          mapM_ cmmLlvmGen cmm

-- -----------------------------------------------------------------------------
-- | Do LLVM code generation on all these Cmms data sections.
--
cmmDataLlvmGens :: [(Section,RawCmmStatics)] -> LlvmM ()

cmmDataLlvmGens statics
  = do lmdatas <- mapM genLlvmData statics

       let (concat -> gs, tss) = unzip lmdatas

       let regGlobal (LMGlobal (LMGlobalVar l ty _ _ _ _) _)
                        = funInsert l ty
           regGlobal _  = pure ()
       mapM_ regGlobal gs
       gss' <- mapM aliasify $ gs

       renderLlvm $ pprLlvmData (concat gss', concat tss)

-- | Complete LLVM code generation phase for a single top-level chunk of Cmm.
cmmLlvmGen ::RawCmmDecl -> LlvmM ()
cmmLlvmGen cmm@CmmProc{} = do

    -- rewrite assignments to global regs
    dflags <- getDynFlag id
    let fixed_cmm = {-# SCC "llvm_fix_regs" #-} fixStgRegisters dflags cmm

    dumpIfSetLlvm Opt_D_dump_opt_cmm "Optimised Cmm"
      FormatCMM (pprCmmGroup [fixed_cmm])

    -- generate llvm code from cmm
    llvmBC <- withClearVars $ genLlvmProc fixed_cmm

    -- pretty print
    (docs, ivars) <- fmap unzip $ mapM pprLlvmCmmDecl llvmBC

    -- Output, note down used variables
    renderLlvm (vcat docs)
    mapM_ markUsedVar $ concat ivars

cmmLlvmGen _ = return ()

-- -----------------------------------------------------------------------------
-- | Generate meta data nodes
--

cmmMetaLlvmPrelude :: LlvmM ()
cmmMetaLlvmPrelude = do
  metas <- flip mapM stgTBAA $ \(uniq, name, parent) -> do
    -- Generate / lookup meta data IDs
    tbaaId <- getMetaUniqueId
    setUniqMeta uniq tbaaId
    parentId <- maybe (return Nothing) getUniqMeta parent
    -- Build definition
    return $ MetaUnnamed tbaaId $ MetaStruct $
          case parentId of
              Just p  -> [ MetaStr name, MetaNode p ]
              -- As of LLVM 4.0, a node without parents should be rendered as
              -- just a name on its own. Previously `null` was accepted as the
              -- name.
              Nothing -> [ MetaStr name ]
  renderLlvm $ ppLlvmMetas metas

-- -----------------------------------------------------------------------------
-- | Marks variables as used where necessary
--

cmmUsedLlvmGens :: LlvmM ()
cmmUsedLlvmGens = do

  -- LLVM would discard variables that are internal and not obviously
  -- used if we didn't provide these hints. This will generate a
  -- definition of the form
  --
  --   @llvm.used = appending global [42 x i8*] [i8* bitcast <var> to i8*, ...]
  --
  -- Which is the LLVM way of protecting them against getting removed.
  ivars <- getUsedVars
  let cast x = LMBitc (LMStaticPointer (pVarLift x)) i8Ptr
      ty     = (LMArray (length ivars) i8Ptr)
      usedArray = LMStaticArray (map cast ivars) ty
      sectName  = Just $ fsLit "llvm.metadata"
      lmUsedVar = LMGlobalVar (fsLit "llvm.used") ty Appending sectName Nothing Constant
      lmUsed    = LMGlobal lmUsedVar (Just usedArray)
  if null ivars
     then return ()
     else renderLlvm $ pprLlvmData ([lmUsed], [])
