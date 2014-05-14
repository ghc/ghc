{-# LANGUAGE CPP, TypeFamilies #-}

-- -----------------------------------------------------------------------------
-- | This is the top-level module in the LLVM code generator.
--
module LlvmCodeGen ( llvmCodeGen, llvmFixupAsm ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base
import LlvmCodeGen.CodeGen
import LlvmCodeGen.Data
import LlvmCodeGen.Ppr
import LlvmCodeGen.Regs
import LlvmMangler

import CgUtils ( fixStgRegisters )
import Cmm
import Hoopl
import PprCmm

import BufWrite
import DynFlags
import ErrUtils
import FastString
import Outputable
import UniqSupply
import SysTools ( figureLlvmVersion )
import qualified Stream

import Control.Monad ( when )
import Data.IORef ( writeIORef )
import Data.Maybe ( fromMaybe, catMaybes )
import System.IO

-- -----------------------------------------------------------------------------
-- | Top-level of the LLVM Code generator
--
llvmCodeGen :: DynFlags -> Handle -> UniqSupply
               -> Stream.Stream IO RawCmmGroup ()
               -> IO ()
llvmCodeGen dflags h us cmm_stream
  = do bufh <- newBufHandle h

       -- Pass header
       showPass dflags "LLVM CodeGen"

       -- get llvm version, cache for later use
       ver <- (fromMaybe defaultLlvmVersion) `fmap` figureLlvmVersion dflags
       writeIORef (llvmVersion dflags) ver

       -- warn if unsupported
       debugTraceMsg dflags 2
            (text "Using LLVM version:" <+> text (show ver))
       let doWarn = wopt Opt_WarnUnsupportedLlvmVersion dflags
       when (ver < minSupportLlvmVersion && doWarn) $
           errorMsg dflags (text "You are using an old version of LLVM that"
                            <> text " isn't supported anymore!"
                            $+$ text "We will try though...")
       when (ver > maxSupportLlvmVersion && doWarn) $
           putMsg dflags (text "You are using a new version of LLVM that"
                          <> text " hasn't been tested yet!"
                          $+$ text "We will try though...")

       -- run code generation
       runLlvm dflags ver bufh us $
         llvmCodeGen' (liftStream cmm_stream)

       bFlush bufh

llvmCodeGen' :: Stream.Stream LlvmM RawCmmGroup () -> LlvmM ()
llvmCodeGen' cmm_stream
  = do  -- Preamble
        renderLlvm pprLlvmHeader
        ghcInternalFunctions
        cmmMetaLlvmPrelude

        -- Procedures
        let llvmStream = Stream.mapM llvmGroupLlvmGens cmm_stream
        _ <- Stream.collect llvmStream

        -- Declare aliases for forward references
        renderLlvm . pprLlvmData =<< generateAliases

        -- Postamble
        cmmUsedLlvmGens

llvmGroupLlvmGens :: RawCmmGroup -> LlvmM ()
llvmGroupLlvmGens cmm = do

        -- Insert functions into map, collect data
        let split (CmmData s d' )     = return $ Just (s, d')
            split (CmmProc h l live g) = do
              -- Set function type
              let l' = case mapLookup (g_entry g) h of
                         Nothing                   -> l
                         Just (Statics info_lbl _) -> info_lbl
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
cmmDataLlvmGens :: [(Section,CmmStatics)] -> LlvmM ()

cmmDataLlvmGens statics
  = do lmdatas <- mapM genLlvmData statics

       let (gss, tss) = unzip lmdatas

       let regGlobal (LMGlobal (LMGlobalVar l ty _ _ _ _) _)
                        = funInsert l ty
           regGlobal _  = return ()
       mapM_ regGlobal (concat gss)

       renderLlvm $ pprLlvmData (concat gss, concat tss)

-- | Complete LLVM code generation phase for a single top-level chunk of Cmm.
cmmLlvmGen ::RawCmmDecl -> LlvmM ()
cmmLlvmGen cmm@CmmProc{} = do

    -- rewrite assignments to global regs
    dflags <- getDynFlag id
    let fixed_cmm = {-# SCC "llvm_fix_regs" #-}
                    fixStgRegisters dflags cmm

    dumpIfSetLlvm Opt_D_dump_opt_cmm "Optimised Cmm" (pprCmmGroup [fixed_cmm])

    -- generate llvm code from cmm
    llvmBC <- withClearVars $ genLlvmProc fixed_cmm

    -- allocate IDs for info table and code, so the mangler can later
    -- make sure they end up next to each other.
    itableSection <- freshSectionId
    _codeSection <- freshSectionId

    -- pretty print
    (docs, ivars) <- fmap unzip $ mapM (pprLlvmCmmDecl itableSection) llvmBC

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
    return $ MetaUnamed tbaaId $ MetaStruct
        [ MetaStr name
        , case parentId of
          Just p  -> MetaNode p
          Nothing -> MetaVar $ LMLitVar $ LMNullLit i8Ptr
        ]
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
