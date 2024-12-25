{-# LANGUAGE TypeFamilies, ViewPatterns, OverloadedStrings #-}

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

import GHC.Prelude

import GHC.Llvm
import GHC.CmmToLlvm.Base
import GHC.CmmToLlvm.CodeGen
import GHC.CmmToLlvm.Config
import GHC.CmmToLlvm.Data
import GHC.CmmToLlvm.Ppr
import GHC.CmmToLlvm.Regs
import GHC.CmmToLlvm.Mangler
import GHC.CmmToLlvm.Version

import GHC.StgToCmm.CgUtils ( fixStgRegisters )
import GHC.Cmm
import GHC.Cmm.Dataflow.Label

import GHC.Utils.BufHandle
import GHC.Driver.DynFlags
import GHC.Platform ( platformArch, Arch(..) )
import GHC.Utils.Error
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Logger
import qualified GHC.Data.Stream as Stream

import Control.Monad ( when, forM_ )
import Data.Maybe ( fromMaybe, catMaybes, isNothing )
import System.IO

-- -----------------------------------------------------------------------------
-- | Top-level of the LLVM Code generator
--
llvmCodeGen :: Logger -> LlvmCgConfig -> Handle
               -> Stream.Stream IO RawCmmGroup a
               -> IO a
llvmCodeGen logger cfg h cmm_stream
  = withTiming logger (text "LLVM CodeGen") (const ()) $ do
       bufh <- newBufHandle h

       -- Pass header
       showPass logger "LLVM CodeGen"

       -- get llvm version, cache for later use
       let mb_ver = llvmCgLlvmVersion cfg

       -- warn if unsupported
       forM_ mb_ver $ \ver -> do
         debugTraceMsg logger 2
              (text "Using LLVM version:" <+> text (llvmVersionStr ver))
         let doWarn = llvmCgDoWarn cfg
         when (not (llvmVersionSupported ver) && doWarn) $ putMsg logger $
           "You are using an unsupported version of LLVM!" $$
           "Currently only" <+> text (llvmVersionStr supportedLlvmVersionLowerBound) <+>
           "up to" <+> text (llvmVersionStr supportedLlvmVersionUpperBound) <+> "(non inclusive) is supported." <+>
           "System LLVM version: " <> text (llvmVersionStr ver) $$
           "We will try though..."

       when (isNothing mb_ver) $ do
         let doWarn = llvmCgDoWarn cfg
         when doWarn $ putMsg logger $
           "Failed to detect LLVM version!" $$
           "Make sure LLVM is installed correctly." $$
           "We will try though..."

       -- HACK: the Nothing case here is potentially wrong here but we
       -- currently don't use the LLVM version to guide code generation
       -- so this is okay.
       let llvm_ver :: LlvmVersion
           llvm_ver = fromMaybe supportedLlvmVersionLowerBound mb_ver

       -- run code generation
       a <- runLlvm logger cfg llvm_ver bufh $
         llvmCodeGen' cfg cmm_stream

       bFlush bufh

       return a

llvmCodeGen' :: LlvmCgConfig -> Stream.Stream IO RawCmmGroup a -> LlvmM a
llvmCodeGen' cfg cmm_stream
  = do  -- Preamble
        renderLlvm (llvmHeader cfg) (llvmHeader cfg)
        ghcInternalFunctions
        cmmMetaLlvmPrelude

        -- Procedures
        a <- Stream.consume cmm_stream liftIO llvmGroupLlvmGens

        -- Declare aliases for forward references
        decls <- generateExternDecls
        renderLlvm (pprLlvmData cfg decls)
                   (pprLlvmData cfg decls)

        -- Postamble
        cmmUsedLlvmGens

        return a

llvmHeader :: IsDoc doc => LlvmCgConfig -> doc
llvmHeader cfg =
  let target  = llvmCgLlvmTarget cfg
      llvmCfg = llvmCgLlvmConfig cfg
  in lines_
      [ text "target datalayout = \"" <> text (getDataLayout llvmCfg target) <> text "\""
      , text "target triple = \"" <> text target <> text "\"" ]
  where
    getDataLayout :: LlvmConfig -> String -> String
    getDataLayout config target =
      case lookup target (llvmTargets config) of
        Just (LlvmTarget {lDataLayout=dl}) -> dl
        Nothing -> pprPanic "Failed to lookup LLVM data layout" $
                   text "Target:" <+> text target $$
                   hang (text "Available targets:") 4
                        (vcat $ map (text . fst) $ llvmTargets config)
{-# SPECIALIZE llvmHeader :: LlvmCgConfig -> SDoc #-}
{-# SPECIALIZE llvmHeader :: LlvmCgConfig -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

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
       gss' <- mapM aliasify gs

       cfg <- getConfig
       renderLlvm (pprLlvmData cfg (concat gss', concat tss))
                  (pprLlvmData cfg (concat gss', concat tss))

-- | Complete LLVM code generation phase for a single top-level chunk of Cmm.
cmmLlvmGen ::RawCmmDecl -> LlvmM ()
cmmLlvmGen cmm@CmmProc{} = do

    -- rewrite assignments to global regs
    platform <- getPlatform
    let fixed_cmm = {-# SCC "llvm_fix_regs" #-} fixStgRegisters platform cmm

    dumpIfSetLlvm Opt_D_dump_opt_cmm "Optimised Cmm"
      FormatCMM (pprCmmGroup platform [fixed_cmm])

    -- generate llvm code from cmm
    llvmBC <- withClearVars $ genLlvmProc fixed_cmm

    -- pretty print - print as we go, since we produce HDocs, we know
    -- no nesting state needs to be maintained for the SDocs.
    forM_ llvmBC (\decl -> do
        (hdoc, sdoc) <- pprLlvmCmmDecl decl
        renderLlvm (hdoc $$ empty) (sdoc $$ empty)
      )

cmmLlvmGen _ = return ()

-- -----------------------------------------------------------------------------
-- | Generate meta data nodes
--

cmmMetaLlvmPrelude :: LlvmM ()
cmmMetaLlvmPrelude = do
  tbaa_metas <- flip mapM stgTBAA $ \(uniq, name, parent) -> do
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

  platform <- getPlatform
  cfg <- getConfig
  let stack_alignment_metas =
          case platformArch platform of
            ArchX86_64 | llvmCgAvxEnabled cfg -> [mkStackAlignmentMeta 32]
            _                                 -> []
  module_flags_metas <- mkModuleFlagsMeta stack_alignment_metas
  let metas = tbaa_metas ++ module_flags_metas
  cfg <- getConfig
  renderLlvm (ppLlvmMetas cfg metas)
             (ppLlvmMetas cfg metas)

mkNamedMeta :: LMString -> [MetaExpr] -> LlvmM [MetaDecl]
mkNamedMeta name exprs = do
    (ids, decls) <- unzip <$> mapM f exprs
    return $ decls ++ [MetaNamed name ids]
  where
    f expr = do
      i <- getMetaUniqueId
      return (i, MetaUnnamed i expr)

mkModuleFlagsMeta :: [ModuleFlag] -> LlvmM [MetaDecl]
mkModuleFlagsMeta =
    mkNamedMeta "llvm.module.flags" . map moduleFlagToMetaExpr

mkStackAlignmentMeta :: Integer -> ModuleFlag
mkStackAlignmentMeta alignment =
    ModuleFlag MFBError "override-stack-alignment" (MetaLit $ LMIntLit alignment i32)


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
      ty     = LMArray (length ivars) i8Ptr
      usedArray = LMStaticArray (map cast ivars) ty
      sectName  = Just $ fsLit "llvm.metadata"
      lmUsedVar = LMGlobalVar (fsLit "llvm.used") ty Appending sectName Nothing Constant
      lmUsed    = LMGlobal lmUsedVar (Just usedArray)
  if null ivars
     then return ()
     else do
      cfg <- getConfig
      renderLlvm (pprLlvmData cfg ([lmUsed], []))
                 (pprLlvmData cfg ([lmUsed], []))
