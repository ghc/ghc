{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

-- | JavaScript code generator
module GHC.StgToJS.CodeGen
  ( stgToJS
  )
where

import GHC.Prelude

import GHC.Driver.Flags (DumpFlag (Opt_D_dump_js, Opt_D_dump_stg_from_js_sinker))

import GHC.JS.Ppr
import GHC.JS.JStg.Syntax
import GHC.JS.Ident
import GHC.JS.Make
import GHC.JS.Transform
import GHC.JS.Optimizer

import GHC.StgToJS.Arg
import GHC.StgToJS.Sinker.Sinker
import GHC.StgToJS.Types
import qualified GHC.StgToJS.Object as Object
import GHC.StgToJS.Utils
import GHC.StgToJS.Deps
import GHC.StgToJS.Expr
import GHC.StgToJS.ExprCtx
import GHC.StgToJS.Monad
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.StaticPtr
import GHC.StgToJS.Symbols
import GHC.StgToJS.Stack
import GHC.StgToJS.Ids

import GHC.Stg.Syntax
import GHC.Core.DataCon
import GHC.Core.TyCo.Rep (scaledThing)

import GHC.Unit.Module
import GHC.Linker.Types (SptEntry (..))

import GHC.Types.CostCentre
import GHC.Types.ForeignStubs (ForeignStubs (..), getCHeader, getCStub)
import GHC.Types.RepType
import GHC.Types.Id
import GHC.Types.Unique
import GHC.Types.Unique.FM (nonDetEltsUFM)

import GHC.Data.FastString
import GHC.Utils.Encoding
import GHC.Utils.Logger
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Binary
import qualified Control.Monad.Trans.State.Strict as State
import GHC.Utils.Outputable hiding ((<>))

import qualified Data.Set as S
import Data.Monoid
import Data.List (sortBy)
import Control.Monad
import System.Directory
import System.FilePath

-- | Code generator for JavaScript
stgToJS
  :: Logger
  -> StgToJSConfig
  -> [CgStgTopBinding]
  -> Module
  -> [SptEntry]
  -> ForeignStubs
  -> CollectedCCs
  -> FilePath -- ^ Output file name
  -> IO ()
stgToJS logger config stg_binds0 this_mod spt_entries foreign_stubs cccs output_fn = do

  let (unfloated_binds, stg_binds) = sinkPgm this_mod stg_binds0
    -- TODO: avoid top level lifting in core-2-core when the JS backend is
    -- enabled instead of undoing it here

  putDumpFileMaybe logger Opt_D_dump_stg_from_js_sinker "STG Optimized JS Sinker:" FormatSTG
    (pprGenStgTopBindings (StgPprOpts False) stg_binds)

  (deps,lus) <- runG config this_mod unfloated_binds $ do
    ifProfilingM $ initCostCentres cccs
    lus  <- genUnits this_mod stg_binds spt_entries foreign_stubs
    deps <- genDependencyData this_mod lus
    pure (deps,lus)

  -- Doc to dump when -ddump-js is enabled
  when (logHasDumpFlag logger Opt_D_dump_js) $ do
    putDumpFileMaybe logger Opt_D_dump_js "JavaScript code" FormatJS
      $ vcat (fmap (jsToDoc . oiStat . luObjBlock) lus)

  -- Write the object file
  bh <- openBinMem (4 * 1000) -- a bit less than 4kB
  Object.putObject bh (moduleName this_mod) deps (map luObjBlock lus)

  createDirectoryIfMissing True (takeDirectory output_fn)
  writeBinMem bh output_fn



-- | Generate the ingredients for the linkable units for this module
genUnits :: HasDebugCallStack
         => Module
         -> [CgStgTopBinding]
         -> [SptEntry]
         -> ForeignStubs
         -> G [LinkableUnit] -- ^ the linkable units
genUnits m ss spt_entries foreign_stubs = do
    gbl     <- generateGlobalBlock
    exports <- generateExportsBlock
    others  <- go 2 ss
    pure (gbl:exports:others)
    where
      go :: HasDebugCallStack
         => Int                 -- the block we're generating (block 0 is the global unit for the module)
         -> [CgStgTopBinding]
         -> G [LinkableUnit]
      go !n = \case
        []     -> pure []
        (x:xs) -> do
          mlu <- generateBlock x n
          lus <- go (n+1) xs
          return (maybe lus (:lus) mlu)

      --   Generate the global unit that all other blocks in the module depend on
      --   used for cost centres and static initializers
      --   the global unit has no dependencies, exports the moduleGlobalSymbol
      generateGlobalBlock :: HasDebugCallStack => G LinkableUnit
      generateGlobalBlock = do
        glbl <- State.gets gsGlobal
        staticInit <-
          initStaticPtrs spt_entries
        let stat = ( jStgStatToJS
                   $ mconcat (reverse glbl) <> staticInit)
        let opt_stat = jsOptimize stat
        let syms = [moduleGlobalSymbol m]
        let oi = ObjBlock
                  { oiSymbols  = syms
                  , oiClInfo   = []
                  , oiStatic   = []
                  , oiStat     = opt_stat
                  , oiRaw      = mempty
                  , oiFExports = []
                  , oiFImports = []
                  }
        let lu = LinkableUnit
                  { luObjBlock     = oi
                  , luIdExports    = []
                  , luOtherExports = syms
                  , luIdDeps       = []
                  , luPseudoIdDeps = []
                  , luOtherDeps    = []
                  , luRequired     = False
                  , luForeignRefs  = []
                  }
        pure lu

      generateExportsBlock :: HasDebugCallStack => G LinkableUnit
      generateExportsBlock = do
        let (f_hdr, f_c) = case foreign_stubs of
                                  NoStubs            -> (empty, empty)
                                  ForeignStubs hdr c -> (getCHeader hdr, getCStub c)
            unique_deps = map mkUniqueDep (lines $ renderWithContext defaultSDocContext f_hdr)
            mkUniqueDep (tag:xs) = mkUnique tag (read xs)
            mkUniqueDep []       = panic "mkUniqueDep"

        let syms = [moduleExportsSymbol m]
        let raw  = utf8EncodeByteString $ renderWithContext defaultSDocContext f_c
        let oi = ObjBlock
                  { oiSymbols  = syms
                  , oiClInfo   = []
                  , oiStatic   = []
                  , oiStat     = mempty
                  , oiRaw      = raw
                  , oiFExports = []
                  , oiFImports = []
                  }
        let lu = LinkableUnit
                  { luObjBlock     = oi
                  , luIdExports    = []
                  , luOtherExports = syms
                  , luIdDeps       = []
                  , luPseudoIdDeps = unique_deps
                  , luOtherDeps    = []
                  , luRequired     = True
                  , luForeignRefs  = []
                  }
        pure lu

      --   Generate the linkable unit for one binding or group of
      --   mutually recursive bindings
      generateBlock :: HasDebugCallStack
                    => CgStgTopBinding
                    -> Int
                    -> G (Maybe LinkableUnit)
      generateBlock top_bind _n = case top_bind of
        StgTopStringLit bnd str -> do
          bids <- identsForId bnd
          case bids of
            [(identFS -> b1t),(identFS -> b2t)] -> do
              emitStatic b1t (StaticUnboxed (StaticUnboxedString str)) Nothing
              emitStatic b2t (StaticUnboxed (StaticUnboxedStringOffset str)) Nothing
              si        <- State.gets (ggsStatic . gsGroup)
              let ids = [bnd]
              syms <- (\(identFS -> i) -> [i]) <$> identForId bnd
              let oi = ObjBlock
                        { oiSymbols  = syms
                        , oiClInfo   = []
                        , oiStatic   = si
                        , oiStat     = mempty
                        , oiRaw      = ""
                        , oiFExports = []
                        , oiFImports = []
                        }
              let lu = LinkableUnit
                        { luObjBlock     = oi
                        , luIdExports    = ids
                        , luOtherExports = []
                        , luIdDeps       = []
                        , luPseudoIdDeps = []
                        , luOtherDeps    = []
                        , luRequired     = False
                        , luForeignRefs  = []
                        }
              pure (Just lu)
            _ -> panic "generateBlock: invalid size"

        StgTopLifted decl -> do
          tl        <- genToplevel decl
          extraTl   <- State.gets (ggsToplevelStats . gsGroup)
          ci        <- State.gets (ggsClosureInfo . gsGroup)
          si        <- State.gets (ggsStatic . gsGroup)
          unf       <- State.gets gsUnfloated
          extraDeps <- State.gets (ggsExtraDeps . gsGroup)
          fRefs     <- State.gets (ggsForeignRefs . gsGroup)
          resetGroup
          let allDeps  = collectIds unf decl
              topDeps  = collectTopIds decl
              required = hasExport decl
              stat     = jStgStatToJS
                         $ mconcat (reverse extraTl) <> tl
          let opt_stat = jsOptimize stat
          syms <- mapM (fmap (\(identFS -> i) -> i) . identForId) topDeps
          let oi = ObjBlock
                    { oiSymbols  = syms
                    , oiClInfo   = ci
                    , oiStatic   = si
                    , oiStat     = opt_stat
                    , oiRaw      = ""
                    , oiFExports = []
                    , oiFImports = fRefs
                    }
          let lu = LinkableUnit
                    { luObjBlock     = oi
                    , luIdExports    = topDeps
                    , luOtherExports = []
                    , luIdDeps       = allDeps
                    , luPseudoIdDeps = []
                    , luOtherDeps    = S.toList extraDeps
                    , luRequired     = required
                    , luForeignRefs  = fRefs
                    }
          pure $! seqList topDeps `seq` seqList allDeps `seq` Just lu

-- | variable prefix for the nth block in module

genToplevel :: CgStgBinding -> G JStgStat
genToplevel (StgNonRec bndr rhs) = genToplevelDecl bndr rhs
genToplevel (StgRec bs)          =
  mconcat <$> mapM (\(bndr, rhs) -> genToplevelDecl bndr rhs) bs

genToplevelDecl :: Id -> CgStgRhs -> G JStgStat
genToplevelDecl i rhs = do
  resetSlots (genToplevelConEntry i rhs)
  resetSlots (genToplevelRhs i rhs)

genToplevelConEntry :: Id -> CgStgRhs -> G ()
genToplevelConEntry i rhs = case rhs of
   StgRhsCon _cc con _mu _ts _args _typ
     | isDataConWorkId i
       -> genSetConInfo i con (stgRhsLive rhs) -- NoSRT
   StgRhsClosure _ _cc _upd_flag _args _body _typ
     | Just dc <- isDataConWorkId_maybe i
       -> genSetConInfo i dc (stgRhsLive rhs) -- srt
   _ -> pure ()

genSetConInfo :: HasDebugCallStack => Id -> DataCon -> LiveVars -> G ()
genSetConInfo i d l {- srt -} = do
  ei <- identForDataConEntryId i
  sr <- genStaticRefs l
  let fields = concatMap (typeJSRep . unwrapType . scaledThing)
                         (dataConRepArgTys d)
  emitClosureInfo $ ClosureInfo
    { ciVar = ei
    , ciRegs = CIRegs 0 [PtrV]
    , ciName = mkFastString $ renderWithContext defaultSDocContext (ppr d)
    , ciLayout = fixedLayout fields
    , ciType = CICon $ dataConTag d
    , ciStatic = sr
    }
  emitToplevel (mkDataEntry ei)

mkDataEntry :: Ident -> JStgStat
mkDataEntry i = FuncStat i [] returnStack

genToplevelRhs :: Id -> CgStgRhs -> G JStgStat
-- general cases:
genToplevelRhs i rhs = case rhs of
  StgRhsCon cc con _mu _tys args _typ -> do
    ii <- identForId i
    allocConStatic ii cc con args
    return mempty
  StgRhsClosure _ext cc _upd_flag {- srt -} args body typ -> do
    {-
      algorithm:
       - collect all Id refs that are in the global id cache
       - count usage in body for each ref
       - order by increasing use
       - prepend loading lives var to body: body can stay the same
    -}
    eid  <- identForEntryId i
    idt  <- identFS <$> identForId i
    body <- genBody (initExprCtx i) R2 args body typ
    occs <- globalOccs body
    let lids = global_id <$> (sortBy cmp_cnt $ nonDetEltsUFM occs)
    -- Regenerate idents from lids to restore right order of representatives.
    -- Representatives have occurrence order which can be mixed.
    lidents <- concat <$> traverse identsForId lids
    let eidt = identFS eid
    let lidents' = map identFS lidents
    CIStaticRefs sr0 <- genStaticRefsRhs rhs
    let sri = filter (`notElem` lidents') sr0
        sr   = CIStaticRefs sri
    et <- genEntryType args
    ll <- loadLiveFun lids
    (appK, regs, upd) <-
      if et == CIThunk
        then do
          r <- updateThunk
          pure (SAKThunk, CIRegs 0 [PtrV], r)
        else
          let regs = if null lidents then CIRegs 1 (concatMap idJSRep args)
                                     else CIRegs 0 (PtrV : concatMap idJSRep args)
          in pure (SAKFun, regs, mempty)
    setcc <- ifProfiling $
               if et == CIThunk
                 then enterCostCentreThunk
                 else enterCostCentreFun cc
    emitClosureInfo $ ClosureInfo
      { ciVar = eid
      , ciRegs = regs
      , ciName = idt
      , ciLayout = fixedLayout $ map (unaryTypeJSRep . idType) lids
      , ciType = et
      , ciStatic = sr
      }
    ccId <- costCentreStackLbl cc
    emitStatic idt (StaticApp appK eidt $ map StaticObjArg lidents') ccId
    return $ (FuncStat eid [] (ll <> upd <> setcc <> body))
    where
      cmp_cnt :: GlobalOcc -> GlobalOcc -> Ordering
      cmp_cnt g1 g2 = compare (global_count g1) (global_count g2)
