{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module GHC.CmmToWasm
   ( wasmCodeGen
   )
where

import GHC.Prelude


import GHC.CmmToAsm.Reg.Liveness


import GHC.CmmToAsm.PIC
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Types
import GHC.Cmm.DebugBlock

import GHC.Cmm.BlockId
import GHC.StgToCmm.CgUtils ( fixStgRegisters )
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Opt           ( cmmMachOpFold )
import GHC.Cmm.Ppr
import GHC.Cmm.CLabel

import GHC.CmmToWasm.Config


import GHC.Types.Unique.FM
import GHC.Types.Unique.Supply
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Utils.Misc
import GHC.Utils.Logger

import qualified GHC.Utils.Ppr as Pretty
import GHC.Utils.BufHandle
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Error
import GHC.Utils.Exception (evaluate)

import GHC.Wasm.ControlFlow

import GHC.Data.FastString
import GHC.Unit
import GHC.Data.Stream (Stream)
import qualified GHC.Data.Stream as Stream

import Control.Monad
import System.IO


--------------------

-- XXX placeholders

data WasmGenImpl a c = WasmGenImpl a c WasmGenConfig
wcgConfig :: WasmGenImpl a c -> WasmGenConfig
wcgConfig (WasmGenImpl _ _ cfg) = cfg

type WasmExpr = CmmExpr -- XXX placeholders
type WasmActions = ()



unimp :: String -> a
unimp s = error ("unimplemented: " ++ s)
--------------------


wasmCodeGen :: forall a . Logger -> WasmGenConfig -> ModLocation -> Handle -> UniqSupply
              -> Stream IO RawCmmGroup a
              -> IO a
wasmCodeGen logger config modLoc h us cmms
 = wasmCodeGen' logger config modLoc (WasmGenImpl undefined undefined config) h us cmms

type Wasm = WasmControl WasmActions WasmExpr
type WasmCmmDecl statics = GenCmmDecl statics (LabelMap RawCmmStatics) Wasm

wasmCodeGen' ::
                  Logger
               -> WasmGenConfig
               -> ModLocation
               -> WasmGenImpl statics jumpDest
               -> Handle
               -> UniqSupply
               -> Stream IO RawCmmGroup a
               -> IO a
wasmCodeGen' logger config modLoc wcgImpl h us cmms
 = do
        -- BufHandle is a performance hack.  We could hide it inside
        -- Pretty if it weren't for the fact that we do lots of little
        -- printDocs here (in order to do codegen in constant space).
        bufh <- newBufHandle h
        let wgs0 = WGS [] [] [] [] emptyUFM mapEmpty
        (wgs, us', a) <- wasmCodeGenStream logger config modLoc wcgImpl bufh us
                                           cmms wgs0
        _ <- finishWasmGen logger config modLoc bufh us' wgs
        return a


-- | Data accumulated during code generation. Mostly about statistics,
-- but also collects debug data for DWARF generation.
data WasmGenAcc statics
  = WGS { wgs_imports     :: ![[CLabel]]
        , wgs_wasms     :: ![[WasmCmmDecl statics]]
             -- ^ Native code generated, for statistics. This might
             -- hold a lot of data, so it is important to clear this
             -- field as early as possible if it isn't actually
             -- required.
        , wgs_labels      :: ![Label]
        , wgs_debug       :: ![DebugBlock]
        , wgs_dwarfFiles  :: !DwarfFiles
        , wgs_unwinds     :: !(LabelMap [UnwindPoint])
             -- ^ see Note [Unwinding information in the NCG]
             -- and Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock".
        }

{-
Note [Unwinding information in the NCG]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unwind information is a type of metadata which allows a debugging tool
to reconstruct the values of machine registers at the time a procedure was
entered. For the most part, the production of unwind information is handled by
the Cmm stage, where it is represented by CmmUnwind nodes.

Unfortunately, the Cmm stage doesn't know everything necessary to produce
accurate unwinding information. For instance, the x86-64 calling convention
requires that the stack pointer be aligned to 16 bytes, which in turn means that
GHC must sometimes add padding to $sp prior to performing a foreign call. When
this happens unwind information must be updated accordingly.
For this reason, we make the NCG backends responsible for producing
unwinding tables (with the extractUnwindPoints function in WasmGenImpl).

We accumulate the produced unwind tables over CmmGroups in the wgs_unwinds
field of WasmGenAcc. This is a label map which contains an entry for each
procedure, containing a list of unwinding points (e.g. a label and an associated
unwinding table).

See also Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock".
-}

-- XXX TODO replace prettyprinter with Data.ByteString.Builder

wasmCodeGenStream :: forall statics jumpDest a
              . Logger
             -> WasmGenConfig
             -> ModLocation
             -> WasmGenImpl statics jumpDest
             -> BufHandle
             -> UniqSupply
             -> Stream IO RawCmmGroup a
             -> WasmGenAcc statics
             -> IO (WasmGenAcc statics, UniqSupply, a)
wasmCodeGenStream logger config modLoc wcgImpl h us cmms wgs
 = loop us (Stream.runStream cmms) wgs
  where
    wcglabel = text "WasmCodeGen"
    loop :: UniqSupply
              -> Stream.StreamS IO RawCmmGroup a
              -> WasmGenAcc statics
              -> IO (WasmGenAcc statics, UniqSupply, a)
    loop us s wgs =
      case s of
        Stream.Done a ->
          return (wgs { wgs_imports = reverse $ wgs_imports wgs
                      , wgs_wasms = reverse $ wgs_wasms wgs
                      },
                  us,
                  a)
        Stream.Effect m -> m >>= \cmm_stream' -> loop us cmm_stream' wgs
        Stream.Yield cmms cmm_stream' -> do
          (us', wgs'') <-
            withTimingSilent logger
                wcglabel (\(a, b) -> a `seq` b `seq` ()) $ do
              -- Generate debug information
              let !ndbgs | wcgDwarfEnabled config = cmmDebugGen modLoc cmms
                         | otherwise              = []
                  dbgMap = debugToMap ndbgs

              -- Generate native code
              (wgs',us') <- cmmWasmGens logger config modLoc wcgImpl h
                                        dbgMap us cmms wgs 0

              -- Link native code information into debug blocks
              -- See Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock".
              let !ldbgs = cmmDebugLink (wgs_labels wgs') (wgs_unwinds wgs') ndbgs
                  platform = wcgPlatform config
              unless (null ldbgs) $
                putDumpFileMaybe logger Opt_D_dump_debug "Debug Infos" FormatText
                  (vcat $ map (pdoc platform) ldbgs)

              -- Accumulate debug information for emission in finishNativeGen.
              let wgs'' = wgs' { wgs_debug = wgs_debug wgs' ++ ldbgs, wgs_labels = [] }
              return (us', wgs'')

          loop us' cmm_stream' wgs''


---- XXX hack omitted:
----        -- and `bufh`, not `h`, gets passed to `cmmWasmGenStream`
----        (wgs, us', a) <- cmmWasmGenStream logger config modLoc wcgImpl bufh us
----                                         cmms wgs 0
----        _ <- finishWasmGen logger config modLoc bufh us' wgs
----        return a


finishWasmGen :: Logger
                -> WasmGenConfig
                -> ModLocation
                -> BufHandle
                -> UniqSupply
                -> WasmGenAcc statics
                -> IO UniqSupply
finishWasmGen logger config _modLoc bufh@(BufHandle _ _ h) us wgs
 = withTimingSilent logger (text "WCG") (`seq` ()) $ do
        -- Write debug data and finish
        bFlush bufh

        -- write out the imports
        let ctx = wcgAsmContext config
        printSDocLn ctx Pretty.LeftMode h
                $ makeImportsDoc config (concat (wgs_imports wgs))

        -- stats could be dumped here
        return us


cmmWasmGens :: forall statics jumpDest .
                 Logger
              -> WasmGenConfig
              -> ModLocation
              -> WasmGenImpl statics jumpDest
              -> BufHandle
              -> LabelMap DebugBlock
              -> UniqSupply
              -> [RawCmmDecl]
              -> WasmGenAcc statics
              -> Int
              -> IO (WasmGenAcc statics, UniqSupply)

cmmWasmGens logger config modLoc wcgImpl h dbgMap = go
  where
    go :: UniqSupply -> [RawCmmDecl]
       -> WasmGenAcc statics -> Int
       -> IO (WasmGenAcc statics, UniqSupply)

    go us [] wgs !_ =
        return (wgs, us)


    go us (cmm : cmms) wgs count = do
        let fileIds = wgs_dwarfFiles wgs
        (us', fileIds', wasm, imports, unwinds)
          <- {-# SCC "cmmWasmGen" #-}
             cmmWasmGen logger modLoc wcgImpl us fileIds dbgMap
                          cmm count

        -- Generate .file directives for every new file that has been
        -- used. (XXX not yet in wasm)
        let _newFileIds = nonDetEltsUFM $ fileIds' `minusUFM` fileIds

             -- XXX TODO will want config to say text vs binary
        emitWasmText logger config h $ vcat $
          map (pprWasmCmmDecl wcgImpl) wasm

        -- force evaluation all this stuff to avoid space leaks
        let platform = wcgPlatform config
        {-# SCC "seqString" #-} evaluate $ seqList (showSDocUnsafe $ vcat $ map (pdoc platform) imports) ()

        let !labels' = if wcgDwarfEnabled config
                       then wasmDebugLabels wasm else []
            !wasms'  = if logHasDumpFlag logger Opt_D_dump_asm_stats
                        then wasm : wgs_wasms wgs else []

            wgs' = wgs{ wgs_imports     = imports : wgs_imports wgs
                      , wgs_wasms       = wasms'
                      , wgs_labels      = wgs_labels wgs ++ labels'
                      , wgs_dwarfFiles  = fileIds'
                      , wgs_unwinds     = wgs_unwinds wgs `mapUnion` unwinds
                      }
        go us' cmms wgs' (count + 1)

wasmDebugLabels :: [WasmCmmDecl statics] -> [Label]
wasmDebugLabels _ =
    unimp "analog of cmmDebugLabels (no clue what it is yet)"


-- XXX TODO add function `emitWasmBinary`

emitWasmText :: Logger -> WasmGenConfig -> BufHandle -> SDoc -> IO ()
emitWasmText logger config h sdoc = do

        let ctx = wcgAsmContext config
        {-# SCC "pprNativeCode" #-} bufLeftRenderSDoc ctx h sdoc

        -- dump wasm code
        putDumpFileMaybe logger
                Opt_D_dump_asm "Wasm code" FormatASM -- XXX TODO
                                                     -- options may be
                                                     -- off here
                sdoc



-- | Complete Wasm code-generation phase for a single top-level chunk of Cmm.
--      Dumping the output of each stage along the way.
cmmWasmGen
    :: forall statics jumpDest.
       Logger
    -> ModLocation
    -> WasmGenImpl statics jumpDest
    -> UniqSupply
    -> DwarfFiles
    -> LabelMap DebugBlock
    -> RawCmmDecl                                   -- ^ the cmm to generate code for
    -> Int                                          -- ^ sequence number of this top thing
    -> IO   ( UniqSupply
            , DwarfFiles
            , [WasmCmmDecl statics]                -- Wasm code
            , [CLabel]                                  -- things imported by this cmm
            , LabelMap [UnwindPoint]                    -- unwinding information for blocks
            )

cmmWasmGen logger modLoc wcgImpl us fileIds dbgMap cmm _count
 = do -- _count used in NCG to identify dumped stats
        let config   = wcgConfig wcgImpl
        let platform = wcgPlatform config

        let _proc_name = case cmm of
                (CmmProc _ entry_label _ _) -> pdoc platform entry_label
                _                           -> text "DataChunk"

        -- rewrite assignments to global regs
        let fixed_cmm =
                {-# SCC "fixStgRegisters" #-}
                fixStgRegisters platform cmm -- XXX TODO
                                             -- refactor so we can
                                             -- provide list of
                                             -- registers, not a platform

        -- cmm to cmm optimisations
        let (opt_cmm, imports) =
                {-# SCC "cmmToCmm" #-}
                cmmToCmm config fixed_cmm

        putDumpFileMaybe logger
                Opt_D_dump_opt_cmm "Optimised Cmm" FormatCMM
                (pprCmmGroup platform [opt_cmm])

        -- allocate a local variable for each Cmm variable
        let varmap = localVarLocationsFrom cmm (length (wcgPinnedRegs config))
           -- consider putDumpFileMaybe

        -- generate wasm code from cmm
        let ((wasm, lastMinuteImports, fileIds'), usGen) =
                {-# SCC "genWasmCode" #-}
                initUs us $ genWasmCode config modLoc wasmTopCodeGen
                                        fileIds dbgMap opt_cmm varmap

        putDumpFileMaybe logger
                Opt_D_dump_asm_native "Wasm code" FormatWASM
                (vcat $ map (pprWasmCmmDecl wcgImpl) wasm)

        -- generate unwinding information from cmm
        let unwinds :: BlockMap [UnwindPoint]
            unwinds = unimp "generate unwinding information"

        return  ( usGen
                , fileIds'
                , wasm
                , lastMinuteImports ++ imports
                , unwinds )

-- | Build a doc for all the imports.
--
makeImportsDoc :: WasmGenConfig -> [CLabel] -> SDoc
makeImportsDoc _config _imports = unimp "way to specify imports for a Wasm module"

pprWasmCmmDecl :: WasmGenImpl statics jumpDest -> WasmCmmDecl statics -> SDoc
pprWasmCmmDecl _ = unimp "emit Wasm code (and change SDoc to Builder)"

newtype WasmLocal = WasmLocal Int

-- Local-register info.
-- A Cmm local register may be used either with integer type or with
-- floating-point type.  Each use must map onto a *different*
-- WebAssembly local variable.

data LRInfo = LRInt !WasmLocal
            | LRFloat !WasmLocal
            | LRBoth { lr_int :: !WasmLocal
                     , lr_float :: !WasmLocal
                     }

type LRMap = UniqueMap LRInfo

localVarLocationsFrom :: RawCmmDecl -> Int -> LRMap
localVarLocationsFrom _cmm _first = unimp "Cmm crawl for locals"

-- -----------------------------------------------------------------------------
-- Instruction selection

type CGMonad a = UniqSM a

genWasmCode
        :: WasmGenConfig
        -> ModLocation
        -> (RawCmmDecl -> CGMonad [WasmCmmDecl statics])
        -> DwarfFiles
        -> LabelMap DebugBlock
        -> RawCmmDecl
        -> LRMap
        -> UniqSM
                ( [WasmCmmDecl statics]
                , [CLabel]
                , DwarfFiles
                )

genWasmCode _config _modLoc _cmmTopCodeGen _fileIds _dbgMap _cmm_top
  = unimp "code generation with last-minute imports and dwarf files"

wasmTopCodeGen :: RawCmmDecl -> CGMonad [WasmCmmDecl statics]
wasmTopCodeGen = unimp "code generation"


-- -----------------------------------------------------------------------------
-- Generic Cmm optimiser

{-
Here we do:

  (a) Constant folding

-}

cmmToCmm :: WasmGenConfig -> RawCmmDecl -> (RawCmmDecl, [CLabel])
cmmToCmm _ top@(CmmData _ _) = (top, [])
cmmToCmm config (CmmProc info lbl live graph)
    = runCmmOpt config $
      do blocks' <- mapM cmmBlockConFold (toBlockList graph)
         return $ CmmProc info lbl live (ofBlockList (g_entry graph) blocks')

type OptMResult a = (# a, [CLabel] #)

pattern OptMResult :: a -> b -> (# a, b #)
pattern OptMResult x y = (# x, y #)
{-# COMPLETE OptMResult #-}

newtype CmmOptM a = CmmOptM (WasmGenConfig -> [CLabel] -> OptMResult a)
    deriving (Functor)

instance Applicative CmmOptM where
    pure x = CmmOptM $ \_ imports -> OptMResult x imports
    (<*>) = ap

instance Monad CmmOptM where
  (CmmOptM f) >>= g =
    CmmOptM $ \config imports0 ->
                case f config imports0 of
                  OptMResult x imports1 ->
                    case g x of
                      CmmOptM g' -> g' config imports1

instance CmmMakeDynamicReferenceM CmmOptM where
    addImport = addImportCmmOpt

addImportCmmOpt :: CLabel -> CmmOptM ()
addImportCmmOpt lbl = CmmOptM $ \_ imports -> OptMResult () (lbl:imports)

getCmmOptConfig :: CmmOptM WasmGenConfig
getCmmOptConfig = CmmOptM $ \config imports -> OptMResult config imports

runCmmOpt :: WasmGenConfig -> CmmOptM a -> (a, [CLabel])
runCmmOpt config (CmmOptM f) =
  case f config [] of
    OptMResult result imports -> (result, imports)

-- XXX TODO think about sharing this code with CmmToAsm
cmmBlockConFold :: CmmBlock -> CmmOptM CmmBlock
cmmBlockConFold block = do
  let (entry, middle, last) = blockSplit block
      stmts = blockToList middle
  stmts' <- mapM cmmStmtConFold stmts
  last' <- cmmStmtConFold last
  return $ blockJoin entry (blockFromList stmts') last'

-- This does three optimizations, but they're very quick to check, so we don't
-- bother turning them off even when the Hoopl code is active.  Since
-- this is on the old Cmm representation, we can't reuse the code either:
--  * reg = reg      --> nop
--  * if 0 then jump --> nop
--  * if 1 then jump --> jump
cmmStmtConFold :: CmmNode e x -> CmmOptM (CmmNode e x)
cmmStmtConFold stmt
   = case stmt of
        CmmAssign reg src
           -> do src' <- cmmExprConFold DataReference src
                 return $ case src' of
                   CmmReg reg' | reg == reg' -> CmmComment (fsLit "nop")
                   new_src -> CmmAssign reg new_src

        CmmStore addr src align
           -> do addr' <- cmmExprConFold DataReference addr
                 src'  <- cmmExprConFold DataReference src
                 return $ CmmStore addr' src' align

        CmmCall { cml_target = addr }
           -> do addr' <- cmmExprConFold JumpReference addr
                 return $ stmt { cml_target = addr' }

        CmmUnsafeForeignCall target regs args
           -> do target' <- case target of
                              ForeignTarget e conv -> do
                                e' <- cmmExprConFold CallReference e
                                return $ ForeignTarget e' conv
                              PrimTarget _ ->
                                return target
                 args' <- mapM (cmmExprConFold DataReference) args
                 return $ CmmUnsafeForeignCall target' regs args'

        CmmCondBranch test true false likely
           -> do test' <- cmmExprConFold DataReference test
                 return $ case test' of
                   CmmLit (CmmInt 0 _) -> CmmBranch false
                   CmmLit (CmmInt _ _) -> CmmBranch true
                   _other -> CmmCondBranch test' true false likely

        CmmSwitch expr ids
           -> do expr' <- cmmExprConFold DataReference expr
                 return $ CmmSwitch expr' ids

        other
           -> return other

cmmExprConFold :: ReferenceKind -> CmmExpr -> CmmOptM CmmExpr
cmmExprConFold referenceKind expr = do
    config <- getCmmOptConfig

    let expr' = if not (wcgDoConstantFolding config)
                    then expr
                    else cmmExprCon config expr

    cmmExprNative referenceKind expr'

cmmExprCon :: WasmGenConfig -> CmmExpr -> CmmExpr
cmmExprCon config (CmmLoad addr rep align) = CmmLoad (cmmExprCon config addr) rep align
cmmExprCon config (CmmMachOp mop args)
    = cmmMachOpFold (wcgPlatform config) mop (map (cmmExprCon config) args)
cmmExprCon _ other = other

-- handles both PIC and non-PIC cases... a very strange mixture
-- of things to do.
cmmExprNative :: ReferenceKind -> CmmExpr -> CmmOptM CmmExpr
cmmExprNative referenceKind expr = do
     config <- getCmmOptConfig
     let platform = wcgPlatform config
         directJump = True -- see usage below
     case expr of
        CmmLoad addr rep align
          -> do addr' <- cmmExprNative DataReference addr
                return $ CmmLoad addr' rep align

        CmmMachOp mop args
          -> do args' <- mapM (cmmExprNative DataReference) args
                return $ CmmMachOp mop args'

        CmmLit (CmmBlock id) -- XXX TODO ensure our Cmm has no such references
          -> cmmExprNative referenceKind (CmmLit (CmmLabel (infoTblLbl id)))
          -- we must convert block Ids to CLabels here, because we
          -- might have to do the PIC transformation.  Hence we must
          -- not modify BlockIds beyond this point.

        CmmLit (CmmLabel lbl)
          -> wasmMakeDynamicReference config referenceKind lbl
        CmmLit (CmmLabelOff lbl off)
          -> do dynRef <- wasmMakeDynamicReference config referenceKind lbl
                -- need to optimize here, since it's late
                return $ cmmMachOpFold platform (MO_Add (wordWidth platform)) [
                    dynRef,
                    CmmLit $ CmmInt (fromIntegral off) (wordWidth platform)
                  ]

        -- XXX keeping these around until we see if something similar applies on wasm

        -- It might be easier to jump directly to a label than
        -- to use the register table, so we replace these registers
        -- with the corresponding labels:
        CmmReg (CmmGlobal EagerBlackholeInfo)
          | directJump
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsUnitId (fsLit "__stg_EAGER_BLACKHOLE_info")))
        CmmReg (CmmGlobal GCEnter1)
          | directJump
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsUnitId (fsLit "__stg_gc_enter_1")))
        CmmReg (CmmGlobal GCFun)
          | directJump
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsUnitId (fsLit "__stg_gc_fun")))

        other
           -> return other


wasmMakeDynamicReference
  :: CmmMakeDynamicReferenceM m
  => WasmGenConfig
  -> ReferenceKind     -- whether this is the target of a jump
  -> CLabel            -- the label
  -> m CmmExpr

wasmMakeDynamicReference _config _referenceKind _lbl =
    unimp "convert CLabel to something Wasm can understand"
