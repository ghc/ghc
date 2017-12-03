{-# OPTIONS_GHC -fprof-auto -Wno-type-defaults -Wno-unused-matches -Wno-unused-local-binds -Wno-overlapping-patterns -Wno-incomplete-patterns -Wno-unused-do-bind -Wno-missing-signatures #-}
{-# LANGUAGE CPP, GADTs, GeneralizedNewtypeDeriving, RecursiveDo, LambdaCase, FlexibleInstances, FlexibleContexts, StandaloneDeriving, BangPatterns, TupleSections #-}
module Data.BitCode.LLVM.Gen where

#include "HsVersions.h"

import GhcPrelude

import qualified Data.BitCode.LLVM.Gen.Monad as Llvm
import qualified EDSL.Monad as EDSL
import qualified EDSL.Monad.EdslT as EDSL
import qualified EDSL as EDSL
import qualified Data.BitCode.LLVM.Classes.HasType as EDSL (ty)
import qualified Data.BitCode.LLVM.Util as EDSL
import EDSL ((-->), (++>))
import Data.BitCode.LLVM.Gen.Monad (LlvmT, runLlvmT, LlvmEnv(..))
import EDSL.Monad.EdslT (BodyBuilderT)
import Data.BitCode.LLVM.Pretty (pretty)
import Text.PrettyPrint
import qualified Data.BitCode.LLVM.Function        as Func
-- import Data.BitCode.LLVM (Ident(..))
-- import Data.BitCode.LLVM.Codes.Identification (Epoch(Current))

import CgUtils ( fixStgRegisters )
import PprCmm
-- import ErrUtils
import Outputable (panic)
import CmmUtils

-- import Control.Monad (liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (throwE, catchE)

import Outputable as Outp hiding ((<+>), text, ($+$), int, vcat)

import qualified Stream
-- debugging
-- import Debug.Trace
import Cmm
import CmmSwitch
-- import Data.Maybe (catMaybes)

import CLabel
import Platform

import CodeGen.Platform ( activeStgRegs, callerSaves )

import ForeignCall

import DynFlags
import Plugins (CommandLineOption)
import UniqSupply (MonadUnique(..), UniqSupply)
import BlockId (newBlockId, infoTblLbl)

-- body builder
-- import Data.BitCode.LLVM.Instruction (Inst)
import Data.BitCode.LLVM.Value (Symbol)
import Data.BitCode.LLVM.Types (BasicBlockId)
-- [TODO] reexport from the EDSL?
import Data.BitCode.LLVM.Codes.SynchronizationScope
import Data.BitCode.LLVM.Codes.AtomicOrdering

import qualified Data.BitCode.LLVM.Type            as Ty
import qualified Data.BitCode.LLVM.Value           as Val
import Data.Maybe (fromMaybe)

import Data.List (nub, sort)
import Data.Either (lefts, rights)
import Control.Monad
import Control.Monad.Fix (MonadFix(..))

import GHC.Stack (HasCallStack)

import Hoopl.Block
import Hoopl.Label
import Hoopl.Graph
import Hoopl.Collections

import Numeric (fromRat)

import qualified System.Environment as Env (lookupEnv)
import Data.Maybe (isJust)
import System.IO.Unsafe (unsafePerformIO)
--------------------------------------------------------------------------------
-- * Types
-- | Global registers live on proc entry
type LiveGlobalRegs = [GlobalReg]

--------------------------------------------------------------------------------
-- * Llvm Monad
newtype LlvmM a = LlvmM { unLlvmM :: LlvmT IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance HasDynFlags LlvmM where
  getDynFlags = LlvmM $ getDynFlags

instance MonadUnique LlvmM where
  getUniqueSupplyM = LlvmM $ getUniqueSupplyM
  getUniqueM       = LlvmM $ getUniqueM

{-# NOINLINE llvmDebugFlag #-}
llvmDebugFlag :: Bool
llvmDebugFlag = unsafePerformIO $ isJust <$> Env.lookupEnv "LLVM_DEBUG"

runLlvm :: [CommandLineOption] -> DynFlags -> FilePath -> UniqSupply
  -> LlvmM [Either (Edsl Val.Symbol) (Edsl Func.Function)]
  -> IO ()
runLlvm opts dflags fp us m = do
--  putStrLn $ "Output File: " ++ fp
--  putStrLn $ "CommandLineOptions: " ++ show opts
  mod <- flip evalStateT env . runLlvmT . unLlvmM $ do
--    liftIO . putStrLn $ "Generated Decls"
    decls <- {-# SCC "module_gen_decls" #-} m
--    liftIO . putStrLn $ "Generating module"
    {-# SCC "module_building" #-} EDSL.mod' "anon" (lefts decls) (rights decls)

  let target = LLVM_TARGET
      layout = case lookup target (llvmTargets dflags) of
        Just (LlvmTarget dl _ _) -> dl
        Nothing -> error $ "Failed to lookup the datalayout for " ++ target ++ "; available targets: " ++ show (map fst $ llvmTargets dflags)


  let mod' = case mod of
        Left e -> error e
        Right m -> m { EDSL.mTriple     = Just target
                     , EDSL.mDatalayout = Just layout
                     }

  dumpDeclsEnv <- liftIO $ Env.lookupEnv "LLVM_DUMP_DECLS"
  dumpAstEnv <- liftIO $ Env.lookupEnv "LLVM_DUMP_AST"
  when ("-dump-decls" `elem` opts || isJust dumpDeclsEnv) $
    liftIO $ do
      putStrLn "SYMBOLS: "
--      putStrLn . show . vcat . map pretty $ lefts decls
      putStrLn "FUNCTIONS: "
--      putStrLn . show . vcat . map pretty $ rights decls
  when ("-dump-ast" `elem` opts || isJust dumpAstEnv) $
    liftIO . putStrLn . show . pretty $ mod'
  when ("-dump-module" `elem` opts) $ EDSL.dumpModuleBitcodeAST (fp ++ "bin") mod'

  _ <- {-# SCC "module_write" #-} EDSL.writeModule fp mod'
  return ()
  where env = LlvmEnv { envDynFlags = dflags
                      , envUniq     = us }

type BodyBuilder a = BodyBuilderT LlvmM a
type Edsl a = EDSL.EdslT LlvmM a

instance HasDynFlags (BodyBuilderT LlvmM) where
  getDynFlags = lift getDynFlags

-- instance HasDynFlags (ExceptT e (BodyBuilderT LlvmM)) where
--  getDynFlags = lift getDynFlags

--------------------------------------------------------------------------------
-- * Lifted functions
getDynFlag :: (DynFlags -> a) -> LlvmM a
getDynFlag = LlvmM . Llvm.getDynFlag
dumpIfSetLlvm :: DumpFlag -> String -> Outp.SDoc -> LlvmM ()
dumpIfSetLlvm f s = LlvmM . Llvm.dumpIfSetLlvm f s

-- | Get the platform we are generating code for
getLlvmPlatform :: LlvmM Platform
getLlvmPlatform = getDynFlag targetPlatform

--------------------------------------------------------------------------------
-- * Cmm Helper
showCmm :: (HasDynFlags f, Outputable a, Functor f) => a -> f String
showCmm cmm = (\dflags -> showSDoc dflags (ppr cmm)) <$> getDynFlags

--------------------------------------------------------------------------------
-- Llvm Code gen

llvmCodeGen :: Stream.Stream LlvmM RawCmmGroup () -> LlvmM [(Either (Edsl Val.Symbol) (Edsl Func.Function))]
llvmCodeGen cmm_stream = do
  -- The cmm stream contains multiple groups.
  --
  -- each group consists of a set of data and procs.
  fns <- Stream.collect $ Stream.mapM llvmGroupGen cmm_stream
  -- as we want to put all these data and procs into a single module
  -- we simply concat the result of the stream.
  return $ concat fns

-- llvmCodeGen' :: RawCmmGroup -> LlvmM ()


-- | LLVM can't handle entry blocks which loop back to themselves (could be
-- seen as an LLVM bug) so we rearrange the code to keep the original entry
-- label which branches to a newly generated second label that branches back
-- to itself. See: Trac #11649
fixBottom :: MonadUnique m => RawCmmDecl -> m RawCmmDecl
fixBottom cp@(CmmProc hdr entry_lbl live g) =
    maybe (pure cp) fix_block $ mapLookup (g_entry g) blk_map
  where
    blk_map = toBlockMap g

    fix_block :: MonadUnique m => CmmBlock -> m RawCmmDecl
    fix_block blk
        | (CmmEntry e_lbl tickscp, middle, CmmBranch b_lbl) <- blockSplit blk
        , isEmptyBlock middle
        , e_lbl == b_lbl = do
            new_lbl <- newBlockId

            let fst_blk =
                    BlockCC (CmmEntry e_lbl tickscp) BNil (CmmBranch new_lbl)
                snd_blk =
                    BlockCC (CmmEntry new_lbl tickscp) BNil (CmmBranch new_lbl)

            pure . CmmProc hdr entry_lbl live . ofBlockMap (g_entry g)
                $ mapFromList [(e_lbl, fst_blk), (new_lbl, snd_blk)]

    fix_block _ = pure cp

fixBottom rcd = pure rcd

--------------------------------------------------------------------------------
-- * Groups
llvmGroupGen :: HasCallStack => RawCmmGroup -> LlvmM [(Either (Edsl Val.Symbol) (Edsl Func.Function))]
llvmGroupGen = return . map llvmCodeGen'

llvmCodeGen' :: HasCallStack => RawCmmDecl -> Either (Edsl Val.Symbol) (Edsl Func.Function)
llvmCodeGen' dat@(CmmData{}) = Left $ genLlvmData dat
llvmCodeGen' prc@(CmmProc{}) = Right $ do
    -- rewrite assignments to global regs
    dflags <- lift . lift $ getDynFlag id
    fixed_cmm@(CmmProc  infos entry_lbl live graph) <- lift . lift . fixBottom $
      {-# SCC "llvm_fix_regs" #-}
      fixStgRegisters dflags prc

    lift . lift $ dumpIfSetLlvm Opt_D_dump_opt_cmm "Optimised Cmm" (pprCmmGroup [fixed_cmm])

    -- extract the proper label for this function.
    let mb_info = mapLookup (g_entry graph) infos
        funLbl = case mb_info of
                   Nothing                   -> entry_lbl
                   Just (Statics info_lbl _) -> info_lbl

    lbl <- strCLabel_llvm funLbl

    let link :: Val.Value -> Val.Value
        link | externallyVisibleCLabel funLbl = Val.external -- External
             | otherwise                      = Val.private  -- Internal


    let blocks = toBlockListEntryFirstFalseFallthrough graph


    trash <- getTrashRegs

    let live' = activeStgRegs (targetPlatform dflags)

    let getAssignedRegs :: CmmNode O O -> [CmmReg]
        getAssignedRegs (CmmAssign reg _)= [reg]
          -- Calls will trash all registers. Unfortunately, this needs them to
          -- be stack-allocated in the first place.
        getAssignedRegs (CmmUnsafeForeignCall _ rs _) = map CmmGlobal trash ++ map CmmLocal rs
        getAssignedRegs _                             = []
        getRegsBlock (_, body, _)                     = concatMap getAssignedRegs $ blockToList body
        assignedRegs = nub . sort $ concatMap (getRegsBlock . blockSplit) blocks
--        localRegs  = [r | CmmLocal r  <- assignedRegs ]
        globalRegs = [r | CmmGlobal r <- assignedRegs ]
        isLive r     = r `elem` alwaysLive || r `elem` live
        isPassed r   = not (isSSE r) || isLive r

    liveRegsS             <- show <$> mapM showCmm live
    liveRegsS'            <- show <$> mapM showCmm live'
    alwaysLiveRegsS       <- show <$> mapM showCmm alwaysLive
    assignedGlobalRegsS   <- show <$> mapM showCmm globalRegs
    activeRegS            <- show <$> mapM showCmm (activeRegs dflags live) 
    allActiveRegsS        <- show <$> mapM showCmm (allActiveRegs dflags)

    let argRegs = filter isPassed . activeRegs dflags $ live ++ globalRegs

    argRegsS              <- show <$> mapM showCmm argRegs

    when llvmDebugFlag $ liftIO $ do
       putStrLn $ "llvmCodeGen: fun:                  " ++ show lbl
       putStrLn $ "llvmCodeGen: liveRegs:             " ++ liveRegsS
       putStrLn $ "llvmCodeGen: liveRegs':            " ++ liveRegsS'
       putStrLn $ "llvmCodeGen: alwaysLiveRegs:       " ++ alwaysLiveRegsS
       putStrLn $ "llvmCodeGen: assignedGlobalRegs:   " ++ assignedGlobalRegsS
       putStrLn $ "llvmCodeGen: activeRegs:           " ++ activeRegS
       putStrLn $ "llvmCodeGen: allActiveRegs:        " ++ allActiveRegsS
       putStrLn $ "llvmCodeGen: passed regs via args: " ++ argRegsS

    -- build up the function body; signature (and function is generated below)
    let body = basicBlocksCodeGen live blocks

    -- TODO: FLAGS: -traceDefSigs
    -- let sig = (fnSig dflags globalRegs)
    -- show the pretty signature on definition. E.g. add `traceShow (pretty sig)` infront of (fnSig...)

    -- produce a ghc function.
    -- now run the BodyBuilder on it with the function arguments.
    -- Eventually producing an LlvmM value.
    let sig = fnSig dflags argRegs
    case mb_info of
      Nothing -> EDSL.ghcdefT (pure link) lbl sig body
      Just (Statics _ statics)
        -> do prefixData <- EDSL.packedStruct =<< mapM genData statics
              EDSL.ghcdefT (pure $ EDSL.withPrefixData prefixData . link) lbl sig body

-- llvmCodeGen' _ = panic "LlvmCodeGen': unhandled raw cmm group"

fnSig :: DynFlags -> LiveGlobalRegs -> Edsl Ty.Ty
fnSig dflags live = (llvmFunArgs dflags live) --> EDSL.void

allocaAndStoreArg arg = do
  slot <- EDSL.alloca (EDSL.ty arg) =<< EDSL.int32 1
  EDSL.store slot arg
  return slot


-- TODO: Make CmmType an instance of HasType.
--       Also can we get away with a little less casting, by using isGcPtrType?
--       I'm still a big confused about the `Gc` in there though.
fromCmmType :: CmmType -> Edsl Ty.Ty
fromCmmType ty | isVecType ty   = EDSL.vec (vecLength ty) =<< fromCmmType (vecElemType ty)
               | isFloatType ty = floatTypeWithWidth (typeWidth ty)
               | otherwise      = intTypeWithWidth   (typeWidth ty)
  where floatTypeWithWidth W32  = EDSL.f32
        floatTypeWithWidth W64  = EDSL.f64   -- aka double
        floatTypeWithWidth W80  = EDSL.f80   -- x86 specific?
        floatTypeWithWidth W128 = EDSL.f128  -- always avilable?
        intTypeWithWidth        = EDSL.i . widthInBits

-- | Construct a floating point value
-- TODO: Maybe push Width into EDSL Types and Values?
float :: Width -> Double -> Edsl Symbol
float W32 = EDSL.float
float W64 = EDSL.double
float W80 = EDSL.floating 80
float W128 = EDSL.quad

allocaLocalRegs (LocalReg id ty) = EDSL.bind2 EDSL.alloca (fromCmmType ty) (EDSL.int32 1)
allocaGlobalRegs :: Edsl Ty.Ty -> Edsl Symbol
allocaGlobalRegs ty              = EDSL.bind2 EDSL.alloca ty               (EDSL.int32 1)

allActiveRegs :: DynFlags -> LiveGlobalRegs
allActiveRegs = activeStgRegs . targetPlatform

-- | Active registers are all that are not SSE or those that are
-- SSE and are in @alwaysLive@ or @live@
activeRegs :: DynFlags -> LiveGlobalRegs -> LiveGlobalRegs
activeRegs dflags live = filter isLive (allActiveRegs dflags)
  where isLive r = not (isSSE r) || r `elem` alwaysLive || r `elem` live

-- TODO: filter out all *non* live regs. (See LlvmCodeGen/Base.hs)
llvmFunArgs :: DynFlags -> LiveGlobalRegs -> [Edsl Ty.Ty]
llvmFunArgs dflags live = map (llvmRegType dflags) (activeRegs dflags live)

llvmRegType :: DynFlags -> GlobalReg -> Edsl Ty.Ty
llvmRegType dflags = regType
  where wordSize = wORD_SIZE dflags
        wordGlobal = EDSL.word wordSize
        ptrGlobal  = EDSL.ptr =<< wordGlobal
        fltGlobal = EDSL.f32
        dblGlobal = EDSL.f64
        xmmGlobal = EDSL.vec 4 =<< EDSL.i32
        ymmGlobal = EDSL.vec 8 =<< EDSL.i32
        zmmGlobal = EDSL.vec 16 =<< EDSL.i32
        regType BaseReg      = ptrGlobal
        regType Sp           = ptrGlobal
        regType Hp           = ptrGlobal
        regType VanillaReg{} = wordGlobal
        regType SpLim        = wordGlobal
        regType FloatReg{}   = fltGlobal
        regType DoubleReg{}  = dblGlobal
        regType XmmReg{}     = xmmGlobal
        regType YmmReg{}     = ymmGlobal
        regType ZmmReg{}     = zmmGlobal
        regType MachSp       = wordGlobal
        regType r            = panic $ "LlvmCodeGen.Reg: llvmFunArgs GlobalReg (" ++ show r ++ ") not supported!"


isSSE :: GlobalReg -> Bool
isSSE (FloatReg _)  = True
isSSE (DoubleReg _) = True
isSSE (XmmReg _)    = True
isSSE (YmmReg _)    = True
isSSE (ZmmReg _)    = True
isSSE _             = False

--------------------------------------------------------------------------------
-- * Data
--
-- TODO: Missing.
-- CmmData (part of RawCmmDecl = GenCmmDecl CmmStatics (BlockEvn CmmStatics) CmmGraph) for reference:
--
-- CmmData Section d -- d here is CmmStatics
-- Section = Section SectionType CLabel
-- SectionType = Text | Data | ReadOnlyData | RelocatableReadOnlyData | UninitialisedData | ReadOnlyData16 | OtherSection String
-- CmmStatics = Statics CLabel [CmmStatic]
-- CmmStatic = CmmStaticLit CmmLit
--           | CmmUninitialised Int
--           | CmmString [Word8]
--
-- Labels are addresses, and offsets are usually given in bytes.
-- CmmLit = CmmInt Integer Width
--        | CmmFloat Rational Width
--        | CmmVec [CmmLit]
--        | CmmLabel CLabel                     -- address of label
--        | CmmLabelOff CLabel Int              -- address of label + offset
--        | CmmLabelDiffOff CLabel CLabel Int   -- address of label1 - label2 + offset
--        | CmmBlock BlockId                    -- code label
--        | CmmHighStackMark                    -- This will *not* be supported!

genLlvmData :: RawCmmDecl -> Edsl Symbol
genLlvmData (CmmData section statics) = genStatics statics
  -- TODO: We irgnore the section right now.
  -- We will turn [CmmStatic] into a Struct.
  -- showCmm statics >>= (\s -> error $ "DATA: " ++ s)
  -- This is what we do for prefix data:
  -- Just (Statics _ statics) -> do
  -- infoStatics <- mapM genData statics
  -- return $ Just $ EDSL.struct infoStatics

genStatics :: CmmStatics -> Edsl Symbol
genStatics s@(Statics l statics) = do
  lbl  <- strCLabel_llvm l
  ty   <- tyCLabel_llvm l
  body <- mapM genData statics
  -- similarly to the genStaticLit, we will turn the
  -- ptr into an int.
  let link | externallyVisibleCLabel l = Val.external -- External
           | otherwise                 = Val.private  -- Internal

  struct <- EDSL.packedStruct body
  -- make statics mutable.
  -- E.g.
  --  x :: T
  --  x = unsafePerformIO $ newMVar ...
  --
  -- needs to be mutable.
  EDSL.global (Val.mutable . link) lbl $ struct

genData :: CmmStatic -> Edsl Symbol
genData (CmmString str) = EDSL.cStr $ map (toEnum . fromIntegral) str
genData (CmmUninitialised bytes) = EDSL.undef =<< (EDSL.arr bytes =<< EDSL.i8)
genData (CmmStaticLit lit) = genStaticLit lit

-- | Generate llvm code for a static literal
--
-- Will either generate the code or leave it unresolved if it is a 'CLabel'
-- which isn't yet known.

genStaticLit :: CmmLit -> Edsl Symbol
genStaticLit = \case
  (CmmInt i w)   -> EDSL.int (widthInBits w) i
  (CmmFloat r w) -> EDSL.floating (widthInBits w) (fromRat r)
  (CmmVec ls)    -> throwE "genStaticLit: CmmVec not supported!"
  (CmmLabel l)   -> do
    lbl <- strCLabel_llvm l
    ty  <- tyCLabel_llvm l
    l   <- EDSL.label lbl =<< EDSL.ptr ty
    EDSL.ptrToIntC ty l

  (CmmLabelOff l off) | off == 0  -> genStaticLit (CmmLabel l)
                      | otherwise -> do
                          size <- (*8) . wORD_SIZE <$> getDynFlags
                          n    <- EDSL.int size off
                          l'   <- genStaticLit (CmmLabel l)
                          EDSL.addC n l'
  (CmmLabelDiffOff l1 l2 off) | off == 0  -> do
                                  l1' <- genStaticLit (CmmLabel l1)
                                  l2' <- genStaticLit (CmmLabel l2)
                                  EDSL.subC l1' l2'
                              | otherwise -> do
                                  size <- (*8) . wORD_SIZE <$> getDynFlags
                                  n   <- EDSL.int size off
                                  l1' <- genStaticLit (CmmLabel l1)
                                  l2' <- genStaticLit (CmmLabel l2)
                                  EDSL.addC n =<<  EDSL.subC l1' l2'

  (CmmBlock b)     -> genStaticLit (CmmLabel $ infoTblLbl b)
  CmmHighStackMark -> throwE "genStaticLit: CmmHighStackMark unsupported!"
  _                -> throwE "genStaticLit: unsupported lit!"

genLit :: BlockMap -> RegMap -> CmmLit -> Edsl Symbol
genLit blockMap regMap = \case
    (CmmInt i w)   -> EDSL.int (widthInBits w) i
    (CmmFloat r w) -> EDSL.floating (widthInBits w) (fromRat r)
    (CmmVec ls)    -> throwE "genLit: CmmVec not supported!"
    (CmmLabel l)   -> do
      lbl <- strCLabel_llvm l
      ty  <- tyCLabel_llvm l
      -- FIXME: We do a ptrToInt cast here, if ty is int. This
      --        should better be done at the resolution site
      --        but we are not in the BodyBuilder at that point.
      if EDSL.isPtr ty
        then EDSL.label lbl ty
        else EDSL.ptrToInt ty =<< EDSL.label lbl =<< EDSL.ptr ty
    (CmmLabelOff l o) -> do
      -- liftIO . putStrLn . show =<< showCmm (CmmLabelOff l o)
      width <- (*8) . wORD_SIZE <$> getDynFlags
      lbl   <- genLit blockMap regMap (CmmLabel l)
      off   <- EDSL.int width o
      EDSL.add lbl off
    (CmmLabelDiffOff l1 l2 o) -> do
      width <- (*8) . wORD_SIZE <$> getDynFlags
      l1'   <- genLit blockMap regMap (CmmLabel l1)
      l2'   <- genLit blockMap regMap (CmmLabel l2)
      off   <- EDSL.int width o
      case (EDSL.ty l1', EDSL.ty l2') of
        (Ty.Int n, Ty.Int m) | n == m -> bind2 EDSL.add (EDSL.sub l1' l2') (pure off)
        _ -> panic "genLit: CmmLabelDiffOff encountered with different label ty!"

    (CmmBlock b)                -> genLit blockMap regMap (CmmLabel $ infoTblLbl b)
    CmmHighStackMark            -> throwE "genLit: CmmHighStackMark unsupported!"
    l                           -> throwE "genLit: unsupported lit!"

--------------------------------------------------------------------------------
-- * Procs
--
-- | Pretty print a 'CLabel'.
strCLabel_llvm :: (Monad a, HasDynFlags a) => CLabel -> a String
strCLabel_llvm lbl = do
  platform <- targetPlatform <$> getDynFlags
  dflags <- getDynFlags
  let sdoc = pprCLabel platform lbl
      str = Outp.renderWithStyle dflags sdoc (Outp.mkCodeStyle Outp.CStyle)
  return str

tyCLabel_llvm :: CLabel -> Edsl Ty.Ty
tyCLabel_llvm lbl = do
  dflags <- getDynFlags
  let ltype = cmmLitType dflags (CmmLabel lbl)
  fromCmmType ltype


getTrashRegs :: Edsl [GlobalReg]
getTrashRegs = do plat <- lift (lift getLlvmPlatform)
                  return $ filter (callerSaves plat) (activeStgRegs plat)
-- | Generate code for a list of blocks that make up a complete
-- procedure.  The first block in the list is expected to be the
-- entry point.  We will prefix this with the list of all
-- registers, to use in the function body.  LLVM's mem2reg
-- optimization pass will perform the actual register allocation
-- for us.
--
basicBlocksCodeGen :: HasCallStack => LiveGlobalRegs -> [CmmBlock] -> [Symbol] -> Edsl ()
basicBlocksCodeGen live bs@(entryBlock:cmmBlocks) args = mdo
  -- insert the function prologue, containing the
  -- local registers available.  As we generate typed
  -- references for each allocation, we end up with a
  -- list of (Register, TRef)

  trash <- getTrashRegs
  let getAssignedRegs :: CmmNode O O -> [CmmReg]
      getAssignedRegs (CmmAssign reg _)= [reg]
        -- Calls will trash all registers. Unfortunately, this needs them to
        -- be stack-allocated in the first place.
      getAssignedRegs (CmmUnsafeForeignCall _ rs _) = map CmmGlobal trash ++ map CmmLocal rs
      getAssignedRegs _                             = []
      getRegsBlock (_, body, _)                     = concatMap getAssignedRegs $ blockToList body
      assignedRegs = nub . sort $ concatMap (getRegsBlock . blockSplit) bs
      localRegs  = [r | CmmLocal r  <- assignedRegs ]
      globalRegs = [r | CmmGlobal r <- assignedRegs ]
      uGlobalRegs = [ r | r <- alwaysLive ++ live, not (r `elem` globalRegs)]
      isLive r     = r `elem` alwaysLive || r `elem` live
      isPassed r   = not (isSSE r) || isLive r


  dflags <- getDynFlags
  -- argRegs are those regs, that are passed via arguments
  let argRegs = filter isPassed $ activeRegs dflags (live ++ globalRegs) -- <- WARNING: this are only _assigned_ global regs.
      -- other assigned, registers, that are not passed.
      globalRegs' = filter (not . isPassed) globalRegs

  liveRegsS             <- show <$> mapM showCmm live
  argRegsS              <- show <$> mapM showCmm argRegs
  assignedGlobalRegsS   <- show <$> mapM showCmm globalRegs
  unassignedGlobalRegsS <- show <$> mapM showCmm uGlobalRegs
  globalRegs'S          <- show <$> mapM showCmm globalRegs'
  localRegsS            <- show <$> mapM showCmm localRegs

  when llvmDebugFlag $ liftIO . putStrLn . unlines
    $ map ("\tbasicBlocksCodeGen: " ++)
    $ [ "liveRegs:              " ++ liveRegsS
      , "argRegs:               " ++ argRegsS
      , "other assigned regs:   " ++ globalRegs'S
      , "local assigned regs:   " ++ localRegsS
      , "assignedGlobalRegs:    " ++ assignedGlobalRegsS
      , "unassignedGlobalRegsS: " ++ unassignedGlobalRegsS
      ]

  -- cmmExpr <- ("[basicBlocksCodeGen]\n" ++) . unlines <$> mapM showCmm bs
  -- return . trace cmmExpr $ \args -> mdo
  (eMap, regSlots) <-        entryBlockCodeGen argRegs args     globalRegs' localRegs  idMap  entryBlock
  idMap <- (eMap:) <$> mapM (basicBlockCodeGen argRegs regSlots                        idMap) cmmBlocks
  return ()

type BlockMapEntry = (Label, BasicBlockId)
type BlockMap = [BlockMapEntry]
type RegMapEntry = (CmmReg, Symbol)
type RegMap = [RegMapEntry]

entryBlockCodeGen :: HasCallStack
                  => LiveGlobalRegs
                  -> [Symbol]         -- ^ a set of arguments (entry block)
                  -> [GlobalReg]      -- ^ a set of global registers that will get assigned (and are not part of the passed arguments).
                  -> [LocalReg]       -- ^ a set of local registers that will get assigned.
                  -> BlockMap
                  -> CmmBlock
                  -> Edsl (BlockMapEntry, RegMap)
entryBlockCodeGen live args globalRegs localRegs idMap block = do
  let (_, nodes, tail) = blockSplit block
      id = entryLabel block
      stmts = blockToList nodes
  EDSL.block'' id $ do
    dflags <- getDynFlags
    -- for the entry block we will turn all arguments into
    -- assignments.
    -- create space on the stack to move all the function arguments into.
    -- the result will then contain a mapping of register to the references
    -- to that virtual register
    -- We also allocate local registers before hand. (TODO: can we allocate them on demand?)
    gRegs  <- mapM allocaAndStoreArg args
    gRegs' <- mapM (allocaGlobalRegs . llvmRegType dflags) globalRegs
    lRegs  <- mapM allocaLocalRegs localRegs


    liveRegsS    <- show <$> mapM showCmm live
    globalRegsS  <- show <$> mapM showCmm globalRegs
    localRegsS   <- show <$> mapM showCmm localRegs


    when llvmDebugFlag $ liftIO . putStrLn . unlines
      $ [ "live regs (" ++ show (length live) ++ "): " ++ liveRegsS
        , "used global regs (" ++ show (length globalRegs) ++ "): " ++ globalRegsS
        , "local regs (" ++ show (length localRegs) ++ "): " ++ localRegsS
        ]

    let regMap = (zip (map CmmGlobal live) gRegs)
              ++ (zip (map CmmGlobal globalRegs) gRegs')
              ++ (zip (map CmmLocal localRegs) lRegs)
    _ <- stmtsToInstrs idMap regMap stmts
    _ <- stmtToInstrs  idMap regMap tail
    return regMap

basicBlockCodeGen :: HasCallStack
                  => LiveGlobalRegs                                       -- ^ live global regs
                  -> RegMap                                               -- ^ Register -> Reference map.
                  -> BlockMap                                             -- ^ a map of BlockLabel -> BlockId
                  -> CmmBlock                                             -- ^ the actual block to assemble.
                  -> Edsl BlockMapEntry
basicBlockCodeGen live regMap idMap block = do
  let (_, nodes, tail) = blockSplit block
      id = entryLabel block
      stmts = blockToList nodes
  EDSL.block' id $ do
    dflags <- getDynFlags
    _ <- stmtsToInstrs idMap regMap stmts
    _ <- stmtToInstrs idMap regMap tail
    pure ()
-- | Convert a list of CmmNode's to LlvmStatement's
stmtsToInstrs :: HasCallStack => BlockMap -> RegMap -> [CmmNode e x] -> Edsl ()
stmtsToInstrs blockMap regMap stmts =  mapM_ (stmtToInstrs blockMap regMap) stmts


-- NOTE TO SELF: ULabel is {-# UNPACK #-} !Label

lookup_ k = fromMaybe (panic "not found") . lookup k

lookupGlobalReg
  :: (HasCallStack, HasDynFlags f, Monad f)
  => GlobalReg -> [(CmmReg, a)] -> f a
lookupGlobalReg g map = case lookup (CmmGlobal g) map of
  Just slot -> pure slot
  Nothing   -> do dflags <- getDynFlags
                  error $ "Failed to lookup global reg: " ++ showSDoc dflags (ppr g)
                  pure undefined

lookupLocalReg
  :: (HasCallStack, HasDynFlags f, Monad f)
  => LocalReg -> [(CmmReg, a)] -> f a
lookupLocalReg l map = case lookup (CmmLocal l) map of
  Just slot -> pure slot
  Nothing   -> do dflags <- getDynFlags
                  error $ "Failed to lookup local reg: " ++ showSDoc dflags (ppr l)
                  pure undefined

lookupReg
  :: (HasCallStack, Monad f, HasDynFlags f)
  => CmmReg -> [(CmmReg, a)] -> f a
lookupReg (CmmGlobal g) = lookupGlobalReg g
lookupReg (CmmLocal  l) = lookupLocalReg  l

loadGlobalReg :: HasCallStack => GlobalReg -> [(CmmReg, Symbol)] -> Edsl Symbol
loadGlobalReg g map = lookupGlobalReg g map >>= EDSL.load
loadLocalReg :: HasCallStack => LocalReg -> [(CmmReg, Symbol)] -> Edsl Symbol
loadLocalReg  l map = lookupLocalReg  l map >>= EDSL.load

loadReg :: HasCallStack => CmmReg -> RegMap -> Edsl Symbol
loadReg r m = lookupReg r m >>= EDSL.load

bclog :: String -> Edsl ()
bclog msg = do
  l <- EDSL.global Val.private "log" =<< EDSL.cStr msg 
  s <- EDSL.gep l =<< sequence [EDSL.int32 0, EDSL.int32 0]
  f <- EDSL.fun "puts" =<< [EDSL.i8ptr] --> EDSL.i32
  _ <- EDSL.ccall f [s]
  return ()

-- | Convert a CmmStmt to a list of LlvmStatement's
stmtToInstrs :: HasCallStack => BlockMap -> RegMap -> CmmNode e x -> Edsl ()
stmtToInstrs blockMap regMap stmt = flip catchE (\e -> showCmm stmt >>= \stmt -> throwE $ "in cmm stmt: " ++ stmt ++ "\n" ++ e) $ do
  dflags <- getDynFlags
  stmt' <- showCmm stmt
  -- liftIO . putStrLn $ "Compiling Cmm statement: " ++ showSDoc dflags (ppr stmt)
  -- DEBUG
  -- bclog embeds the cmm statement directly into the output stream.
  --       therfore the evaluated cmm statement will be printed right
  --       before the synthesized bitcode is executed.
  -- --
  -- bclog stmt'
  case stmt of
    -- nuke these
    CmmComment _ -> pure ()
    CmmTick    _ -> pure ()
    CmmUnwind {} -> pure () -- not yet supported

    -- CmmReg -> CmmExpr
    CmmAssign reg src -> genAssign blockMap regMap reg src
      -- slot <- lookupReg reg regMap
      -- var <- exprToVar blockMap regMap src
      -- EDSL.store slot var

    -- CmmExpr -> CmmExpr
    CmmStore addr src -> genStore blockMap regMap addr src

    -- ULabel
    CmmBranch id      -> EDSL.ubr (lookup_ id blockMap) >> pure ()
    -- CmmExpr -> ULabel -> ULabel -> Maybe Bool
    CmmCondBranch cond true false hint -> do
      c <- boolExprToVar blockMap regMap cond
      EDSL.br c (lookup_ true blockMap) (lookup_ false blockMap)
    -- CmmExpr -> SwitchTargets
    CmmSwitch cond ids -> do
      c <- exprToVar blockMap regMap cond
      size <- case EDSL.ty c of
            Ty.Int w -> return w
            _        -> throwE "Non-integer conditional in CmmSwitch"
      blocks <- sequence [(,lookup_ l blockMap) <$> EDSL.int size idx
                         | (idx, l) <- switchTargetsCases ids]
      -- out of range is undefined, so let's just branch to first label
      let defBlock | Just l <- switchTargetsDefault ids = lookup_ l blockMap
                   | otherwise                          = snd (head blocks)
      EDSL.switch c defBlock blocks
      pure ()

    -- Foreign call
    -- ForeignTarget -> [CmmFormal] -> [CmmActual]
    CmmUnsafeForeignCall target res args -> genCall blockMap regMap target res args

    -- Tail call
    CmmCall { cml_target = target,
              cml_args_regs = live }
      | (CmmLit (CmmLabel lbl)) <- target -> do
          -- liftIO . putStrLn $ "CmmCall: " ++ stmt'
          -- call a known function using a jump.
          fname <- strCLabel_llvm lbl
          -- fty   <- tyCLabel_llvm lbl
          fty'  <- flip fnSig live dflags -- <$> getDynFlags
          -- Let's ignore this for now, and just always generate the full type.
          -- unless (fty == fty') $ panic $ "types do not match for fn " ++ show fname ++"!\n  fty: " ++ show fty ++ "\n  fty': " ++ show fty'
          args  <- funArgs blockMap regMap live
          fn    <- EDSL.ghcfun fname fty'
          EDSL.tailghccall fn args
          EDSL.retVoid

      | otherwise -> do
          -- liftIO . putStrLn $ "CmmCall other: " ++ stmt'
          s <- exprToVar blockMap regMap target
          fty <- flip fnSig live <$> (lift getDynFlags)
          f <- bind2 EDSL.intToPtr (EDSL.ptr =<< fty) (pure s)
          EDSL.tailghccall f =<< funArgs blockMap regMap live
          EDSL.retVoid

    _ -> throwE "Llvm.GenCode.stmtToInstrs"


-- | A list of STG Registers that should always be considered alive
alwaysLive :: [GlobalReg]
alwaysLive = [BaseReg, Sp, Hp, SpLim, HpLim, node] -- node is in CmmExpr.

funArgs :: HasCallStack => BlockMap -> RegMap
        -> LiveGlobalRegs
        -> Edsl [Symbol]
funArgs blockMap regMap live = do

  let liveRegs = alwaysLive ++ live

  -- Set to value or "undef" depending on whether the register is
  -- actually live
  dflags <- getDynFlags

  -- XXX platform dependence!
  -- TODO: We always load *all* regs.
  loads <- forM (activeRegs dflags live) $ \case
    r | r `elem` liveRegs -> loadGlobalReg r regMap
      | not (isSSE r)     -> EDSL.undef =<< fromCmmType (globalRegType dflags r)

  return loads

-- genCall ---------------------------------------------------------------------
coerceArg t v | t == (EDSL.ty v) = pure v
              -- if both are pointers, assume we want a bitcast
              | EDSL.isPtr t && EDSL.isPtr (EDSL.ty v) = EDSL.bitcast t v
              -- if the required argument is a pointer, but the value is not
              -- assume the value represents a pointer.
              | EDSL.isPtr t = EDSL.intToPtr t v
              -- turn (const 1 :: i64) into (const 1 :: i8)
              | Val.Unnamed _ _ (Val.Constant (Ty.Int w') v') <- v
              , Ty.Int w <- t = EDSL.tellConst $ EDSL.uconst t v'
              -- turn ref X :: i64 into ref Y :: i8
              | Ty.Int w' <- EDSL.ty v
              , Ty.Int w  <- t
              , w < w' = EDSL.trunc t v
              -- otherwise, just fail
              | otherwise = throwE . show $ text "Foreign Call type error"
                            $+$ (text "Cannot coerce: " <+> pretty v <+> text (" (" ++ show v ++ ")")
                                 $+$ text "to: " <+> pretty t)



cast :: Ty.Ty -> Symbol -> Edsl Symbol
cast t s | t == EDSL.ty s = pure s
         | otherwise = case (EDSL.ty s, t) of
             (Ty.Int n, Ty.Int m) -> if n < m
                                         then EDSL.sext  t s
                                         else EDSL.trunc t s
             (st, _ )         | isFloat st && isFloat t -> if floatSize st < floatSize t
                                                   then EDSL.fpExt   t s
                                                   else EDSL.fpTrunc t s
             (Ty.Int _,  _)  | isFloat t  -> EDSL.siToFp t s
             (st, Ty.Int _)  | isFloat st -> EDSL.fpToSi t s
             (Ty.Int _, Ty.Ptr{})       -> EDSL.intToPtr t s
             (Ty.Ptr{}, Ty.Int _)       -> EDSL.ptrToInt t s
             (Ty.Ptr{}, Ty.Ptr{})       -> EDSL.bitcast t s
             (Ty.Vector{}, Ty.Vector{}) -> EDSL.bitcast t s
             (vt, t) -> panic $ "cast: Can't cast " ++ show vt ++ " to " ++ show t

  where isFloat :: Ty.Ty -> Bool
        isFloat t = t `elem` [Ty.Half, Ty.Float, Ty.Double, Ty.X86Fp80, Ty.Fp128]
        floatSize :: Ty.Ty -> Int
        floatSize Ty.Half = 16
        floatSize Ty.Float = 32
        floatSize Ty.Double = 64
        floatSize Ty.X86Fp80 = 80
        floatSize Ty.Fp128 = 128
-- Calling a foreign function
genCall :: HasCallStack => BlockMap -> RegMap
        -> ForeignTarget -> [CmmFormal] -> [CmmActual]
        -> Edsl ()
genCall blockMap regMap target dsts args = case target of
  -- TODO: Insert Fence instruction if needed, or can we simply insert one
  --       for each platform, and LLVM will ignore where not needed?
  (PrimTarget MO_WriteBarrier) -> EDSL.fence ORDERING_SEQCST CROSS_THREAD
  (PrimTarget MO_Touch)        -> pure () -- ignore
  (PrimTarget (MO_UF_Conv w))
    | ([dst],[e]) <- (dsts, args) -> do
        slot <- lookupLocalReg dst regMap
        val <- bind2 EDSL.uiToFp (EDSL.f (widthInBits w)) (exprToVar blockMap regMap e)
        EDSL.store slot val
    | otherwise                     -> panic $ "genCall: Too many arguments to MO_UF_Conv. "
                                     ++ "Can only handle 1, given " ++ show (length args) ++ "."
  (PrimTarget (MO_Prefetch_Data localityInt))
    | ([], [e]) <- (dsts, args)
    , 0 <= localityInt && localityInt <= 3   -> do
        f <- EDSL.fun "llvm.prefetch" =<< [EDSL.i8ptr, EDSL.i32, EDSL.i32, EDSL.i32] --> EDSL.void
        rest <- mapM EDSL.int32 [0, localityInt, 1]
        arg <- bind2 cast EDSL.i8ptr (exprToVar blockMap regMap e)
        -- TODO: trash regs here?
        void $ EDSL.ccall f (arg:rest)

    | ([], _args) <- (dsts, args)            -> panic $ "prefetch locality level integer must be between 0 and 3, given." ++ (show localityInt)
    | otherwise                              -> panic $ "genCall: Prefetch_data expected exactly 0 destinations, " ++ show (length dsts) ++ " given."
  (PrimTarget (MO_PopCnt w))
    | ([dst], [e]) <- (dsts, args) -> do
        slot <- lookupLocalReg dst regMap
        trunc <- bind2 cast (EDSL.i (widthInBits w)) (exprToVar blockMap regMap e)
        f <- let w' = (widthInBits w)
             in EDSL.fun ("llvm.ctpop.i" ++ show w') =<< [ EDSL.i w' ] --> EDSL.i w'
        Just ret <- EDSL.ccall f [ trunc ]
        ty <- EDSL.deptr (EDSL.ty slot)
        EDSL.store slot =<< cast ty ret
    | otherwise -> panic "genCall: PopCnt not implemented."
  (PrimTarget (MO_Clz w))
    | ([dst], [e]) <- (dsts, args) -> do
        slot <- lookupLocalReg dst regMap
        trunc <- bind2 cast (EDSL.i (widthInBits w)) (exprToVar blockMap regMap e)
        f <- let w' = (widthInBits w)
             in EDSL.fun ("llvm.ctlz.i" ++ show w') =<< [ EDSL.i w' ] --> EDSL.i w'
        Just ret <- EDSL.ccall f [ trunc ]
        ty <- EDSL.deptr (EDSL.ty slot)
        EDSL.store slot =<< cast ty ret
    | otherwise -> panic "genCall: Clz not implemented."
  (PrimTarget (MO_Ctz w))
    | ([dst], [e]) <- (dsts, args) -> do
        slot <- lookupLocalReg dst regMap
        trunc <- bind2 cast (EDSL.i (widthInBits w)) (exprToVar blockMap regMap e)
        f <- let w' = (widthInBits w)
             in EDSL.fun ("llvm.cttz.i" ++ show w') =<< [ EDSL.i w' ] --> EDSL.i w'
        Just ret <- EDSL.ccall f [ trunc ]
        ty <- EDSL.deptr (EDSL.ty slot)
        EDSL.store slot =<< cast ty ret
    | otherwise -> panic "genCall: Ctz not implemented."
  (PrimTarget (MO_BSwap w))
    | ([dst], [e]) <- (dsts, args) -> do
        slot <- lookupLocalReg dst regMap
        trunc <- bind2 cast (EDSL.i (widthInBits w)) (exprToVar blockMap regMap e)
        f <- let w' = (widthInBits w)
             in EDSL.fun ("llvm.bswap.i" ++ show w') =<< [ EDSL.i w' ] --> EDSL.i w'
        Just ret <- EDSL.ccall f [ trunc ]
        ty <- EDSL.deptr (EDSL.ty slot)
        EDSL.store slot =<< cast ty ret
    | otherwise -> panic "genCall: BSwap not implemented."
  (PrimTarget (MO_AtomicRMW w amop))
    | ([dst], [addr, n]) <- (dsts, args) -> do
        slot <- lookupLocalReg dst regMap

        addrVar <- exprToVar blockMap regMap addr
        nVar    <- exprToVar blockMap regMap n
        targetTy <- EDSL.ptr =<< EDSL.i (widthInBits w)
        ptrVar  <- EDSL.intToPtr targetTy addrVar

        let op = case amop of
              AMO_Add -> EDSL.atomicAdd
              AMO_Sub -> EDSL.atomicSub
              AMO_And -> EDSL.atomicAnd
              AMO_Nand -> EDSL.atomicNand
              AMO_Or  -> EDSL.atomicOr
              AMO_Xor -> EDSL.atomicXor

        ret <- op ptrVar nVar ORDERING_SEQCST CROSS_THREAD

        EDSL.store slot ret

    | otherwise -> panic "genCall: AtomicRMW not implemented."
  (PrimTarget (MO_AtomicRead _))
    | ([dst], [addr]) <- (dsts, args) -> do
        slot <- lookupLocalReg dst regMap
        v    <- genLoad blockMap regMap True addr (localRegType dst)
        EDSL.store slot v
    | otherwise -> panic "genCall: AtomicRead not implemented."
  (PrimTarget (MO_Cmpxchg width))
    | ([dst], [addr, old, new]) <- (dsts, args) -> do
        addrVar <- exprToVar blockMap regMap addr
        oldVar  <- exprToVar blockMap regMap old
        newVar  <- exprToVar blockMap regMap new
        ptrVar  <- flip EDSL.intToPtr addrVar =<< EDSL.ptr (EDSL.ty oldVar)
        slot    <- lookupLocalReg dst regMap
        retVar  <- EDSL.cmpXchg ptrVar oldVar newVar ORDERING_SEQCST CROSS_THREAD ORDERING_SEQCST
        retVar' <- EDSL.extractValue retVar [0]
        EDSL.store slot retVar'

    | otherwise -> panic "genCall: Cmpxchg not implemented."
  (PrimTarget (MO_AtomicWrite width))
    | ([], [addr, val]) <- (dsts, args) -> do
        addrVar <- exprToVar blockMap regMap addr
        valVar  <- exprToVar blockMap regMap val
        ptrVar  <- bind2 EDSL.intToPtr (EDSL.ptr (EDSL.ty valVar)) (pure addrVar)
        void $ EDSL.atomicXchg ptrVar valVar ORDERING_SEQCST CROSS_THREAD
    | otherwise -> panic "genCall: AtomicWrite not implemented."
  -- Handle memcpy function specifically since llvm's intrinsic version takes
  -- some extra parameters.
  (PrimTarget (MO_Memcpy align))
    | ([], [ dst, src, len ]) <- (dsts, args) -> do
        wordSize <- (*8) . wORD_SIZE <$> getDynFlags
        f <- case wordSize of
          32 -> EDSL.fun "llvm.memcpy.p0i8.p0i8.i32" =<< [ EDSL.i8ptr {- dst -}
                                                         , EDSL.i8ptr {- src -}
                                                         , EDSL.i32   {- len -}
                                                         , EDSL.i32   {- align -}
                                                         , EDSL.i1    {- volatile -}
                                                         ] --> EDSL.void
          64 -> EDSL.fun "llvm.memcpy.p0i8.p0i8.i64" =<< [ EDSL.i8ptr
                                                         , EDSL.i8ptr
                                                         , EDSL.i64
                                                         , EDSL.i32
                                                         , EDSL.i1 ] --> EDSL.void
          _  -> fail $ show wordSize ++ " bit words not supported for memcpy."
        -- try to coerce the args where necessary.
        alignment <- EDSL.int32 align
        volatile  <- EDSL.int (1 :: Int) (0 :: Int) -- False

        dst' <- bind2 coerceArg EDSL.i8ptr (exprToVar blockMap regMap dst)
        src' <- bind2 coerceArg EDSL.i8ptr (exprToVar blockMap regMap src)
        len' <- case wordSize of
          32 -> bind2 coerceArg EDSL.i32 (exprToVar blockMap regMap len)
          64 -> bind2 coerceArg EDSL.i64 (exprToVar blockMap regMap len)
          _  -> fail $ show wordSize ++ " bit words not supported for memcpy"

        EDSL.ccall f [ dst', src', len', alignment, volatile ]
        pure ()

    | otherwise -> panic $ "genCall: memcpy with " ++ show (length args) ++ " arguments not implemented."

  (PrimTarget (MO_Memset align))
    | ([], [dst, val, len ]) <- (dsts, args) -> do
        wordSize <- (*8) . wORD_SIZE <$> getDynFlags
        f <- case wordSize of
          32 -> EDSL.fun "llvm.memset.p0i8.i32" =<< [ EDSL.i8ptr {- dst -}
                                                    , EDSL.i8    {- val -}
                                                    , EDSL.i32   {- len -}
                                                    , EDSL.i32   {- align -}
                                                    , EDSL.i1    {- volatile -}
                                                    ] --> EDSL.void
          64 -> EDSL.fun "llvm.memset.p0i8.i64" =<< [ EDSL.i8ptr {- dst -}
                                                    , EDSL.i8    {- val -}
                                                    , EDSL.i64   {- len -}
                                                    , EDSL.i32   {- align -}
                                                    , EDSL.i1    {- volatile -}
                                                    ] --> EDSL.void

          _  -> fail $ show wordSize ++ " bit words not supported for memcpy."
        -- try to coerce the args where necessary.
        alignment <- EDSL.int32 align
        volatile  <- EDSL.int (1 :: Int) (0 :: Int) -- False

        dst' <- bind2 coerceArg EDSL.i8ptr (exprToVar blockMap regMap dst)
        val' <- bind2 coerceArg EDSL.i8    (exprToVar blockMap regMap val)
        len' <- case wordSize of
          32 -> bind2 coerceArg EDSL.i32   (exprToVar blockMap regMap len)
          64 -> bind2 coerceArg EDSL.i64   (exprToVar blockMap regMap len)
          _  -> fail $ show wordSize ++ " bit words not supported for memcpy"

        EDSL.ccall f [ dst', val', len', alignment, volatile ]
        pure ()
    | otherwise -> panic $ "genCall: memset with " ++ show (length args) ++ " arguments not implemented."

  (PrimTarget (MO_Memmove align))
    | ([], [ dst, src, len ]) <- (dsts, args) -> do
        wordSize <- (*8) . wORD_SIZE <$> getDynFlags
        f <- case wordSize of
          32 -> EDSL.fun "llvm.memmove.p0i8.p0i8.i32" =<< [ EDSL.i8ptr {- dst -}
                                                          , EDSL.i8ptr {- src -}
                                                          , EDSL.i32   {- len -}
                                                          , EDSL.i32   {- align -}
                                                          , EDSL.i1    {- volatile -}
                                                          ] --> EDSL.void
          64 -> EDSL.fun "llvm.memmove.p0i8.p0i8.i64" =<< [ EDSL.i8ptr
                                                          , EDSL.i8ptr
                                                          , EDSL.i64
                                                          , EDSL.i32
                                                          , EDSL.i1 ] --> EDSL.void
          _  -> fail $ show wordSize ++ " bit words not supported for memcpy."
        -- try to coerce the args where necessary.
        alignment <- EDSL.int32 align
        volatile <- EDSL.int (1 :: Int) (0 :: Int) -- False

        dst' <- bind2 coerceArg EDSL.i8ptr (exprToVar blockMap regMap dst)
        src' <- bind2 coerceArg EDSL.i8ptr (exprToVar blockMap regMap src)
        len' <- case wordSize of
          32 -> bind2 coerceArg EDSL.i32 (exprToVar blockMap regMap len)
          64 -> bind2 coerceArg EDSL.i64 (exprToVar blockMap regMap len)
          _  -> fail $ show wordSize ++ " bit words not supported for memcpy"

        EDSL.ccall f [ dst', src', len', alignment, volatile ]
        pure ()

    | otherwise -> panic $ "genCall: memmove with " ++ show (length args) ++ " arguments not implemented."

  -- We handle MO_U_Mul2 by simply using a 'mul' instruction, but with operands
  -- twice the width (we first zero-extend them), e.g., on 64-bit arch we will
  -- generate 'mul' on 128-bit operands. Then we only need some plumbing to
  -- extract the two 64-bit values out of 128-bit result.
  (PrimTarget (MO_U_Mul2 w))
    | ([dstH, dstL], [lhs, rhs]) <- (dsts, args) -> do
        let width = widthInBits w
            width2x = width * 2
        lhs2x <- bind2 EDSL.zext (EDSL.i width2x) (exprToVar blockMap regMap lhs)
        rhs2x <- bind2 EDSL.zext (EDSL.i width2x) (exprToVar blockMap regMap rhs)

        ret   <- EDSL.mul lhs2x rhs2x
        slotH <- lookupLocalReg dstH regMap
        slotL <- lookupLocalReg dstL regMap
        EDSL.store slotH =<< bind2 EDSL.trunc (EDSL.i width) (EDSL.lshrM (pure ret) (EDSL.int width2x width))
        EDSL.store slotL =<< bind2 EDSL.trunc (EDSL.i width) (pure ret)
    | otherwise -> panic "genCall: U_Mul2 not implemented"
  -- MO_U_QuotRem2 is another case we handle by widening the registers to double
  -- the width and use normal LLVM instructions (similarly to the MO_U_Mul2). The
  -- main difference here is that we need to combine two words into one register
  -- and then use both 'udiv' and 'urem' instructions to compute the result.
  (PrimTarget (MO_U_QuotRem2 w))
    | ([dstQ, dstR], [lhsH, lhsL, rhs]) <- (dsts, args) -> do
        let width = widthInBits w
            width2x = width * 2
        lhsH2x <- bind2 EDSL.zext (EDSL.i width2x) (exprToVar blockMap regMap lhsH)
        lhsL2x <- bind2 EDSL.zext (EDSL.i width2x) (exprToVar blockMap regMap lhsL)
        rhs2x  <- bind2 EDSL.zext (EDSL.i width2x) (exprToVar blockMap regMap rhs)

        -- combine the high and low part.
        lhs2x <- EDSL.or lhsL2x =<< EDSL.shlM (pure lhsH2x) (EDSL.int width2x width)

        slotQ <- lookupLocalReg dstQ regMap
        slotR <- lookupLocalReg dstR regMap

        EDSL.store slotQ =<< bind2 EDSL.trunc (EDSL.i width) (EDSL.udiv lhs2x rhs2x)
        EDSL.store slotR =<< bind2 EDSL.trunc (EDSL.i width) (EDSL.urem lhs2x rhs2x)
    | otherwise -> panic "genCall: U_QuotRem2 not implemented"
  -- Handle the MO_{Add,Sub}IntC separately. LLVM versions return a record from
  -- which we need to extract the actual values.
  (PrimTarget (MO_AddIntC w))
    | ([dstV, dstO], [lhs, rhs]) <- (dsts, args) -> do
        f <- let w' = (widthInBits w)
             in EDSL.fun ("llvm.sadd.with.overflow.i" ++ show w')
                =<< [ EDSL.i w', EDSL.i w'] --> (EDSL.ustruct =<< mapM EDSL.i [w', 1])
        Just ret <- EDSL.ccall f =<< mapM (exprToVar blockMap regMap) [lhs, rhs]
        slotV <- lookupLocalReg dstV regMap
        slotO <- lookupLocalReg dstO regMap
        EDSL.store slotV =<< EDSL.extractValue ret [0]
        EDSL.store slotO =<< bind2 EDSL.zext (EDSL.i (widthInBits w)) (EDSL.extractValue ret [1])
    | otherwise -> panic "genCall: AddIntC not implemented"
  (PrimTarget (MO_SubIntC w))
    | ([dstV, dstO], [lhs, rhs]) <- (dsts, args) -> do
        f <- let w' = (widthInBits w)
             in EDSL.fun ("llvm.ssub.with.overflow.i" ++ show w')
                =<< [ EDSL.i w', EDSL.i w'] --> (EDSL.ustruct =<< mapM EDSL.i [w', 1])
        Just ret <- EDSL.ccall f =<< mapM (exprToVar blockMap regMap) [lhs, rhs]
        slotV <- lookupLocalReg dstV regMap
        slotO <- lookupLocalReg dstO regMap
        EDSL.store slotV =<< EDSL.extractValue ret [0]
        EDSL.store slotO =<< bind2 EDSL.zext (EDSL.i (widthInBits w)) (EDSL.extractValue ret [1])
    | otherwise -> panic "genCall: SubIntC not implemented"
  -- Similar to MO_{Add,Sub}IntC, but MO_Add2 expects the first element of the
  -- return tuple to be the overflow bit and the second element to contain the
  -- actual result of the addition. So we still use genCallWithOverflow but swap
  -- the return registers.
  (PrimTarget (MO_Add2 w))
    | ([dstO, dstV],[lhs, rhs]) <- (dsts, args) -> do
        f <- let w' = (widthInBits w)
             in EDSL.fun ("llvm.uadd.with.overflow.i" ++ show w')
                =<< [ EDSL.i w', EDSL.i w'] --> (EDSL.ustruct =<< mapM EDSL.i [w', 1])
        Just ret <- EDSL.ccall f =<< mapM (exprToVar blockMap regMap) [lhs, rhs]
        slotV <- lookupLocalReg dstV regMap
        slotO <- lookupLocalReg dstO regMap
        EDSL.store slotV =<< EDSL.extractValue ret [0]
        EDSL.store slotO =<< bind2 EDSL.zext (EDSL.i (widthInBits w)) (EDSL.extractValue ret [1])
    | otherwise -> panic "genCall: Add2 not implemented"
  (PrimTarget (MO_SubWordC w))
    | ([dstV, dstO], [lhs, rhs]) <- (dsts, args) -> do
        f <- let w' = (widthInBits w)
             in EDSL.fun ("llvm.usub.with.overflow.i" ++ show w')
                =<< [ EDSL.i w', EDSL.i w'] --> (EDSL.ustruct =<< mapM EDSL.i [w', 1])
        Just ret <- EDSL.ccall f =<< mapM (exprToVar blockMap regMap) [lhs, rhs]
        slotV <- lookupLocalReg dstV regMap
        slotO <- lookupLocalReg dstO regMap
        EDSL.store slotV =<< EDSL.extractValue ret [0]
        EDSL.store slotO =<< bind2 EDSL.zext (EDSL.i (widthInBits w)) (EDSL.extractValue ret [1])
    | otherwise -> panic "genCall: SubWordC not implemented"
  target                         -> do
    -- liftIO $ putStrLn "Generic Call"
    dflags <- getDynFlags

    -- parameter types
    let arg_type (_, AddrHint) = EDSL.i8ptr
        -- cast pointers to i8*. Llvm equivalent of void*
        arg_type (expr, _) = fromCmmType $ cmmExprType dflags expr
        -- return type
        ret_type :: [(LocalReg, ForeignHint)] -> Edsl Ty.Ty
        ret_type [] = EDSL.void
        ret_type [(_, AddrHint)] = EDSL.i8ptr
        ret_type [(reg, _)] = fromCmmType $ localRegType reg
        ret_type t = panic $ "genCall: Too many return values! Can only handle"
                          ++ " 0 or 1, given " ++ show (length t) ++ "."

    let cc = case target of
          ForeignTarget _ (ForeignConvention conv _ _ _) ->
            case conv of
              PrimCallConv -> panic "genCall: PrimCallConv"
              JavaScriptCallConv -> panic "genCall: JavaScriptCallConv"
              -- while this can be made target dependent
              -- by emitting Stdcc for X86 targets, we'll
              -- try to be non-target dependent, and go with
              -- Ccc
              -- StdCallConv | CCCallConv | CApiConv
              _ -> True -- Cc_ccc
          PrimTarget _ -> True
    {-
        CC_Ccc of the possibilities here are a worry with the use of a custom
        calling convention for passing STG args. In practice the more
        dangerous combinations (e.g StdCall + llvmGhcCC) don't occur.

        The native code generator only handles StdCall and CCallConv.
    -}

    -- call attributes
    -- TODO: somehow handle this?!
    -- let fnAttrs | never_returns = NoReturn : llvmStdFunAttrs
    --             | otherwise     = llvmStdFunAttrs

    --     never_returns = case target of
    --          ForeignTarget _ (ForeignConvention _ _ _ CmmNeverReturns) -> True
    --          _ -> False

    -- fun type
    let (res_hints, arg_hints) = foreignTargetHints target
        args_hints = zip args arg_hints
        ress_hints = zip dsts res_hints
    retTy <- ret_type ress_hints
    argTy <- mapM arg_type args_hints -- TODO: we completely ignore any param attributes!
--    let funTy = \name -> LMFunction $ LlvmFunctionDecl name ExternallyVisible
--                             lmconv retTy FixedArgs argTy (llvmFunAlign dflags)
    fsig <- argTy ++> retTy
    fn <- getFunPtr blockMap regMap fsig target

    -- TODO: make use of hints.
    argVars <- zipWithM coerceArg argTy =<< mapM (exprToVar blockMap regMap) args
    let call = EDSL.ccall -- tail calls should be done through CmmJump, we'll do CCallConv and a standard call (no explicit tail)

    -- store the result value
    case retTy of
      Ty.Void   | length dsts == 0 -> call fn argVars >> pure ()
                | otherwise        -> panic $ "genCall: void result with register assignment!"
      _         | [reg] <- dsts    -> do Just res <- call fn argVars -- we *know* the function has a return value!
                                         slot <- lookupLocalReg reg regMap
                                         -- TODO: this is quite cmplex. We now go ahead
                                         --       and store res -> slot, even though we
                                         --       could later on just use the res slot.
                                         res' <- case (EDSL.ty res, EDSL.lower (EDSL.ty slot)) of
                                                   (t, s) | t == s -> pure res
                                                          -- if the slot type is a pointer
                                                          -- just bitcast the result to that.
                                                          | EDSL.isPtr s -> EDSL.bitcast s res
                                                          -- if the slot type is an Integer,
                                                          -- assume we want to store the pointer
                                                          -- address.
                                                          | EDSL.isInt s && EDSL.isPtr t -> EDSL.ptrToInt s res
                                                          | otherwise -> panic . show $ text "genCall: CmmReg" <+> pretty slot <+> text "bad match for result" <+> pretty res
                                         EDSL.store slot res'
                                         -- TODO: Add Return Nothing, if TailCall
                                         --       Add Unreachable if never_returns.
                                         --       Add nothing, otherwise.
                                         pure ()
                | otherwise        -> panic $ "genCall: Bad number of registers! Can only handle"
                                           ++ " 1, given " ++ show (length dsts) ++ "."

getFunPtr :: HasCallStack => BlockMap -> RegMap -> Ty.Ty -> ForeignTarget -> Edsl Symbol
getFunPtr blockMap regMap ty = \case
  ForeignTarget (CmmLit (CmmLabel lbl)) _ -> do
    lbl <- strCLabel_llvm lbl
    EDSL.fun lbl ty

  ForeignTarget expr _ -> do
    var <- exprToVar blockMap regMap expr
    case EDSL.ty var of
      t | EDSL.isPtr t -> flip EDSL.bitcast var =<< EDSL.ptr ty
      t | EDSL.isInt t -> flip EDSL.intToPtr var =<< EDSL.ptr ty

  PrimTarget mop -> case mop of
    MO_F32_Exp  -> EDSL.fun "llvm.exp.f32"  =<< [EDSL.f32] --> EDSL.f32
    MO_F32_Log  -> EDSL.fun "llvm.log.f32"  =<< [EDSL.f32] --> EDSL.f32
    MO_F32_Sqrt -> EDSL.fun "llvm.sqrt.f32" =<< [EDSL.f32] --> EDSL.f32
    MO_F32_Fabs -> EDSL.fun "llvm.fabs.f32" =<< [EDSL.f32] --> EDSL.f32
    MO_F32_Pwr  -> EDSL.fun "llvm.pow.f32"  =<< [EDSL.f32, EDSL.f32] --> EDSL.f32

    MO_F32_Sin  -> EDSL.fun "llvm.sin.f32"  =<< [EDSL.f32] --> EDSL.f32
    MO_F32_Cos  -> EDSL.fun "llvm.cos.f32"  =<< [EDSL.f32] --> EDSL.f32
    MO_F32_Tan  -> EDSL.fun "tanf"          =<< [EDSL.f32] --> EDSL.f32

    MO_F32_Asin -> EDSL.fun "asinf"         =<< [EDSL.f32] --> EDSL.f32
    MO_F32_Acos -> EDSL.fun "acosf"         =<< [EDSL.f32] --> EDSL.f32
    MO_F32_Atan -> EDSL.fun "atanf"         =<< [EDSL.f32] --> EDSL.f32

    MO_F32_Sinh -> EDSL.fun "sinhf"         =<< [EDSL.f32] --> EDSL.f32
    MO_F32_Cosh -> EDSL.fun "coshf"         =<< [EDSL.f32] --> EDSL.f32
    MO_F32_Tanh -> EDSL.fun "tanhf"         =<< [EDSL.f32] --> EDSL.f32

    MO_F64_Exp  -> EDSL.fun "llvm.exp.f64"  =<< [EDSL.f64] --> EDSL.f64
    MO_F64_Log  -> EDSL.fun "llvm.log.f64"  =<< [EDSL.f64] --> EDSL.f64
    MO_F64_Sqrt -> EDSL.fun "llvm.sqrt.f64" =<< [EDSL.f64] --> EDSL.f64
    MO_F64_Fabs -> EDSL.fun "llvm.fabs.f64" =<< [EDSL.f64] --> EDSL.f64
    MO_F64_Pwr  -> EDSL.fun "llvm.pow.f64"  =<< [EDSL.f64, EDSL.f64] --> EDSL.f64

    MO_F64_Sin  -> EDSL.fun "llvm.sin.f64"  =<< [EDSL.f64] --> EDSL.f64
    MO_F64_Cos  -> EDSL.fun "llvm.cos.f64"  =<< [EDSL.f64] --> EDSL.f64
    MO_F64_Tan  -> EDSL.fun "tan"           =<< [EDSL.f64] --> EDSL.f64

    MO_F64_Asin -> EDSL.fun "asin"          =<< [EDSL.f64] --> EDSL.f64
    MO_F64_Acos -> EDSL.fun "acos"          =<< [EDSL.f64] --> EDSL.f64
    MO_F64_Atan -> EDSL.fun "atan"          =<< [EDSL.f64] --> EDSL.f64

    MO_F64_Sinh -> EDSL.fun "sinh"          =<< [EDSL.f64] --> EDSL.f64
    MO_F64_Cosh -> EDSL.fun "cosh"          =<< [EDSL.f64] --> EDSL.f64
    MO_F64_Tanh -> EDSL.fun "tanh"          =<< [EDSL.f64] --> EDSL.f64
    MO_Memcmp _ -> do
      wordSize <- (*8) . wORD_SIZE <$> (lift getDynFlags)
      EDSL.fun "memcmp"        =<< [ EDSL.i8ptr
                                   , EDSL.i8ptr
                                   , EDSL.i wordSize] --> EDSL.i32

    _           -> panic $ "getFunPtr \\w primOp: " ++ show mop
  _              -> panic "getFunPtr, unknown case not implemented!"

--------------------------------------------------------------------------------
genAssign :: HasCallStack => BlockMap -> RegMap -> CmmReg -> CmmExpr -> Edsl ()
genAssign blockMap regMap reg val = do
  slot <- lookupReg reg regMap
  val' <- exprToVar blockMap regMap val
  wordSize <- fromIntegral . (*8) . wORD_SIZE <$> (lift getDynFlags)
  ty <- EDSL.deptr (EDSL.ty slot)
  case ty of
    Ty.Ptr _ _
      | EDSL.ty val' == Ty.Int wordSize -> EDSL.intToPtr ty val' >>= EDSL.store slot
    Ty.Vector _ _ -> EDSL.bitcast ty val' >>= EDSL.store slot
    _ -> EDSL.store slot val'

-- genStore --------------------------------------------------------------------
-- TODO: WIP!
-- | CmmStore operation
genStore :: HasCallStack => BlockMap -> RegMap -> CmmExpr -> CmmExpr -> Edsl ()
-- First we try to detect a few common cases and produce better code for
-- these then the default case. We are mostly trying to detect Cmm code
-- like I32[Sp + n] and use 'getelementptr' operations instead of the
-- generic case that uses casts and pointer arithmetic
genStore blockMap regMap addrE val = case addrE of
  (CmmReg (CmmGlobal r))       -> genStore_fast' addrE r 0 =<< exprToVar blockMap regMap val
  (CmmRegOff (CmmGlobal r) n)  -> genStore_fast' addrE r n =<< exprToVar blockMap regMap val
  (CmmMachOp (MO_Add _)
   [ (CmmReg (CmmGlobal r))
   , (CmmLit (CmmInt n _)) ])  -> genStore_fast' addrE r (fromInteger n) =<< exprToVar blockMap regMap val
  (CmmMachOp (MO_Sub _)
   [ (CmmReg (CmmGlobal r))
   , (CmmLit (CmmInt n _)) ])  -> genStore_fast' addrE r (negate $ fromInteger n) =<< exprToVar blockMap regMap val
  _                            -> genStore_slow' addrE =<< exprToVar blockMap regMap val

  where genStore_fast' = genStore_fast blockMap regMap
        genStore_slow' = genStore_slow blockMap regMap


-- | CmmStore operation
-- This is a special case for storing to a global register pointer
-- offset such as I32[Sp+8].
genStore_fast :: HasCallStack => BlockMap -> RegMap
              -> CmmExpr -> GlobalReg -> Int -> Symbol
              -> Edsl ()
genStore_fast blockMap regMap addr r n val = genStore_slow blockMap regMap addr val
  -- -- ptrSize (ptrs are the size of words)
  -- ptrSize <- (*8) . wORD_SIZE <$> (lift getDynFlags)
  -- slot <- loadGlobalReg r regMap
  -- let slotTy = EDSL.ty slot
  -- -- Note: n is in bytes. Hence we need to compute the actual offset
  -- --       depending on the underlying structure ourself.  As the
  -- --       getElementPointer works relative to the size of the
  -- --       underlying structure.
  -- -- we could compute the size of the element using gep.
  -- -- see: http://stackoverflow.com/a/30830445
  -- -- That way, we would need to insert additional blocks to
  -- -- handle the slow case, as we would need to verify that there
  -- -- is no remainder.
  -- --
  -- -- for now we will assume a pointer has the size of a word.
  --     (ix, rem) = n `divMod` ((EDSL.size ptrSize slotTy) `div` 8)
  --     expectTy = EDSL.ty val
  -- case rem of
  --   -- if its a bit type then we use the slow method since we
  --   -- can't avoid casting anyway.
  --   r | r /= 0 -> genStore_slow blockMap regMap addr val
  --     | not (EDSL.isPtr slotTy) -> genLoad_slow blockMap regMap addr val
  --     | not (baseTy slotTy == baseTy expectTy) -> genStore_slow blockMap regMap addr val
  --     -- If the ptrLevel of slotTy (lowered slot) and expectTy match. Let's just GEP it.
  --     | ptrLvl slotTy == ptrLvl expectTy -> do slot' <- EDSL.gep slot =<< sequence [EDSL.int32 ix]
  --                                              EDSL.store slot' val
  --     -- If the ptrLevel of the slotTy is larger; we need to bitcast the result.
  --     | ptrLvl slotTy >  ptrLvl expectTy -> do slot' <- bind2 EDSL.bitcast (EDSL.ptr expectTy) (EDSL.gep slot =<< sequence [EDSL.int32 ix])
  --                                              EDSL.store slot' val

  -- where
  --   ptrLvl t | EDSL.isPtr t = 1 + ptrLvl (EDSL.lower t)
  --            | otherwise = 0
  --   baseTy t | EDSL.isPtr t = baseTy (EDSL.lower t)
  --            | otherwise = t

genStore_slow :: HasCallStack => BlockMap -> RegMap
              -> CmmExpr -> Symbol
              -> Edsl ()
genStore_slow blockMap regMap addrExpr val = do
  slot <- exprToVar blockMap regMap addrExpr
  wordSize <- fromIntegral . (*8) . wORD_SIZE <$> (lift getDynFlags)
  case EDSL.ty slot of
    -- if the slot is a ptr to a ptr, assume we want to
    -- store the value as a ptr.
    Ty.Ptr _ ty@(Ty.Ptr _ _)
      | EDSL.ty val == Ty.Int wordSize -> do
          val' <- EDSL.intToPtr ty val
          EDSL.store slot val'
    -- if the slot is of ptr type, try to store the value.
    Ty.Ptr _ _ -> EDSL.store slot val
    -- if the slot ends up being an int, assume it's the address
    -- to be written to.
    i@(Ty.Int w)
      | w == wordSize -> do
          -- Note: I32[X] = Y, means Y is of type i32,
          --       and X might still be a 64bit int. As such
          --       the assumption is that we want the 32bit
          --       pointer at X to store 32bit wide Y in.
          slot' <- bind2 EDSL.intToPtr (EDSL.ptr (EDSL.ty val)) (pure slot)
          EDSL.store slot' val
    otherwise -> throwE $ "genStore: ptr not of the right type!\n Slot: " ++ (show slot) ++ "\n  Val: " ++ (show val)

--------------------------------------------------------------------------------
-- * CmmExpr code generation

-- | exprToVar' will just evaluate the expression. If the
-- resulting type does *not* match the expected cmm type
-- this will be ignored. Use @exprToVar@ if you want to
-- ensure that the result is widened if needed.
exprToVar' :: HasCallStack => BlockMap -> RegMap -> CmmExpr -> Edsl Symbol
exprToVar' blockMap regMap = \case
  -- Literal
  CmmLit lit         -> genLit blockMap regMap lit
  -- Read memory location
  CmmLoad e' ty      -> genLoad blockMap regMap False e' ty
  -- Contents of register
  CmmReg r           -> do wordSize <- (*8) . wORD_SIZE <$> (lift getDynFlags)
                           val <- loadReg r regMap
                           case EDSL.ty val of
                             -- Cmm wants the value, so pointers must be cast to ints.
                             Ty.Ptr _ _ -> bind2 EDSL.ptrToInt (EDSL.i wordSize) (pure val)
                             _          -> return val
  -- Machine operation
  CmmMachOp op exprs -> genMachOp blockMap regMap op exprs
  -- Expand the CmmRegOff shorthand.
  CmmRegOff reg off  -> do dflags <- getDynFlags
                           let rep = typeWidth (cmmRegType dflags reg)
                             in exprToVar blockMap regMap $ CmmMachOp (MO_Add rep) [CmmReg reg, CmmLit (CmmInt (fromIntegral off) rep)]
  CmmStackSlot _ _   -> panic "exprToVar: CmmStackSlot not supported!"

-- | exprToVar will compute the @Symbol@ for the @CmmExpr@ and ensure that
-- the @Symbol@ will match the expected return type of the @CmmExpr@.
exprToVar :: HasCallStack => BlockMap -> RegMap -> CmmExpr -> Edsl Symbol
exprToVar blockMap regMap expr = do
  dflags <- getDynFlags
  exprTy <- fromCmmType $ cmmExprType dflags expr
  var <- exprToVar' blockMap regMap expr
  case EDSL.ty var of
    t            | t == exprTy             -> pure var
    -- often boolean operations in llvm return i1, but ghc expects i<wordsize>
    t@(Ty.Int 1) | (Ty.Int _) <- exprTy    -> EDSL.zext exprTy var
    _                                      -> throwE $ "exprToVar: cannot make " ++ show (pretty var) ++ " into " ++ show exprTy

-- | boolExprToVar will compiute the @Symbol@ for the @CmmExpr@ and ensure
-- that the result is a boolean, independent of the type of the @CmmExpr@.
--
-- This is used in branch expressions, where the result is expected to be
-- binary.
boolExprToVar :: HasCallStack => BlockMap -> RegMap -> CmmExpr -> Edsl Symbol
boolExprToVar blockMap regMap expr = do
  var <- exprToVar' blockMap regMap expr
  if EDSL.isBoolTy var
    then pure var
    else panic $ "boolExprToVar: cannot make "-- ++ show (pretty var) ++ " into bool (i1)"


-- TODO: We might also want to short cut ((Reg +/- N) +/- M)
--       Instead of getting the relative offset of R and then
--       computing ptrToInt -> add/sub -> intToPtr.
genLoad :: HasCallStack => BlockMap -> RegMap -> Bool -> CmmExpr -> CmmType -> Edsl Symbol
genLoad blockMap regMap atomic e ty = genLoad_slow' e ty
  -- case e of
  -- (CmmReg (CmmGlobal r))        -> genLoad_fast' e r 0 ty
  -- (CmmRegOff (CmmGlobal r) n)   -> genLoad_fast' e r n ty
  -- (CmmMachOp (MO_Add _)
  --  [ (CmmReg (CmmGlobal r))
  --  , (CmmLit (CmmInt n _))]) -> genLoad_fast' e r (fromInteger n) ty
  -- (CmmMachOp (MO_Sub _)
  --  [ (CmmReg (CmmGlobal r))
  --  , (CmmLit (CmmInt n _))]) -> genLoad_fast' e r (negate $ fromInteger n) ty
  -- _ -> genLoad_slow' e ty
  where genLoad_fast' = genLoad_fast blockMap regMap atomic
        genLoad_slow' = genLoad_slow blockMap regMap atomic

genLoad_fast :: HasCallStack => BlockMap -> RegMap
             -> Bool -> CmmExpr -> GlobalReg -> Int -> CmmType
             -> Edsl Symbol
genLoad_fast blockMap regMap atomic e r n ty = do
  ptrSize <- (*8) <$> wORD_SIZE <$> (lift getDynFlags)
  slot <- lookupGlobalReg r regMap
  expectTy <- fromCmmType ty
  slotTy <- EDSL.deptr (EDSL.ty slot)

  let (ix, rem) = n `divMod` ((EDSL.size ptrSize slotTy) `div` 8)
  case rem of
    -- if its a bit type then we use the slow method since we
    -- can't avoid casting anyway.
    r | r /= 0 -> genLoad_slow blockMap regMap atomic e ty
      | not (EDSL.isPtr slotTy) -> genLoad_slow blockMap regMap atomic e ty
      | not (baseTy slotTy == baseTy expectTy) -> genLoad_slow blockMap regMap atomic e ty
      -- If the ptrLevel of slotTy (lowered slot) and expectTy match. Let's just GEP it.
      | ptrLvl slotTy == ptrLvl expectTy -> load =<< EDSL.gep slot =<< sequence [EDSL.int32 ix]
      -- If the ptrLevel of the slotTy is larger; we need to bitcast the result.
      | ptrLvl slotTy >  ptrLvl expectTy -> load =<< bind2 EDSL.bitcast (EDSL.ptr expectTy) (EDSL.gep slot =<< sequence [EDSL.int32 ix])
      -- this is just not logical!
      | otherwise -> throwE . show $
            pretty "(genLoad_fast)gep:" <+> (    text "Slot:" <+> pretty slot     <+> text "ptrLvl" <+> int (ptrLvl (EDSL.ty slot))
                                             $+$ text "ExpT:" <+> pretty expectTy <+> text "ptrLvl" <+> int (ptrLvl expectTy))

  where
    ptrLvl t | EDSL.isPtr t = 1 + ptrLvl (EDSL.lower t)
             | otherwise = 0
    baseTy t | EDSL.isPtr t = baseTy (EDSL.lower t)
             | otherwise = t
    load | atomic == False = EDSL.load
         | otherwise       = \s -> EDSL.atomicLoad s ORDERING_SEQCST CROSS_THREAD


genLoad_slow :: HasCallStack => BlockMap -> RegMap
             -> Bool -> CmmExpr -> CmmType
             -> Edsl Symbol
genLoad_slow blockMap regMap atomic e ty = do
  ptr <- exprToVar blockMap regMap e
  e' <- showCmm e
  ty' <- showCmm ty
  wordSize <- (*8) . wORD_SIZE <$> (lift getDynFlags)
  cmmType <- fromCmmType ty
  case EDSL.ty ptr of
    Ty.Ptr _ t   | t == cmmType                        -> load ptr
    -- E.g. I32[I64[...] + N], thus the expression will be 64bit, but we want to load 32bit
    t@(Ty.Int _) | t /= cmmType                        -> bind2 EDSL.intToPtr (EDSL.ptr =<< fromCmmType ty) (pure ptr) >>= load
    t@(Ty.Int _) | t == Ty.Int (fromIntegral wordSize) -> bind2 EDSL.intToPtr (EDSL.ptr (EDSL.ty ptr))      (pure ptr) >>= load
    otherwise                                          -> throwE $ "genLoad_slow not implemented, expr: " ++ e' ++ "("++ ty' ++ ")" ++ " -> " ++ show ptr
  where load | atomic == False = EDSL.load
             | otherwise       = \s -> EDSL.atomicLoad s ORDERING_SEQCST CROSS_THREAD

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f x y = do x' <- x; y' <- y; f x' y' 

genMachOp :: HasCallStack => BlockMap -> RegMap -> MachOp -> [CmmExpr] -> Edsl Symbol
genMachOp blockMap regMap op [x] = case op of
  MO_Not   w -> bind2 EDSL.xor (EDSL.int (widthInBits w) ((-1) :: Int))   (exprToVar blockMap regMap x)
  MO_S_Neg w -> bind2 EDSL.sub (EDSL.int (widthInBits w) (0 :: Int))      (exprToVar blockMap regMap x)
  MO_F_Neg w -> bind2 EDSL.sub (EDSL.floating (widthInBits w) (-0))       (exprToVar blockMap regMap x)

  MO_SF_Conv _ w -> bind2 EDSL.siToFp (EDSL.f (widthInBits w)) (exprToVar blockMap regMap x)
  MO_FS_Conv _ w -> bind2 EDSL.fpToSi (EDSL.i (widthInBits w)) (exprToVar blockMap regMap x)

  MO_SS_Conv from to
    | widthInBits from == widthInBits to -> exprToVar blockMap regMap x
    | widthInBits from  > widthInBits to -> bind2 EDSL.trunc (EDSL.i (widthInBits to)) (exprToVar blockMap regMap x)
    | widthInBits from  < widthInBits to -> bind2 EDSL.sext  (EDSL.i (widthInBits to)) (exprToVar blockMap regMap x)
  MO_UU_Conv from to
    | widthInBits from == widthInBits to -> exprToVar blockMap regMap x
    | widthInBits from  > widthInBits to -> bind2 EDSL.trunc (EDSL.i (widthInBits to)) (exprToVar blockMap regMap x)
    | widthInBits from  < widthInBits to -> bind2 EDSL.zext  (EDSL.i (widthInBits to)) (exprToVar blockMap regMap x)
  MO_FF_Conv from to
    | widthInBits from == widthInBits to -> exprToVar blockMap regMap x
    | widthInBits from  > widthInBits to -> bind2 EDSL.fpTrunc (EDSL.f (widthInBits to)) (exprToVar blockMap regMap x)
    | widthInBits from  < widthInBits to -> bind2 EDSL.fpExt   (EDSL.f (widthInBits to)) (exprToVar blockMap regMap x)

  -- MO_VS_Neg
  -- MO_VF_Neg

  _        -> panicOp
  where
    panicOp = panic $ "LLVM.CodeGen.genMachOp: non unary op encountered"
                        ++ "with one argument! (" ++ show op ++ ")"

-- Handle GlobalRegs pointers
genMachOp blockMap regMap o@(MO_Add _) e@[(CmmReg (CmmGlobal r)), (CmmLit (CmmInt n _))]
    = genMachOp_fast blockMap regMap o r (fromInteger n) e

genMachOp blockMap regMap o@(MO_Sub _) e@[(CmmReg (CmmGlobal r)), (CmmLit (CmmInt n _))]
    = genMachOp_fast blockMap regMap o r (negate . fromInteger $ n) e

-- Generic case
genMachOp blockMap regMap op e = genMachOp_slow blockMap regMap op e

genMachOp_fast
  :: HasCallStack
  => BlockMap
  -> RegMap
  -> MachOp
  -> GlobalReg
  -> Int
  -> [CmmExpr]
  -> Edsl Symbol
genMachOp_fast blockMap regMap op r n e = do
  -- See genStore_fast
  -- e'  <- showCmm e
  -- lift . EDSL.tellLog $ "genMachOp: " ++ show op ++ " - " ++ e'
  ptrSize <- (*8) <$> wORD_SIZE <$> (lift getDynFlags)
  slot <- loadGlobalReg r regMap
  let slotTy = EDSL.ty slot
      (ix, rem) = n `divMod` ((EDSL.size ptrSize slotTy) `div` 8)
  if EDSL.isPtr slotTy && rem == 0
    -- We are performing ADD or SUB, hence this would otherwise be:
    -- see also the exprToVar for CmmReg.
    then EDSL.bind2 EDSL.ptrToInt (EDSL.i ptrSize) (EDSL.gep slot =<< sequence [EDSL.int32 ix])
    else genMachOp_slow blockMap regMap op e

-- | Handle CmmMachOp expressions
-- This handles all the cases not handle by the specialised genMachOp_fast.
-- Element extraction
genMachOp_slow :: BlockMap -> RegMap -> MachOp -> [CmmExpr] -> Edsl Symbol
-- genMachOp_slow blockMap regMap (MO_V_Extract  l w) [val, idx] = return
-- genMachOp_slow blockMap regMap (MO_VF_Extract l w) [val, idx] = return
-- -- Element insertion
-- genMachOp_slow blockMap regMap (MO_V_Insert  l w) [val, elt, idx] = return
-- genMachOp_slow blockMap regMap (MO_VF_Insert l w) [val, elt, idx] = return
-- -- Binary MachOp
genMachOp_slow blockMap regMap op [x, y] = case op of
    MO_Eq _   -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.ieq lhs rhs
    MO_Ne _   -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.ineq lhs rhs
    MO_S_Gt _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.isgt lhs rhs
    MO_S_Ge _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.isge lhs rhs
    MO_S_Lt _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.islt lhs rhs
    MO_S_Le _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.isle lhs rhs
    MO_U_Gt _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.iugt lhs rhs
    MO_U_Ge _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.iuge lhs rhs
    MO_U_Lt _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.iult lhs rhs
    MO_U_Le _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.iule lhs rhs

    MO_Add _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.add lhs rhs
    MO_Sub _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      if EDSL.ty lhs == EDSL.ty rhs
        then EDSL.sub lhs rhs
        else if EDSL.isPtr (EDSL.ty lhs)
             -- we likely want pointer arithmetic.
             -- TODO: Use GEP or fold into parent
             --       instruction, see TODO above.
             --       not sure if that's even safe/legal.
             --       otherwise we migth have to adjust the
             --       load logic, to always ptrToInt...
             then do lhs' <- EDSL.ptrToInt (EDSL.ty rhs) lhs
                     EDSL.sub lhs' rhs
             else error $ "Cannot sub: " -- ++ (show . pretty . EDSL.ty $ rhs) ++ " from " ++ (show . pretty . EDSL.ty $ lhs)
    MO_Mul _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.mul lhs rhs

    MO_U_MulMayOflo w -> do
      f <- let w' = (widthInBits w)
           in EDSL.fun ("llvm.umul.with.overflow.i" ++ show w')
              =<< [ EDSL.i w', EDSL.i w'] --> (EDSL.ustruct =<< mapM EDSL.i [w', 1])
      Just ret <- EDSL.ccall f =<< mapM (exprToVar blockMap regMap) [x, y]
      bind2 EDSL.zext (EDSL.i (widthInBits w)) (EDSL.extractValue ret [1])

    MO_S_MulMayOflo w -> do
      f <- let w' = (widthInBits w)
           in EDSL.fun ("llvm.smul.with.overflow.i" ++ show w')
              =<< [ EDSL.i w', EDSL.i w'] --> (EDSL.ustruct =<< mapM EDSL.i [w', 1])
      Just ret <- EDSL.ccall f =<< mapM (exprToVar blockMap regMap) [x, y]
      bind2 EDSL.zext (EDSL.i (widthInBits w)) (EDSL.extractValue ret [1])

    MO_S_Quot _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.sdiv lhs rhs

    MO_S_Rem  _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.srem lhs rhs

    MO_U_Quot _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.udiv lhs rhs

    MO_U_Rem  _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.urem lhs rhs

    MO_F_Eq _ -> bind2 EDSL.foeq (exprToVar blockMap regMap x) (exprToVar blockMap regMap y)
    -- NOTE: GHC expects to be able to check against NaN, hence we need unorderd equality here.
    MO_F_Ne _ -> bind2 EDSL.fune (exprToVar blockMap regMap x) (exprToVar blockMap regMap y)
    MO_F_Gt _ -> bind2 EDSL.fogt (exprToVar blockMap regMap x) (exprToVar blockMap regMap y)
    MO_F_Ge _ -> bind2 EDSL.foge (exprToVar blockMap regMap x) (exprToVar blockMap regMap y)
    MO_F_Lt _ -> bind2 EDSL.folt (exprToVar blockMap regMap x) (exprToVar blockMap regMap y)
    MO_F_Le _ -> bind2 EDSL.fole (exprToVar blockMap regMap x) (exprToVar blockMap regMap y)

    MO_F_Add  _ -> bind2 EDSL.add (exprToVar blockMap regMap x) (exprToVar blockMap regMap y)
    MO_F_Sub  _ -> bind2 EDSL.sub (exprToVar blockMap regMap x) (exprToVar blockMap regMap y)
    MO_F_Mul  _ -> bind2 EDSL.mul (exprToVar blockMap regMap x) (exprToVar blockMap regMap y)
    MO_F_Quot _ -> bind2 EDSL.sdiv (exprToVar blockMap regMap x) (exprToVar blockMap regMap y)

    MO_And _   -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.and lhs rhs

    MO_Or  _   -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.or lhs rhs

    MO_Xor _   -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.xor lhs rhs

    MO_Shl _   -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.shl lhs rhs

    MO_U_Shr _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.lshr lhs rhs

    MO_S_Shr _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.ashr lhs rhs

    -- MO_V_Add l w   -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_Add
    -- MO_V_Sub l w   -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_Sub
    -- MO_V_Mul l w   -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_Mul

    -- MO_VS_Quot l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_SDiv
    -- MO_VS_Rem  l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_SRem

    -- MO_VU_Quot l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_UDiv
    -- MO_VU_Rem  l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_URem

    -- MO_VF_Add  l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FAdd
    -- MO_VF_Sub  l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FSub
    -- MO_VF_Mul  l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FMul
    -- MO_VF_Quot l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FDiv

    MO_Not _       -> panicOp
    MO_S_Neg _     -> panicOp
    MO_F_Neg _     -> panicOp

    MO_SF_Conv _ _ -> panicOp
    MO_FS_Conv _ _ -> panicOp
    MO_SS_Conv _ _ -> panicOp
    MO_UU_Conv _ _ -> panicOp
    MO_FF_Conv _ _ -> panicOp

    MO_V_Insert  {} -> panicOp
    MO_V_Extract {} -> panicOp

    MO_VS_Neg {} -> panicOp

    MO_VF_Insert  {} -> panicOp
    MO_VF_Extract {} -> panicOp

    MO_VF_Neg {} -> panicOp

    _            -> panicOp
    where
      panicOp = panic $ "LLVM.CodeGen.genMachOp_slow: unary op encountered"
                ++ "with two arguments! (" ++ show op ++ ")"

genMachOp_slow _blockMap _regMap op _e = panic $ "genMachOp_slow not supported for (" ++ show op ++ ")."
