{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Bytecode assembler and linker
module GHC.ByteCode.Asm (
        assembleBCOs, assembleOneBCO,
        bcoFreeNames,
        SizedSeq, sizeSS, ssElts,
        iNTERP_STACK_CHECK_THRESH,
        mkNativeCallInfoLit
  ) where

import GHC.Prelude

import GHC.ByteCode.Instr
import GHC.ByteCode.InfoTable
import GHC.ByteCode.Types
import GHCi.RemoteTypes
import GHC.Runtime.Interpreter
import GHC.Runtime.Heap.Layout ( fromStgWord, StgWord )

import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Literal
import GHC.Types.Unique.DSet

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Core.TyCon
import GHC.Data.FastString
import GHC.Data.SizedSeq

import GHC.StgToCmm.Layout     ( ArgRep(..) )
import GHC.Cmm.Expr
import GHC.Cmm.CallConv        ( allArgRegsCover )
import GHC.Platform
import GHC.Platform.Profile

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Data.Array.Unboxed as Array
import Data.Array.Base  ( UArray(..) )

import Foreign hiding (shiftL, shiftR)
import Data.Char        ( ord )
import Data.List        ( genericLength )
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import GHC.Float (castFloatToWord32, castDoubleToWord64)

-- -----------------------------------------------------------------------------
-- Unlinked BCOs

-- CompiledByteCode represents the result of byte-code
-- compiling a bunch of functions and data types

-- | Finds external references.  Remember to remove the names
-- defined by this group of BCOs themselves
bcoFreeNames :: UnlinkedBCO -> UniqDSet Name
bcoFreeNames bco
  = bco_refs bco `uniqDSetMinusUniqSet` mkNameSet [unlinkedBCOName bco]
  where
    bco_refs (UnlinkedBCO _ _ _ _ nonptrs ptrs)
        = unionManyUniqDSets (
             mkUniqDSet [ n | BCOPtrName n <- elemsFlatBag ptrs ] :
             mkUniqDSet [ n | BCONPtrItbl n <- elemsFlatBag nonptrs ] :
             map bco_refs [ bco | BCOPtrBCO bco <- elemsFlatBag ptrs ]
          )

-- -----------------------------------------------------------------------------
-- The bytecode assembler

-- The object format for bytecodes is: 16 bits for the opcode, and 16
-- for each field -- so the code can be considered a sequence of
-- 16-bit ints.  Each field denotes either a stack offset or number of
-- items on the stack (eg SLIDE), and index into the pointer table (eg
-- PUSH_G), an index into the literal table (eg PUSH_I/D/L), or a
-- bytecode address in this BCO.

-- Top level assembler fn.
assembleBCOs
  :: Interp
  -> Profile
  -> [ProtoBCO Name]
  -> [TyCon]
  -> AddrEnv
  -> Maybe ModBreaks
  -> IO CompiledByteCode
assembleBCOs interp profile proto_bcos tycons top_strs modbreaks = do
  -- TODO: the profile should be bundled with the interpreter: the rts ways are
  -- fixed for an interpreter
  itblenv <- mkITbls interp profile tycons
  bcos    <- mapM (assembleBCO (profilePlatform profile)) proto_bcos
  bcos'   <- mallocStrings interp bcos
  return CompiledByteCode
    { bc_bcos = bcos'
    , bc_itbls =  itblenv
    , bc_ffis = concatMap protoBCOFFIs proto_bcos
    , bc_strs = top_strs
    , bc_breaks = modbreaks
    }

-- Note [Allocating string literals]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Our strategy for handling top-level string literal bindings is described in
-- Note [Generating code for top-level string literal bindings] in GHC.StgToByteCode,
-- but not all Addr# literals in a program are guaranteed to be lifted to the
-- top level. Our strategy for handling local Addr# literals is somewhat simpler:
-- after assembling, we find all the BCONPtrStr arguments in the program, malloc
-- memory for them, and bake the resulting addresses into the instruction stream
-- in the form of BCONPtrWord arguments.
--
-- Since we do this when assembling, we only allocate the memory when we compile
-- the module, not each time we relink it. However, we do want to take care to
-- malloc the memory all in one go, since that is more efficient with
-- -fexternal-interpreter, especially when compiling in parallel.
--
-- Note that, as with top-level string literal bindings, this memory is never
-- freed, so it just leaks if the BCO is unloaded. See Note [Generating code for
-- top-level string literal bindings] in GHC.StgToByteCode for some discussion
-- about why.
--
mallocStrings :: Interp -> [UnlinkedBCO] -> IO [UnlinkedBCO]
mallocStrings interp ulbcos = do
  let bytestrings = reverse (execState (mapM_ collect ulbcos) [])
  ptrs <- interpCmd interp (MallocStrings bytestrings)
  return (evalState (mapM splice ulbcos) ptrs)
 where
  splice bco@UnlinkedBCO{..} = do
    lits <- mapM spliceLit unlinkedBCOLits
    ptrs <- mapM splicePtr unlinkedBCOPtrs
    return bco { unlinkedBCOLits = lits, unlinkedBCOPtrs = ptrs }

  spliceLit (BCONPtrStr _) = do
    rptrs <- get
    case rptrs of
      (RemotePtr p : rest) -> do
        put rest
        return (BCONPtrWord (fromIntegral p))
      _ -> panic "mallocStrings:spliceLit"
  spliceLit other = return other

  splicePtr (BCOPtrBCO bco) = BCOPtrBCO <$> splice bco
  splicePtr other = return other

  collect UnlinkedBCO{..} = do
    mapM_ collectLit unlinkedBCOLits
    mapM_ collectPtr unlinkedBCOPtrs

  collectLit (BCONPtrStr bs) = do
    strs <- get
    put (bs:strs)
  collectLit _ = return ()

  collectPtr (BCOPtrBCO bco) = collect bco
  collectPtr _ = return ()


assembleOneBCO :: Interp -> Profile -> ProtoBCO Name -> IO UnlinkedBCO
assembleOneBCO interp profile pbco = do
  -- TODO: the profile should be bundled with the interpreter: the rts ways are
  -- fixed for an interpreter
  ubco <- assembleBCO (profilePlatform profile) pbco
  [ubco'] <- mallocStrings interp [ubco]
  return ubco'

assembleBCO :: Platform -> ProtoBCO Name -> IO UnlinkedBCO
assembleBCO platform (ProtoBCO { protoBCOName       = nm
                             , protoBCOInstrs     = instrs
                             , protoBCOBitmap     = bitmap
                             , protoBCOBitmapSize = bsize
                             , protoBCOArity      = arity }) = do
  -- pass 1: collect up the offsets of the local labels.
  let asm = mapM_ (assembleI platform) instrs

      initial_offset = 0

      -- Jump instructions are variable-sized, there are long and short variants
      -- depending on the magnitude of the offset.  However, we can't tell what
      -- size instructions we will need until we have calculated the offsets of
      -- the labels, which depends on the size of the instructions...  So we
      -- first create the label environment assuming that all jumps are short,
      -- and if the final size is indeed small enough for short jumps, we are
      -- done.  Otherwise, we repeat the calculation, and we force all jumps in
      -- this BCO to be long.
      (n_insns0, lbl_map0) = inspectAsm platform False initial_offset asm
      ((n_insns, lbl_map), long_jumps)
        | isLargeW (fromIntegral $ Map.size lbl_map0)
          || isLargeW n_insns0
                    = (inspectAsm platform True initial_offset asm, True)
        | otherwise = ((n_insns0, lbl_map0), False)

      env :: LocalLabel -> Word
      env lbl = fromMaybe
        (pprPanic "assembleBCO.findLabel" (ppr lbl))
        (Map.lookup lbl lbl_map)

  -- pass 2: run assembler and generate instructions, literals and pointers
  let initial_state = (emptySS, emptySS, emptySS)
  (final_insns, final_lits, final_ptrs) <- flip execStateT initial_state $ runAsm platform long_jumps env asm

  -- precomputed size should be equal to final size
  massertPpr (n_insns == sizeSS final_insns)
             (text "bytecode instruction count mismatch")

  let asm_insns = ssElts final_insns
      !insns_arr =  mkBCOByteArray $ Array.listArray (0 :: Int, fromIntegral n_insns - 1) asm_insns
      !bitmap_arr = mkBCOByteArray $ mkBitmapArray bsize bitmap
      ul_bco = UnlinkedBCO nm arity insns_arr bitmap_arr (fromSizedSeq final_lits) (fromSizedSeq final_ptrs)

  -- 8 Aug 01: Finalisers aren't safe when attached to non-primitive
  -- objects, since they might get run too early.  Disable this until
  -- we figure out what to do.
  -- when (notNull malloced) (addFinalizer ul_bco (mapM_ zonk malloced))

  return ul_bco

mkBitmapArray :: Word -> [StgWord] -> UArray Int Word
-- Here the return type must be an array of Words, not StgWords,
-- because the underlying ByteArray# will end up as a component
-- of a BCO object.
mkBitmapArray bsize bitmap
  = Array.listArray (0, length bitmap) $
      fromIntegral bsize : map (fromInteger . fromStgWord) bitmap

-- instrs nonptrs ptrs
type AsmState = (SizedSeq Word16,
                 SizedSeq BCONPtr,
                 SizedSeq BCOPtr)

data Operand
  = Op Word
  | IOp Int
  | SmallOp Word16
  | LabelOp LocalLabel

wOp :: WordOff -> Operand
wOp = Op . fromIntegral

bOp :: ByteOff -> Operand
bOp = Op . fromIntegral

truncHalfWord :: Platform -> HalfWord -> Operand
truncHalfWord platform w = case platformWordSize platform of
  PW4 | w <= 65535      -> Op (fromIntegral w)
  PW8 | w <= 4294967295 -> Op (fromIntegral w)
  _ -> pprPanic "GHC.ByteCode.Asm.truncHalfWord" (ppr w)

data Assembler a
  = AllocPtr (IO BCOPtr) (Word -> Assembler a)
  | AllocLit [BCONPtr] (Word -> Assembler a)
  | AllocLabel LocalLabel (Assembler a)
  | Emit Word16 [Operand] (Assembler a)
  | NullAsm a
  deriving (Functor)

instance Applicative Assembler where
    pure = NullAsm
    (<*>) = ap

instance Monad Assembler where
  NullAsm x >>= f = f x
  AllocPtr p k >>= f = AllocPtr p (k >=> f)
  AllocLit l k >>= f = AllocLit l (k >=> f)
  AllocLabel lbl k >>= f = AllocLabel lbl (k >>= f)
  Emit w ops k >>= f = Emit w ops (k >>= f)

ioptr :: IO BCOPtr -> Assembler Word
ioptr p = AllocPtr p return

ptr :: BCOPtr -> Assembler Word
ptr = ioptr . return

lit :: [BCONPtr] -> Assembler Word
lit l = AllocLit l return

label :: LocalLabel -> Assembler ()
label w = AllocLabel w (return ())

emit :: Word16 -> [Operand] -> Assembler ()
emit w ops = Emit w ops (return ())

type LabelEnv = LocalLabel -> Word

largeOp :: Bool -> Operand -> Bool
largeOp long_jumps op = case op of
   SmallOp _ -> False
   Op w      -> isLargeW w
   IOp i     -> isLargeI i
   LabelOp _ -> long_jumps

runAsm :: Platform -> Bool -> LabelEnv -> Assembler a -> StateT AsmState IO a
runAsm platform long_jumps e = go
  where
    go (NullAsm x) = return x
    go (AllocPtr p_io k) = do
      p <- lift p_io
      w <- state $ \(st_i0,st_l0,st_p0) ->
        let st_p1 = addToSS st_p0 p
        in (sizeSS st_p0, (st_i0,st_l0,st_p1))
      go $ k w
    go (AllocLit lits k) = do
      w <- state $ \(st_i0,st_l0,st_p0) ->
        let st_l1 = addListToSS st_l0 lits
        in (sizeSS st_l0, (st_i0,st_l1,st_p0))
      go $ k w
    go (AllocLabel _ k) = go k
    go (Emit w ops k) = do
      let largeArgs = any (largeOp long_jumps) ops
          opcode
            | largeArgs = largeArgInstr w
            | otherwise = w
          words = concatMap expand ops
          expand (SmallOp w) = [w]
          expand (LabelOp w) = expand (Op (e w))
          expand (Op w) = if largeArgs then largeArg platform (fromIntegral w) else [fromIntegral w]
          expand (IOp i) = if largeArgs then largeArg platform (fromIntegral i) else [fromIntegral i]
      state $ \(st_i0,st_l0,st_p0) ->
        let st_i1 = addListToSS st_i0 (opcode : words)
        in ((), (st_i1,st_l0,st_p0))
      go k

type LabelEnvMap = Map LocalLabel Word

data InspectState = InspectState
  { instrCount :: !Word
  , ptrCount :: !Word
  , litCount :: !Word
  , lblEnv :: LabelEnvMap
  }

inspectAsm :: Platform -> Bool -> Word -> Assembler a -> (Word, LabelEnvMap)
inspectAsm platform long_jumps initial_offset
  = go (InspectState initial_offset 0 0 Map.empty)
  where
    go s (NullAsm _) = (instrCount s, lblEnv s)
    go s (AllocPtr _ k) = go (s { ptrCount = n + 1 }) (k n)
      where n = ptrCount s
    go s (AllocLit ls k) = go (s { litCount = n + genericLength ls }) (k n)
      where n = litCount s
    go s (AllocLabel lbl k) = go s' k
      where s' = s { lblEnv = Map.insert lbl (instrCount s) (lblEnv s) }
    go s (Emit _ ops k) = go s' k
      where
        s' = s { instrCount = instrCount s + size }
        size = sum (map count ops) + 1
        largeOps = any (largeOp long_jumps) ops
        count (SmallOp _) = 1
        count (LabelOp _) = count (Op 0)
        count (Op _) = if largeOps then largeArg16s platform else 1
        count (IOp _) = if largeOps then largeArg16s platform else 1

-- Bring in all the bci_ bytecode constants.
#include "Bytecodes.h"

largeArgInstr :: Word16 -> Word16
largeArgInstr bci = bci_FLAG_LARGE_ARGS .|. bci

largeArg :: Platform -> Word64 -> [Word16]
largeArg platform w = case platformWordSize platform of
   PW8 -> [fromIntegral (w `shiftR` 48),
           fromIntegral (w `shiftR` 32),
           fromIntegral (w `shiftR` 16),
           fromIntegral w]
   PW4 -> assertPpr (w < fromIntegral (maxBound :: Word32))
                    (text "largeArg too big:" <+> ppr w) $
          [fromIntegral (w `shiftR` 16),
           fromIntegral w]

largeArg16s :: Platform -> Word
largeArg16s platform = case platformWordSize platform of
   PW8 -> 4
   PW4 -> 2

assembleI :: Platform
          -> BCInstr
          -> Assembler ()
assembleI platform i = case i of
  STKCHECK n               -> emit bci_STKCHECK [Op n]
  PUSH_L o1                -> emit bci_PUSH_L [wOp o1]
  PUSH_LL o1 o2            -> emit bci_PUSH_LL [wOp o1, wOp o2]
  PUSH_LLL o1 o2 o3        -> emit bci_PUSH_LLL [wOp o1, wOp o2, wOp o3]
  PUSH8 o1                 -> emit bci_PUSH8 [bOp o1]
  PUSH16 o1                -> emit bci_PUSH16 [bOp o1]
  PUSH32 o1                -> emit bci_PUSH32 [bOp o1]
  PUSH8_W o1               -> emit bci_PUSH8_W [bOp o1]
  PUSH16_W o1              -> emit bci_PUSH16_W [bOp o1]
  PUSH32_W o1              -> emit bci_PUSH32_W [bOp o1]
  PUSH_G nm                -> do p <- ptr (BCOPtrName nm)
                                 emit bci_PUSH_G [Op p]
  PUSH_PRIMOP op           -> do p <- ptr (BCOPtrPrimOp op)
                                 emit bci_PUSH_G [Op p]
  PUSH_BCO proto           -> do let ul_bco = assembleBCO platform proto
                                 p <- ioptr (liftM BCOPtrBCO ul_bco)
                                 emit bci_PUSH_G [Op p]
  PUSH_ALTS proto pk
                           -> do let ul_bco = assembleBCO platform proto
                                 p <- ioptr (liftM BCOPtrBCO ul_bco)
                                 emit (push_alts pk) [Op p]
  PUSH_ALTS_TUPLE proto call_info tuple_proto
                           -> do let ul_bco = assembleBCO platform proto
                                     ul_tuple_bco = assembleBCO platform
                                                                tuple_proto
                                 p <- ioptr (liftM BCOPtrBCO ul_bco)
                                 p_tup <- ioptr (liftM BCOPtrBCO ul_tuple_bco)
                                 info <- word (fromIntegral $
                                              mkNativeCallInfoSig platform call_info)
                                 emit bci_PUSH_ALTS_T
                                      [Op p, Op info, Op p_tup]
  PUSH_PAD8                -> emit bci_PUSH_PAD8 []
  PUSH_PAD16               -> emit bci_PUSH_PAD16 []
  PUSH_PAD32               -> emit bci_PUSH_PAD32 []
  PUSH_UBX8 lit            -> do np <- literal lit
                                 emit bci_PUSH_UBX8 [Op np]
  PUSH_UBX16 lit           -> do np <- literal lit
                                 emit bci_PUSH_UBX16 [Op np]
  PUSH_UBX32 lit           -> do np <- literal lit
                                 emit bci_PUSH_UBX32 [Op np]
  PUSH_UBX lit nws         -> do np <- literal lit
                                 emit bci_PUSH_UBX [Op np, wOp nws]

  -- see Note [Generating code for top-level string literal bindings] in GHC.StgToByteCode
  PUSH_ADDR nm             -> do np <- lit [BCONPtrAddr nm]
                                 emit bci_PUSH_UBX [Op np, SmallOp 1]

  PUSH_APPLY_N             -> emit bci_PUSH_APPLY_N []
  PUSH_APPLY_V             -> emit bci_PUSH_APPLY_V []
  PUSH_APPLY_F             -> emit bci_PUSH_APPLY_F []
  PUSH_APPLY_D             -> emit bci_PUSH_APPLY_D []
  PUSH_APPLY_L             -> emit bci_PUSH_APPLY_L []
  PUSH_APPLY_P             -> emit bci_PUSH_APPLY_P []
  PUSH_APPLY_PP            -> emit bci_PUSH_APPLY_PP []
  PUSH_APPLY_PPP           -> emit bci_PUSH_APPLY_PPP []
  PUSH_APPLY_PPPP          -> emit bci_PUSH_APPLY_PPPP []
  PUSH_APPLY_PPPPP         -> emit bci_PUSH_APPLY_PPPPP []
  PUSH_APPLY_PPPPPP        -> emit bci_PUSH_APPLY_PPPPPP []

  SLIDE     n by           -> emit bci_SLIDE [wOp n, wOp by]
  ALLOC_AP  n              -> emit bci_ALLOC_AP [truncHalfWord platform n]
  ALLOC_AP_NOUPD n         -> emit bci_ALLOC_AP_NOUPD [truncHalfWord platform n]
  ALLOC_PAP arity n        -> emit bci_ALLOC_PAP [truncHalfWord platform arity, truncHalfWord platform n]
  MKAP      off sz         -> emit bci_MKAP [wOp off, truncHalfWord platform sz]
  MKPAP     off sz         -> emit bci_MKPAP [wOp off, truncHalfWord platform sz]
  UNPACK    n              -> emit bci_UNPACK [wOp n]
  PACK      dcon sz        -> do itbl_no <- lit [BCONPtrItbl (getName dcon)]
                                 emit bci_PACK [Op itbl_no, wOp sz]
  LABEL     lbl            -> label lbl
  TESTLT_I  i l            -> do np <- int i
                                 emit bci_TESTLT_I [Op np, LabelOp l]
  TESTEQ_I  i l            -> do np <- int i
                                 emit bci_TESTEQ_I [Op np, LabelOp l]
  TESTLT_W  w l            -> do np <- word w
                                 emit bci_TESTLT_W [Op np, LabelOp l]
  TESTEQ_W  w l            -> do np <- word w
                                 emit bci_TESTEQ_W [Op np, LabelOp l]
  TESTLT_I64  i l          -> do np <- word64 (fromIntegral i)
                                 emit bci_TESTLT_I64 [Op np, LabelOp l]
  TESTEQ_I64  i l          -> do np <- word64 (fromIntegral i)
                                 emit bci_TESTEQ_I64 [Op np, LabelOp l]
  TESTLT_I32  i l          -> do np <- word (fromIntegral i)
                                 emit bci_TESTLT_I32 [Op np, LabelOp l]
  TESTEQ_I32 i l           -> do np <- word (fromIntegral i)
                                 emit bci_TESTEQ_I32 [Op np, LabelOp l]
  TESTLT_I16  i l          -> do np <- word (fromIntegral i)
                                 emit bci_TESTLT_I16 [Op np, LabelOp l]
  TESTEQ_I16 i l           -> do np <- word (fromIntegral i)
                                 emit bci_TESTEQ_I16 [Op np, LabelOp l]
  TESTLT_I8  i l           -> do np <- word (fromIntegral i)
                                 emit bci_TESTLT_I8 [Op np, LabelOp l]
  TESTEQ_I8 i l            -> do np <- word (fromIntegral i)
                                 emit bci_TESTEQ_I8 [Op np, LabelOp l]
  TESTLT_W64  w l          -> do np <- word64 w
                                 emit bci_TESTLT_W64 [Op np, LabelOp l]
  TESTEQ_W64  w l          -> do np <- word64 w
                                 emit bci_TESTEQ_W64 [Op np, LabelOp l]
  TESTLT_W32  w l          -> do np <- word (fromIntegral w)
                                 emit bci_TESTLT_W32 [Op np, LabelOp l]
  TESTEQ_W32  w l          -> do np <- word (fromIntegral w)
                                 emit bci_TESTEQ_W32 [Op np, LabelOp l]
  TESTLT_W16  w l          -> do np <- word (fromIntegral w)
                                 emit bci_TESTLT_W16 [Op np, LabelOp l]
  TESTEQ_W16  w l          -> do np <- word (fromIntegral w)
                                 emit bci_TESTEQ_W16 [Op np, LabelOp l]
  TESTLT_W8  w l           -> do np <- word (fromIntegral w)
                                 emit bci_TESTLT_W8 [Op np, LabelOp l]
  TESTEQ_W8  w l           -> do np <- word (fromIntegral w)
                                 emit bci_TESTEQ_W8 [Op np, LabelOp l]
  TESTLT_F  f l            -> do np <- float f
                                 emit bci_TESTLT_F [Op np, LabelOp l]
  TESTEQ_F  f l            -> do np <- float f
                                 emit bci_TESTEQ_F [Op np, LabelOp l]
  TESTLT_D  d l            -> do np <- double d
                                 emit bci_TESTLT_D [Op np, LabelOp l]
  TESTEQ_D  d l            -> do np <- double d
                                 emit bci_TESTEQ_D [Op np, LabelOp l]
  TESTLT_P  i l            -> emit bci_TESTLT_P [SmallOp i, LabelOp l]
  TESTEQ_P  i l            -> emit bci_TESTEQ_P [SmallOp i, LabelOp l]
  CASEFAIL                 -> emit bci_CASEFAIL []
  SWIZZLE   stkoff n       -> emit bci_SWIZZLE [wOp stkoff, IOp n]
  JMP       l              -> emit bci_JMP [LabelOp l]
  ENTER                    -> emit bci_ENTER []
  RETURN rep               -> emit (return_non_tuple rep) []
  RETURN_TUPLE             -> emit bci_RETURN_T []
  CCALL off m_addr i       -> do np <- addr m_addr
                                 emit bci_CCALL [wOp off, Op np, SmallOp i]
  PRIMCALL                 -> emit bci_PRIMCALL []
  BRK_FUN arr index mod cc -> do p1 <- ptr (BCOPtrBreakArray arr)
                                 m <- addr mod
                                 np <- addr cc
                                 emit bci_BRK_FUN [Op p1, SmallOp index,
                                                   Op m, Op np]

  where
    literal (LitLabel fs (Just sz) _)
     | platformOS platform == OSMinGW32
         = litlabel (appendFS fs (mkFastString ('@':show sz)))
     -- On Windows, stdcall labels have a suffix indicating the no. of
     -- arg words, e.g. foo@8.  testcase: ffi012(ghci)
    literal (LitLabel fs _ _) = litlabel fs
    literal LitNullAddr       = word 0
    literal (LitFloat r)      = float (fromRational r)
    literal (LitDouble r)     = double (fromRational r)
    literal (LitChar c)       = int (ord c)
    literal (LitString bs)    = lit [BCONPtrStr bs]
       -- LitString requires a zero-terminator when emitted
    literal (LitNumber nt i) = case nt of
      LitNumInt     -> word (fromIntegral i)
      LitNumWord    -> word (fromIntegral i)
      LitNumInt8    -> word8 (fromIntegral i)
      LitNumWord8   -> word8 (fromIntegral i)
      LitNumInt16   -> word16 (fromIntegral i)
      LitNumWord16  -> word16 (fromIntegral i)
      LitNumInt32   -> word32 (fromIntegral i)
      LitNumWord32  -> word32 (fromIntegral i)
      LitNumInt64   -> word64 (fromIntegral i)
      LitNumWord64  -> word64 (fromIntegral i)
      LitNumBigNat  -> panic "GHC.ByteCode.Asm.literal: LitNumBigNat"

    -- We can lower 'LitRubbish' to an arbitrary constant, but @NULL@ is most
    -- likely to elicit a crash (rather than corrupt memory) in case absence
    -- analysis messed up.
    literal (LitRubbish {}) = word 0

    litlabel fs = lit [BCONPtrLbl fs]
    addr (RemotePtr a) = words [fromIntegral a]
    words ws = lit (map BCONPtrWord ws)
    word w = words [w]
    word_size  = platformWordSize platform
    word_size_bits = platformWordSizeInBits platform

    -- Make lists of host-sized words for literals, so that when the
    -- words are placed in memory at increasing addresses, the
    -- bit pattern is correct for the host's word size and endianness.
    --
    -- Note that we only support host endianness == target endianness for now,
    -- even with the external interpreter. This would need to be fixed to
    -- support host endianness /= target endianness
    int :: Int -> Assembler Word
    int  i = word (fromIntegral i)

    float :: Float -> Assembler Word
    float f = word32 (castFloatToWord32 f)

    double :: Double -> Assembler Word
    double d = word64 (castDoubleToWord64 d)

    word64 :: Word64 -> Assembler Word
    word64 ww = case word_size of
       PW4 ->
        let !wl = fromIntegral ww
            !wh = fromIntegral (ww `unsafeShiftR` 32)
        in case platformByteOrder platform of
            LittleEndian -> words [wl,wh]
            BigEndian    -> words [wh,wl]
       PW8 -> word (fromIntegral ww)

    word8 :: Word8 -> Assembler Word
    word8  x = case platformByteOrder platform of
      LittleEndian -> word (fromIntegral x)
      BigEndian    -> word (fromIntegral x `unsafeShiftL` (word_size_bits - 8))

    word16 :: Word16 -> Assembler Word
    word16 x = case platformByteOrder platform of
      LittleEndian -> word (fromIntegral x)
      BigEndian    -> word (fromIntegral x `unsafeShiftL` (word_size_bits - 16))

    word32 :: Word32 -> Assembler Word
    word32 x = case platformByteOrder platform of
      LittleEndian -> word (fromIntegral x)
      BigEndian    -> case word_size of
        PW4 -> word (fromIntegral x)
        PW8 -> word (fromIntegral x `unsafeShiftL` 32)


isLargeW :: Word -> Bool
isLargeW n = n > 65535

isLargeI :: Int -> Bool
isLargeI n = n > 32767 || n < -32768

push_alts :: ArgRep -> Word16
push_alts V   = bci_PUSH_ALTS_V
push_alts P   = bci_PUSH_ALTS_P
push_alts N   = bci_PUSH_ALTS_N
push_alts L   = bci_PUSH_ALTS_L
push_alts F   = bci_PUSH_ALTS_F
push_alts D   = bci_PUSH_ALTS_D
push_alts V16 = error "push_alts: vector"
push_alts V32 = error "push_alts: vector"
push_alts V64 = error "push_alts: vector"

return_non_tuple :: ArgRep -> Word16
return_non_tuple V   = bci_RETURN_V
return_non_tuple P   = bci_RETURN_P
return_non_tuple N   = bci_RETURN_N
return_non_tuple L   = bci_RETURN_L
return_non_tuple F   = bci_RETURN_F
return_non_tuple D   = bci_RETURN_D
return_non_tuple V16 = error "return_non_tuple: vector"
return_non_tuple V32 = error "return_non_tuple: vector"
return_non_tuple V64 = error "return_non_tuple: vector"

{-
  we can only handle up to a fixed number of words on the stack,
  because we need a stg_ctoi_tN stack frame for each size N. See
  Note [unboxed tuple bytecodes and tuple_BCO].

  If needed, you can support larger tuples by adding more in
  StgMiscClosures.cmm, Interpreter.c and MiscClosures.h and
  raising this limit.

  Note that the limit is the number of words passed on the stack.
  If the calling convention passes part of the tuple in registers, the
  maximum number of tuple elements may be larger. Elements can also
  take multiple words on the stack (for example Double# on a 32 bit
  platform).
 -}
maxTupleReturnNativeStackSize :: WordOff
maxTupleReturnNativeStackSize = 62

{-
  Construct the call_info word that stg_ctoi_t, stg_ret_t and stg_primcall
  use to convert arguments between the native calling convention and the
  interpreter.

  See Note [GHCi and native call registers] for more information.
 -}
mkNativeCallInfoSig :: Platform -> NativeCallInfo -> Word32
mkNativeCallInfoSig platform NativeCallInfo{..}
  | nativeCallType == NativeTupleReturn && nativeCallStackSpillSize > maxTupleReturnNativeStackSize
  = pprPanic "mkNativeCallInfoSig: tuple too big for the bytecode compiler"
             (ppr nativeCallStackSpillSize <+> text "stack words." <+>
              text "Use -fobject-code to get around this limit"
             )
  | otherwise
  = assertPpr (length regs <= 24) (text "too many registers for bitmap:" <+> ppr (length regs)) {- 24 bits for register bitmap -}
    assertPpr (cont_offset < 255) (text "continuation offset too large:" <+> ppr cont_offset) {- 8 bits for continuation offset (only for NativeTupleReturn) -}
    assertPpr (all (`elem` regs) (regSetToList nativeCallRegs)) (text "not all registers accounted for") {- all regs accounted for -}
    foldl' reg_bit 0 (zip regs [0..]) .|. (cont_offset `shiftL` 24)
  where
    cont_offset :: Word32
    cont_offset
      | nativeCallType == NativeTupleReturn = fromIntegral nativeCallStackSpillSize
      | otherwise                           = 0 -- there is no continuation for primcalls

    reg_bit :: Word32 -> (GlobalReg, Int) -> Word32
    reg_bit x (r, n)
      | r `elemRegSet` nativeCallRegs = x .|. 1 `shiftL` n
      | otherwise                     = x
    regs = allArgRegsCover platform

mkNativeCallInfoLit :: Platform -> NativeCallInfo -> Literal
mkNativeCallInfoLit platform call_info =
  mkLitWord platform . fromIntegral $ mkNativeCallInfoSig platform call_info

iNTERP_STACK_CHECK_THRESH :: Int
iNTERP_STACK_CHECK_THRESH = INTERP_STACK_CHECK_THRESH
