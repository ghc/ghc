%
% (c) The University of Glasgow 2002-2006
%

ByteCodeLink: Bytecode assembler and linker

\begin{code}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}
{-# LANGUAGE BangPatterns #-}

module ByteCodeAsm (
        assembleBCOs, assembleBCO,

        CompiledByteCode(..),
        UnlinkedBCO(..), BCOPtr(..), BCONPtr(..), bcoFreeNames,
        SizedSeq, sizeSS, ssElts,
        iNTERP_STACK_CHECK_THRESH
  ) where

#include "HsVersions.h"

import ByteCodeInstr
import ByteCodeItbls

import Name
import NameSet
import Literal
import TyCon
import PrimOp
import FastString
import StgCmmLayout     ( ArgRep(..) )
import SMRep
import DynFlags
import Outputable
import Platform
import Util

import Control.Applicative (Applicative(..))
import Control.Monad
import Control.Monad.ST ( runST )
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Array.MArray

import qualified Data.Array.Unboxed as Array
import Data.Array.Base  ( UArray(..) )

import Data.Array.Unsafe( castSTUArray )

import Foreign
import Data.Char        ( ord )
import Data.List
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import GHC.Base         ( ByteArray#, MutableByteArray#, RealWorld )

-- -----------------------------------------------------------------------------
-- Unlinked BCOs

-- CompiledByteCode represents the result of byte-code
-- compiling a bunch of functions and data types

data CompiledByteCode
  = ByteCode [UnlinkedBCO] -- Bunch of interpretable bindings
             ItblEnv       -- A mapping from DataCons to their itbls

instance Outputable CompiledByteCode where
  ppr (ByteCode bcos _) = ppr bcos


data UnlinkedBCO
   = UnlinkedBCO {
        unlinkedBCOName   :: Name,
        unlinkedBCOArity  :: Int,
        unlinkedBCOInstrs :: ByteArray#,                 -- insns
        unlinkedBCOBitmap :: ByteArray#,                 -- bitmap
        unlinkedBCOLits   :: (SizedSeq BCONPtr),        -- non-ptrs
        unlinkedBCOPtrs   :: (SizedSeq BCOPtr)          -- ptrs
   }

data BCOPtr
  = BCOPtrName   Name
  | BCOPtrPrimOp PrimOp
  | BCOPtrBCO    UnlinkedBCO
  | BCOPtrBreakInfo  BreakInfo
  | BCOPtrArray (MutableByteArray# RealWorld)

data BCONPtr
  = BCONPtrWord  Word
  | BCONPtrLbl   FastString
  | BCONPtrItbl  Name

-- | Finds external references.  Remember to remove the names
-- defined by this group of BCOs themselves
bcoFreeNames :: UnlinkedBCO -> NameSet
bcoFreeNames bco
  = bco_refs bco `minusNameSet` mkNameSet [unlinkedBCOName bco]
  where
    bco_refs (UnlinkedBCO _ _ _ _ nonptrs ptrs)
        = unionManyNameSets (
             mkNameSet [ n | BCOPtrName n <- ssElts ptrs ] :
             mkNameSet [ n | BCONPtrItbl n <- ssElts nonptrs ] :
             map bco_refs [ bco | BCOPtrBCO bco <- ssElts ptrs ]
          )

instance Outputable UnlinkedBCO where
   ppr (UnlinkedBCO nm _arity _insns _bitmap lits ptrs)
      = sep [text "BCO", ppr nm, text "with",
             ppr (sizeSS lits), text "lits",
             ppr (sizeSS ptrs), text "ptrs" ]

-- -----------------------------------------------------------------------------
-- The bytecode assembler

-- The object format for bytecodes is: 16 bits for the opcode, and 16
-- for each field -- so the code can be considered a sequence of
-- 16-bit ints.  Each field denotes either a stack offset or number of
-- items on the stack (eg SLIDE), and index into the pointer table (eg
-- PUSH_G), an index into the literal table (eg PUSH_I/D/L), or a
-- bytecode address in this BCO.

-- Top level assembler fn.
assembleBCOs :: DynFlags -> [ProtoBCO Name] -> [TyCon] -> IO CompiledByteCode
assembleBCOs dflags proto_bcos tycons
  = do  itblenv <- mkITbls dflags tycons
        bcos    <- mapM (assembleBCO dflags) proto_bcos
        return (ByteCode bcos itblenv)

assembleBCO :: DynFlags -> ProtoBCO Name -> IO UnlinkedBCO
assembleBCO dflags (ProtoBCO nm instrs bitmap bsize arity _origin _malloced) = do
  -- pass 1: collect up the offsets of the local labels.
  let asm = mapM_ (assembleI dflags) instrs

      initial_offset = 0

      -- Jump instructions are variable-sized, there are long and short variants
      -- depending on the magnitude of the offset.  However, we can't tell what
      -- size instructions we will need until we have calculated the offsets of
      -- the labels, which depends on the size of the instructions...  So we
      -- first create the label environment assuming that all jumps are short,
      -- and if the final size is indeed small enough for short jumps, we are
      -- done.  Otherwise, we repeat the calculation, and we force all jumps in
      -- this BCO to be long.
      (n_insns0, lbl_map0) = inspectAsm dflags False initial_offset asm
      ((n_insns, lbl_map), long_jumps)
        | isLarge n_insns0 = (inspectAsm dflags True initial_offset asm, True)
        | otherwise = ((n_insns0, lbl_map0), False)

      env :: Word16 -> Word
      env lbl = fromMaybe
        (pprPanic "assembleBCO.findLabel" (ppr lbl))
        (Map.lookup lbl lbl_map)

  -- pass 2: run assembler and generate instructions, literals and pointers
  let initial_state = (emptySS, emptySS, emptySS)
  (final_insns, final_lits, final_ptrs) <- flip execStateT initial_state $ runAsm dflags long_jumps env asm

  -- precomputed size should be equal to final size
  ASSERT(n_insns == sizeSS final_insns) return ()

  let asm_insns = ssElts final_insns
      barr a = case a of UArray _lo _hi _n b -> b

      insns_arr = Array.listArray (0, n_insns - 1) asm_insns
      !insns_barr = barr insns_arr

      bitmap_arr = mkBitmapArray bsize bitmap
      !bitmap_barr = barr bitmap_arr

      ul_bco = UnlinkedBCO nm arity insns_barr bitmap_barr final_lits final_ptrs

  -- 8 Aug 01: Finalisers aren't safe when attached to non-primitive
  -- objects, since they might get run too early.  Disable this until
  -- we figure out what to do.
  -- when (notNull malloced) (addFinalizer ul_bco (mapM_ zonk malloced))

  return ul_bco

mkBitmapArray :: Word16 -> [StgWord] -> UArray Int Word
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

data SizedSeq a = SizedSeq !Word [a]
emptySS :: SizedSeq a
emptySS = SizedSeq 0 []

addToSS :: SizedSeq a -> a -> SizedSeq a
addToSS (SizedSeq n r_xs) x = SizedSeq (n+1) (x:r_xs)

addListToSS :: SizedSeq a -> [a] -> SizedSeq a
addListToSS (SizedSeq n r_xs) xs
  = SizedSeq (n + genericLength xs) (reverse xs ++ r_xs)

ssElts :: SizedSeq a -> [a]
ssElts (SizedSeq _ r_xs) = reverse r_xs

sizeSS :: SizedSeq a -> Word
sizeSS (SizedSeq n _) = n

data Operand
  = Op Word
  | SmallOp Word16
  | LabelOp Word16
-- (unused)  | LargeOp Word

data Assembler a
  = AllocPtr (IO BCOPtr) (Word -> Assembler a)
  | AllocLit [BCONPtr] (Word -> Assembler a)
  | AllocLabel Word16 (Assembler a)
  | Emit Word16 [Operand] (Assembler a)
  | NullAsm a

instance Functor Assembler where
    fmap = liftM

instance Applicative Assembler where
    pure = return
    (<*>) = ap

instance Monad Assembler where
  return = NullAsm
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

label :: Word16 -> Assembler ()
label w = AllocLabel w (return ())

emit :: Word16 -> [Operand] -> Assembler ()
emit w ops = Emit w ops (return ())

type LabelEnv = Word16 -> Word

largeOp :: Bool -> Operand -> Bool
largeOp long_jumps op = case op of
   SmallOp _ -> False
   Op w      -> isLarge w
   LabelOp _ -> long_jumps
-- LargeOp _ -> True

runAsm :: DynFlags -> Bool -> LabelEnv -> Assembler a -> StateT AsmState IO a
runAsm dflags long_jumps e = go
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
      let largeOps = any (largeOp long_jumps) ops
          opcode
            | largeOps = largeArgInstr w
            | otherwise = w
          words = concatMap expand ops
          expand (SmallOp w) = [w]
          expand (LabelOp w) = expand (Op (e w))
          expand (Op w) = if largeOps then largeArg dflags w else [fromIntegral w]
--        expand (LargeOp w) = largeArg dflags w
      state $ \(st_i0,st_l0,st_p0) ->
        let st_i1 = addListToSS st_i0 (opcode : words)
        in ((), (st_i1,st_l0,st_p0))
      go k

type LabelEnvMap = Map Word16 Word

data InspectState = InspectState
  { instrCount :: !Word
  , ptrCount :: !Word
  , litCount :: !Word
  , lblEnv :: LabelEnvMap
  }

inspectAsm :: DynFlags -> Bool -> Word -> Assembler a -> (Word, LabelEnvMap)
inspectAsm dflags long_jumps initial_offset
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
        count (Op _) = if largeOps then largeArg16s dflags else 1
--      count (LargeOp _) = largeArg16s dflags

-- Bring in all the bci_ bytecode constants.
#include "rts/Bytecodes.h"

largeArgInstr :: Word16 -> Word16
largeArgInstr bci = bci_FLAG_LARGE_ARGS .|. bci

largeArg :: DynFlags -> Word -> [Word16]
largeArg dflags w
 | wORD_SIZE_IN_BITS dflags == 64
           = [fromIntegral (w `shiftR` 48),
              fromIntegral (w `shiftR` 32),
              fromIntegral (w `shiftR` 16),
              fromIntegral w]
 | wORD_SIZE_IN_BITS dflags == 32
           = [fromIntegral (w `shiftR` 16),
              fromIntegral w]
 | otherwise = error "wORD_SIZE_IN_BITS not 32 or 64?"

largeArg16s :: DynFlags -> Word
largeArg16s dflags | wORD_SIZE_IN_BITS dflags == 64 = 4
                   | otherwise                      = 2

assembleI :: DynFlags
          -> BCInstr
          -> Assembler ()
assembleI dflags i = case i of
  STKCHECK n               -> emit bci_STKCHECK [Op n]
  PUSH_L o1                -> emit bci_PUSH_L [SmallOp o1]
  PUSH_LL o1 o2            -> emit bci_PUSH_LL [SmallOp o1, SmallOp o2]
  PUSH_LLL o1 o2 o3        -> emit bci_PUSH_LLL [SmallOp o1, SmallOp o2, SmallOp o3]
  PUSH_G nm                -> do p <- ptr (BCOPtrName nm)
                                 emit bci_PUSH_G [Op p]
  PUSH_PRIMOP op           -> do p <- ptr (BCOPtrPrimOp op)
                                 emit bci_PUSH_G [Op p]
  PUSH_BCO proto           -> do let ul_bco = assembleBCO dflags proto
                                 p <- ioptr (liftM BCOPtrBCO ul_bco)
                                 emit bci_PUSH_G [Op p]
  PUSH_ALTS proto          -> do let ul_bco = assembleBCO dflags proto
                                 p <- ioptr (liftM BCOPtrBCO ul_bco)
                                 emit bci_PUSH_ALTS [Op p]
  PUSH_ALTS_UNLIFTED proto pk
                           -> do let ul_bco = assembleBCO dflags proto
                                 p <- ioptr (liftM BCOPtrBCO ul_bco)
                                 emit (push_alts pk) [Op p]
  PUSH_UBX (Left lit) nws  -> do np <- literal lit
                                 emit bci_PUSH_UBX [Op np, SmallOp nws]
  PUSH_UBX (Right aa) nws  -> do np <- addr aa
                                 emit bci_PUSH_UBX [Op np, SmallOp nws]

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

  SLIDE     n by           -> emit bci_SLIDE [SmallOp n, SmallOp by]
  ALLOC_AP  n              -> emit bci_ALLOC_AP [SmallOp n]
  ALLOC_AP_NOUPD n         -> emit bci_ALLOC_AP_NOUPD [SmallOp n]
  ALLOC_PAP arity n        -> emit bci_ALLOC_PAP [SmallOp arity, SmallOp n]
  MKAP      off sz         -> emit bci_MKAP [SmallOp off, SmallOp sz]
  MKPAP     off sz         -> emit bci_MKPAP [SmallOp off, SmallOp sz]
  UNPACK    n              -> emit bci_UNPACK [SmallOp n]
  PACK      dcon sz        -> do itbl_no <- lit [BCONPtrItbl (getName dcon)]
                                 emit bci_PACK [Op itbl_no, SmallOp sz]
  LABEL     lbl            -> label lbl
  TESTLT_I  i l            -> do np <- int i
                                 emit bci_TESTLT_I [Op np, LabelOp l]
  TESTEQ_I  i l            -> do np <- int i
                                 emit bci_TESTEQ_I [Op np, LabelOp l]
  TESTLT_W  w l            -> do np <- word w
                                 emit bci_TESTLT_W [Op np, LabelOp l]
  TESTEQ_W  w l            -> do np <- word w
                                 emit bci_TESTEQ_W [Op np, LabelOp l]
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
  SWIZZLE   stkoff n       -> emit bci_SWIZZLE [SmallOp stkoff, SmallOp n]
  JMP       l              -> emit bci_JMP [LabelOp l]
  ENTER                    -> emit bci_ENTER []
  RETURN                   -> emit bci_RETURN []
  RETURN_UBX rep           -> emit (return_ubx rep) []
  CCALL off m_addr i       -> do np <- addr m_addr
                                 emit bci_CCALL [SmallOp off, Op np, SmallOp i]
  BRK_FUN array index info -> do p1 <- ptr (BCOPtrArray array)
                                 p2 <- ptr (BCOPtrBreakInfo info)
                                 emit bci_BRK_FUN [Op p1, SmallOp index, Op p2]

  where
    literal (MachLabel fs (Just sz) _)
     | platformOS (targetPlatform dflags) == OSMinGW32
         = litlabel (appendFS fs (mkFastString ('@':show sz)))
     -- On Windows, stdcall labels have a suffix indicating the no. of
     -- arg words, e.g. foo@8.  testcase: ffi012(ghci)
    literal (MachLabel fs _ _) = litlabel fs
    literal (MachWord w)       = int (fromIntegral w)
    literal (MachInt j)        = int (fromIntegral j)
    literal MachNullAddr       = int 0
    literal (MachFloat r)      = float (fromRational r)
    literal (MachDouble r)     = double (fromRational r)
    literal (MachChar c)       = int (ord c)
    literal (MachInt64 ii)     = int64 (fromIntegral ii)
    literal (MachWord64 ii)    = int64 (fromIntegral ii)
    literal other              = pprPanic "ByteCodeAsm.literal" (ppr other)

    litlabel fs = lit [BCONPtrLbl fs]
    addr = words . mkLitPtr
    float = words . mkLitF
    double = words . mkLitD dflags
    int = words . mkLitI
    int64 = words . mkLitI64 dflags
    words ws = lit (map BCONPtrWord ws)
    word w = words [w]

isLarge :: Word -> Bool
isLarge n = n > 65535

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

return_ubx :: ArgRep -> Word16
return_ubx V   = bci_RETURN_V
return_ubx P   = bci_RETURN_P
return_ubx N   = bci_RETURN_N
return_ubx L   = bci_RETURN_L
return_ubx F   = bci_RETURN_F
return_ubx D   = bci_RETURN_D
return_ubx V16 = error "return_ubx: vector"
return_ubx V32 = error "return_ubx: vector"
return_ubx V64 = error "return_ubx: vector"

-- Make lists of host-sized words for literals, so that when the
-- words are placed in memory at increasing addresses, the
-- bit pattern is correct for the host's word size and endianness.
mkLitI   ::             Int    -> [Word]
mkLitF   ::             Float  -> [Word]
mkLitD   :: DynFlags -> Double -> [Word]
mkLitPtr ::             Ptr () -> [Word]
mkLitI64 :: DynFlags -> Int64  -> [Word]

mkLitF f
   = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 f
        f_arr <- castSTUArray arr
        w0 <- readArray f_arr 0
        return [w0 :: Word]
     )

mkLitD dflags d
   | wORD_SIZE dflags == 4
   = runST (do
        arr <- newArray_ ((0::Int),1)
        writeArray arr 0 d
        d_arr <- castSTUArray arr
        w0 <- readArray d_arr 0
        w1 <- readArray d_arr 1
        return [w0 :: Word, w1]
     )
   | wORD_SIZE dflags == 8
   = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 d
        d_arr <- castSTUArray arr
        w0 <- readArray d_arr 0
        return [w0 :: Word]
     )
   | otherwise
   = panic "mkLitD: Bad wORD_SIZE"

mkLitI64 dflags ii
   | wORD_SIZE dflags == 4
   = runST (do
        arr <- newArray_ ((0::Int),1)
        writeArray arr 0 ii
        d_arr <- castSTUArray arr
        w0 <- readArray d_arr 0
        w1 <- readArray d_arr 1
        return [w0 :: Word,w1]
     )
   | wORD_SIZE dflags == 8
   = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 ii
        d_arr <- castSTUArray arr
        w0 <- readArray d_arr 0
        return [w0 :: Word]
     )
   | otherwise
   = panic "mkLitI64: Bad wORD_SIZE"

mkLitI i
   = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 i
        i_arr <- castSTUArray arr
        w0 <- readArray i_arr 0
        return [w0 :: Word]
     )

mkLitPtr a
   = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 a
        a_arr <- castSTUArray arr
        w0 <- readArray a_arr 0
        return [w0 :: Word]
     )

iNTERP_STACK_CHECK_THRESH :: Int
iNTERP_STACK_CHECK_THRESH = INTERP_STACK_CHECK_THRESH
\end{code}
