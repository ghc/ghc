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
import Constants
import FastString
import SMRep
import ClosureInfo -- CgRep stuff
import DynFlags
import Outputable
import Platform

import Control.Monad    ( foldM )
import Control.Monad.ST ( runST )

import Data.Array.MArray
import Data.Array.Unboxed ( listArray )
import Data.Array.Base  ( UArray(..) )
import Data.Array.Unsafe( castSTUArray )

import Foreign
import Data.Char        ( ord )
import Data.List
import Data.Map (Map)
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
  = do  itblenv <- mkITbls tycons
        bcos    <- mapM (assembleBCO dflags) proto_bcos
        return (ByteCode bcos itblenv)

assembleBCO :: DynFlags -> ProtoBCO Name -> IO UnlinkedBCO
assembleBCO dflags (ProtoBCO nm instrs bitmap bsize arity _origin _malloced)
   = let
         -- pass 1: collect up the offsets of the local labels.
         -- Remember that the first insn starts at offset
         --     sizeOf Word / sizeOf Word16
         -- since offset 0 (eventually) will hold the total # of insns.
         lableInitialOffset
          | wORD_SIZE_IN_BITS == 64 = 4
          | wORD_SIZE_IN_BITS == 32 = 2
          | otherwise = error "wORD_SIZE_IN_BITS not 32 or 64?"
         label_env = mkLabelEnv Map.empty lableInitialOffset instrs

         -- Jump instructions are variable-sized, there are long and
         -- short variants depending on the magnitude of the offset.
         -- However, we can't tell what size instructions we will need
         -- until we have calculated the offsets of the labels, which
         -- depends on the size of the instructions...  We could
         -- repeat the calculation and hope to reach a fixpoint, but
         -- instead we just calculate the worst-case size and use that
         -- to decide whether *all* the jumps in this BCO will be long
         -- or short.

         -- True => all our jumps will be long
         large_bco = isLarge max_w16s
            where max_w16s = fromIntegral (length instrs) * maxInstr16s :: Word

         mkLabelEnv :: Map Word16 Word -> Word -> [BCInstr]
                    -> Map Word16 Word
         mkLabelEnv env _ [] = env
         mkLabelEnv env i_offset (i:is)
            = let new_env
                     = case i of LABEL n -> Map.insert n i_offset env ; _ -> env
              in  mkLabelEnv new_env (i_offset + instrSize16s i large_bco) is

         findLabel :: Word16 -> Word
         findLabel lab
            = case Map.lookup lab label_env of
                 Just bco_offset -> bco_offset
                 Nothing -> pprPanic "assembleBCO.findLabel" (ppr lab)
     in
     do  -- pass 2: generate the instruction, ptr and nonptr bits
         insns <- return emptySS :: IO (SizedSeq Word16)
         lits  <- return emptySS :: IO (SizedSeq BCONPtr)
         ptrs  <- return emptySS :: IO (SizedSeq BCOPtr)
         let init_asm_state = (insns,lits,ptrs)
         (final_insns, final_lits, final_ptrs)
            <- mkBits dflags large_bco findLabel init_asm_state instrs

         let asm_insns = ssElts final_insns
             n_insns   = sizeSS final_insns

             insns_arr = mkInstrArray lableInitialOffset n_insns asm_insns
             !insns_barr = case insns_arr of UArray _lo _hi _n barr -> barr

             bitmap_arr = mkBitmapArray bsize bitmap
             !bitmap_barr = case bitmap_arr of UArray _lo _hi _n barr -> barr

         let ul_bco = UnlinkedBCO nm arity insns_barr bitmap_barr final_lits final_ptrs

         -- 8 Aug 01: Finalisers aren't safe when attached to non-primitive
         -- objects, since they might get run too early.  Disable this until
         -- we figure out what to do.
         -- when (notNull malloced) (addFinalizer ul_bco (mapM_ zonk malloced))

         return ul_bco
     -- where
     --     zonk ptr = do -- putStrLn ("freeing malloc'd block at " ++ show (A# a#))
     --                      free ptr

mkBitmapArray :: Word16 -> [StgWord] -> UArray Int StgWord
mkBitmapArray bsize bitmap
  = listArray (0, length bitmap) (fromIntegral bsize : bitmap)

mkInstrArray :: Word -> Word -> [Word16] -> UArray Word Word16
mkInstrArray lableInitialOffset n_insns asm_insns
  = let size = lableInitialOffset + n_insns
    in listArray (0, size - 1) (largeArg size ++ asm_insns)

-- instrs nonptrs ptrs
type AsmState = (SizedSeq Word16,
                 SizedSeq BCONPtr,
                 SizedSeq BCOPtr)

data SizedSeq a = SizedSeq !Word [a]
emptySS :: SizedSeq a
emptySS = SizedSeq 0 []

-- Why are these two monadic???
addToSS :: SizedSeq a -> a -> IO (SizedSeq a)
addToSS (SizedSeq n r_xs) x = return (SizedSeq (n+1) (x:r_xs))
addListToSS :: SizedSeq a -> [a] -> IO (SizedSeq a)
addListToSS (SizedSeq n r_xs) xs
   = return (SizedSeq (n + genericLength xs) (reverse xs ++ r_xs))

ssElts :: SizedSeq a -> [a]
ssElts (SizedSeq _ r_xs) = reverse r_xs

sizeSS :: SizedSeq a -> Word
sizeSS (SizedSeq n _) = n

sizeSS16 :: SizedSeq a -> Word16
sizeSS16 (SizedSeq n _) = fromIntegral n

-- Bring in all the bci_ bytecode constants.
#include "rts/Bytecodes.h"

largeArgInstr :: Word16 -> Word16
largeArgInstr bci = bci_FLAG_LARGE_ARGS .|. bci

largeArg :: Word -> [Word16]
largeArg w
 | wORD_SIZE_IN_BITS == 64
           = [fromIntegral (w `shiftR` 48),
              fromIntegral (w `shiftR` 32),
              fromIntegral (w `shiftR` 16),
              fromIntegral w]
 | wORD_SIZE_IN_BITS == 32
           = [fromIntegral (w `shiftR` 16),
              fromIntegral w]
 | otherwise = error "wORD_SIZE_IN_BITS not 32 or 64?"

largeArg16s :: Word
largeArg16s | wORD_SIZE_IN_BITS == 64  = 4
            | otherwise                = 2

-- This is where all the action is (pass 2 of the assembler)
mkBits :: DynFlags
       -> Bool                          -- jumps are long
       -> (Word16 -> Word)              -- label finder
       -> AsmState
       -> [BCInstr]                     -- instructions (in)
       -> IO AsmState

mkBits dflags long_jumps findLabel st proto_insns
  = foldM doInstr st proto_insns
    where
       doInstr :: AsmState -> BCInstr -> IO AsmState
       doInstr st i
          = case i of
               STKCHECK  n
                  | isLarge n  -> instrn st (largeArgInstr bci_STKCHECK : largeArg n)
                  | otherwise  -> instr2 st bci_STKCHECK (fromIntegral n)

               PUSH_L    o1       -> instr2 st bci_PUSH_L o1
               PUSH_LL   o1 o2    -> instr3 st bci_PUSH_LL o1 o2
               PUSH_LLL  o1 o2 o3 -> instr4 st bci_PUSH_LLL o1 o2 o3
               PUSH_G    nm       -> do (p, st2) <- ptr st (BCOPtrName nm)
                                        instr2 st2 bci_PUSH_G p
               PUSH_PRIMOP op     -> do (p, st2) <- ptr st (BCOPtrPrimOp op)
                                        instr2 st2 bci_PUSH_G p
               PUSH_BCO proto     -> do ul_bco <- assembleBCO dflags proto
                                        (p, st2) <- ptr st (BCOPtrBCO ul_bco)
                                        instr2 st2 bci_PUSH_G p
               PUSH_ALTS proto    -> do ul_bco <- assembleBCO dflags proto
                                        (p, st2) <- ptr st (BCOPtrBCO ul_bco)
                                        instr2 st2 bci_PUSH_ALTS p
               PUSH_ALTS_UNLIFTED proto pk -> do
                                        ul_bco <- assembleBCO dflags proto
                                        (p, st2) <- ptr st (BCOPtrBCO ul_bco)
                                        instr2 st2 (push_alts pk) p
               PUSH_UBX  (Left lit) nws
                                  -> do (np, st2) <- literal st lit
                                        instr3 st2 bci_PUSH_UBX np nws
               PUSH_UBX  (Right aa) nws
                                  -> do (np, st2) <- addr st aa
                                        instr3 st2 bci_PUSH_UBX np nws

               PUSH_APPLY_N         -> do instr1 st bci_PUSH_APPLY_N
               PUSH_APPLY_V         -> do instr1 st bci_PUSH_APPLY_V
               PUSH_APPLY_F         -> do instr1 st bci_PUSH_APPLY_F
               PUSH_APPLY_D         -> do instr1 st bci_PUSH_APPLY_D
               PUSH_APPLY_L         -> do instr1 st bci_PUSH_APPLY_L
               PUSH_APPLY_P         -> do instr1 st bci_PUSH_APPLY_P
               PUSH_APPLY_PP        -> do instr1 st bci_PUSH_APPLY_PP
               PUSH_APPLY_PPP       -> do instr1 st bci_PUSH_APPLY_PPP
               PUSH_APPLY_PPPP      -> do instr1 st bci_PUSH_APPLY_PPPP
               PUSH_APPLY_PPPPP     -> do instr1 st bci_PUSH_APPLY_PPPPP
               PUSH_APPLY_PPPPPP    -> do instr1 st bci_PUSH_APPLY_PPPPPP

               SLIDE     n by     -> instr3 st bci_SLIDE n by
               ALLOC_AP  n        -> instr2 st bci_ALLOC_AP n
               ALLOC_AP_NOUPD n   -> instr2 st bci_ALLOC_AP_NOUPD n
               ALLOC_PAP arity n  -> instr3 st bci_ALLOC_PAP arity n
               MKAP      off sz   -> instr3 st bci_MKAP off sz
               MKPAP     off sz   -> instr3 st bci_MKPAP off sz
               UNPACK    n        -> instr2 st bci_UNPACK n
               PACK      dcon sz  -> do (itbl_no,st2) <- itbl st dcon
                                        instr3 st2 bci_PACK itbl_no sz
               LABEL     _        -> return st
               TESTLT_I  i l      -> do (np, st2) <- int st i
                                        jumpInstr2 st2 bci_TESTLT_I np (findLabel l)
               TESTEQ_I  i l      -> do (np, st2) <- int st i
                                        jumpInstr2 st2 bci_TESTEQ_I np (findLabel l)
               TESTLT_W  w l      -> do (np, st2) <- word st w
                                        jumpInstr2 st2 bci_TESTLT_W np (findLabel l)
               TESTEQ_W  w l      -> do (np, st2) <- word st w
                                        jumpInstr2 st2 bci_TESTEQ_W np (findLabel l)
               TESTLT_F  f l      -> do (np, st2) <- float st f
                                        jumpInstr2 st2 bci_TESTLT_F np (findLabel l)
               TESTEQ_F  f l      -> do (np, st2) <- float st f
                                        jumpInstr2 st2 bci_TESTEQ_F np (findLabel l)
               TESTLT_D  d l      -> do (np, st2) <- double st d
                                        jumpInstr2 st2 bci_TESTLT_D np (findLabel l)
               TESTEQ_D  d l      -> do (np, st2) <- double st d
                                        jumpInstr2 st2 bci_TESTEQ_D np (findLabel l)
               TESTLT_P  i l      -> jumpInstr2 st bci_TESTLT_P i (findLabel l)
               TESTEQ_P  i l      -> jumpInstr2 st bci_TESTEQ_P i (findLabel l)
               CASEFAIL           -> instr1 st bci_CASEFAIL
               SWIZZLE   stkoff n -> instr3 st bci_SWIZZLE stkoff n
               JMP       l        -> jumpInstr1 st bci_JMP (findLabel l)
               ENTER              -> instr1 st bci_ENTER
               RETURN             -> instr1 st bci_RETURN
               RETURN_UBX rep     -> instr1 st (return_ubx rep)
               CCALL off m_addr int -> do (np, st2) <- addr st m_addr
                                          instr4 st2 bci_CCALL off np int
               BRK_FUN array index info -> do
                  (p1, st2) <- ptr st  (BCOPtrArray array)
                  (p2, st3) <- ptr st2 (BCOPtrBreakInfo info)
                  instr4 st3 bci_BRK_FUN p1 index p2

       instrn :: AsmState -> [Word16] -> IO AsmState
       instrn st [] = return st
       instrn (st_i, st_l, st_p) (i:is)
          = do st_i' <- addToSS st_i i
               instrn (st_i', st_l, st_p) is

       jumpInstr1 st i1 i2
            | long_jumps = instrn st (largeArgInstr i1 : largeArg i2)
            | otherwise  = instr2 st i1 (fromIntegral i2)

       jumpInstr2 st i1 i2 i3
           | long_jumps = instrn st (largeArgInstr i1 : i2 : largeArg i3)
           | otherwise  = instr3 st i1 i2 (fromIntegral i3)

       instr1 (st_i0,st_l0,st_p0) i1
          = do st_i1 <- addToSS st_i0 i1
               return (st_i1,st_l0,st_p0)

       instr2 (st_i0,st_l0,st_p0) w1 w2
          = do st_i1 <- addToSS st_i0 w1
               st_i2 <- addToSS st_i1 w2
               return (st_i2,st_l0,st_p0)

       instr3 (st_i0,st_l0,st_p0) w1 w2 w3
          = do st_i1 <- addToSS st_i0 w1
               st_i2 <- addToSS st_i1 w2
               st_i3 <- addToSS st_i2 w3
               return (st_i3,st_l0,st_p0)

       instr4 (st_i0,st_l0,st_p0) w1 w2 w3 w4
          = do st_i1 <- addToSS st_i0 w1
               st_i2 <- addToSS st_i1 w2
               st_i3 <- addToSS st_i2 w3
               st_i4 <- addToSS st_i3 w4
               return (st_i4,st_l0,st_p0)

       float (st_i0,st_l0,st_p0) f
          = do let ws = mkLitF f
               st_l1 <- addListToSS st_l0 (map BCONPtrWord ws)
               return (sizeSS16 st_l0, (st_i0,st_l1,st_p0))

       double (st_i0,st_l0,st_p0) d
          = do let ws = mkLitD d
               st_l1 <- addListToSS st_l0 (map BCONPtrWord ws)
               return (sizeSS16 st_l0, (st_i0,st_l1,st_p0))

       int (st_i0,st_l0,st_p0) i
          = do let ws = mkLitI i
               st_l1 <- addListToSS st_l0 (map BCONPtrWord ws)
               return (sizeSS16 st_l0, (st_i0,st_l1,st_p0))

       word (st_i0,st_l0,st_p0) w
          = do let ws = [w]
               st_l1 <- addListToSS st_l0 (map BCONPtrWord ws)
               return (sizeSS16 st_l0, (st_i0,st_l1,st_p0))

       int64 (st_i0,st_l0,st_p0) i
          = do let ws = mkLitI64 i
               st_l1 <- addListToSS st_l0 (map BCONPtrWord ws)
               return (sizeSS16 st_l0, (st_i0,st_l1,st_p0))

       addr (st_i0,st_l0,st_p0) a
          = do let ws = mkLitPtr a
               st_l1 <- addListToSS st_l0 (map BCONPtrWord ws)
               return (sizeSS16 st_l0, (st_i0,st_l1,st_p0))

       litlabel (st_i0,st_l0,st_p0) fs
          = do st_l1 <- addListToSS st_l0 [BCONPtrLbl fs]
               return (sizeSS16 st_l0, (st_i0,st_l1,st_p0))

       ptr (st_i0,st_l0,st_p0) p
          = do st_p1 <- addToSS st_p0 p
               return (sizeSS16 st_p0, (st_i0,st_l0,st_p1))

       itbl (st_i0,st_l0,st_p0) dcon
          = do st_l1 <- addToSS st_l0 (BCONPtrItbl (getName dcon))
               return (sizeSS16 st_l0, (st_i0,st_l1,st_p0))

       literal st (MachLabel fs (Just sz) _)
        | platformOS (targetPlatform dflags) == OSMinGW32
            = litlabel st (appendFS fs (mkFastString ('@':show sz)))
        -- On Windows, stdcall labels have a suffix indicating the no. of
        -- arg words, e.g. foo@8.  testcase: ffi012(ghci)
       literal st (MachLabel fs _ _) = litlabel st fs
       literal st (MachWord w)     = int st (fromIntegral w)
       literal st (MachInt j)      = int st (fromIntegral j)
       literal st MachNullAddr     = int st 0
       literal st (MachFloat r)    = float st (fromRational r)
       literal st (MachDouble r)   = double st (fromRational r)
       literal st (MachChar c)     = int st (ord c)
       literal st (MachInt64 ii)   = int64 st (fromIntegral ii)
       literal st (MachWord64 ii)  = int64 st (fromIntegral ii)
       literal _  other            = pprPanic "ByteCodeAsm.literal" (ppr other)

isLarge :: Word -> Bool
isLarge n = n > 65535

push_alts :: CgRep -> Word16
push_alts NonPtrArg = bci_PUSH_ALTS_N
push_alts FloatArg  = bci_PUSH_ALTS_F
push_alts DoubleArg = bci_PUSH_ALTS_D
push_alts VoidArg   = bci_PUSH_ALTS_V
push_alts LongArg   = bci_PUSH_ALTS_L
push_alts PtrArg    = bci_PUSH_ALTS_P

return_ubx :: CgRep -> Word16
return_ubx NonPtrArg = bci_RETURN_N
return_ubx FloatArg  = bci_RETURN_F
return_ubx DoubleArg = bci_RETURN_D
return_ubx VoidArg   = bci_RETURN_V
return_ubx LongArg   = bci_RETURN_L
return_ubx PtrArg    = bci_RETURN_P


-- The size in 16-bit entities of an instruction.
instrSize16s :: BCInstr -> Bool -> Word
instrSize16s instr long_jumps
   = case instr of
        STKCHECK n              -> if isLarge n then 1 + largeArg16s else 2
        PUSH_L{}                -> 2
        PUSH_LL{}               -> 3
        PUSH_LLL{}              -> 4
        PUSH_G{}                -> 2
        PUSH_PRIMOP{}           -> 2
        PUSH_BCO{}              -> 2
        PUSH_ALTS{}             -> 2
        PUSH_ALTS_UNLIFTED{}    -> 2
        PUSH_UBX{}              -> 3
        PUSH_APPLY_N{}          -> 1
        PUSH_APPLY_V{}          -> 1
        PUSH_APPLY_F{}          -> 1
        PUSH_APPLY_D{}          -> 1
        PUSH_APPLY_L{}          -> 1
        PUSH_APPLY_P{}          -> 1
        PUSH_APPLY_PP{}         -> 1
        PUSH_APPLY_PPP{}        -> 1
        PUSH_APPLY_PPPP{}       -> 1
        PUSH_APPLY_PPPPP{}      -> 1
        PUSH_APPLY_PPPPPP{}     -> 1
        SLIDE{}                 -> 3
        ALLOC_AP{}              -> 2
        ALLOC_AP_NOUPD{}        -> 2
        ALLOC_PAP{}             -> 3
        MKAP{}                  -> 3
        MKPAP{}                 -> 3
        UNPACK{}                -> 2
        PACK{}                  -> 3
        LABEL{}                 -> 0    -- !!
        TESTLT_I{}              -> 2 + jump
        TESTEQ_I{}              -> 2 + jump
        TESTLT_W{}              -> 2 + jump
        TESTEQ_W{}              -> 2 + jump
        TESTLT_F{}              -> 2 + jump
        TESTEQ_F{}              -> 2 + jump
        TESTLT_D{}              -> 2 + jump
        TESTEQ_D{}              -> 2 + jump
        TESTLT_P{}              -> 2 + jump
        TESTEQ_P{}              -> 2 + jump
        JMP{}                   -> 1 + jump
        CASEFAIL{}              -> 1
        ENTER{}                 -> 1
        RETURN{}                -> 1
        RETURN_UBX{}            -> 1
        CCALL{}                 -> 4
        SWIZZLE{}               -> 3
        BRK_FUN{}               -> 4
  where
    jump | long_jumps = largeArg16s
         | otherwise  = 1

-- The biggest instruction in Word16s
maxInstr16s :: Word
maxInstr16s = 2 + largeArg16s -- LARGE TESTLT_I = 2 + largeArg16s

-- Make lists of host-sized words for literals, so that when the
-- words are placed in memory at increasing addresses, the
-- bit pattern is correct for the host's word size and endianness.
mkLitI   :: Int    -> [Word]
mkLitF   :: Float  -> [Word]
mkLitD   :: Double -> [Word]
mkLitPtr :: Ptr () -> [Word]
mkLitI64 :: Int64  -> [Word]

mkLitF f
   = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 f
        f_arr <- castSTUArray arr
        w0 <- readArray f_arr 0
        return [w0 :: Word]
     )

mkLitD d
   | wORD_SIZE == 4
   = runST (do
        arr <- newArray_ ((0::Int),1)
        writeArray arr 0 d
        d_arr <- castSTUArray arr
        w0 <- readArray d_arr 0
        w1 <- readArray d_arr 1
        return [w0 :: Word, w1]
     )
   | wORD_SIZE == 8
   = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 d
        d_arr <- castSTUArray arr
        w0 <- readArray d_arr 0
        return [w0 :: Word]
     )
   | otherwise
   = panic "mkLitD: Bad wORD_SIZE"

mkLitI64 ii
   | wORD_SIZE == 4
   = runST (do
        arr <- newArray_ ((0::Int),1)
        writeArray arr 0 ii
        d_arr <- castSTUArray arr
        w0 <- readArray d_arr 0
        w1 <- readArray d_arr 1
        return [w0 :: Word,w1]
     )
   | wORD_SIZE == 8
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
