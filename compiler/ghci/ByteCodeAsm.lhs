%
% (c) The University of Glasgow 2002
%
\section[ByteCodeLink]{Bytecode assembler and linker}

\begin{code}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module ByteCodeAsm (  
	assembleBCOs, assembleBCO,

	CompiledByteCode(..), 
	UnlinkedBCO(..), BCOPtr(..), bcoFreeNames,
	SizedSeq, sizeSS, ssElts,
	iNTERP_STACK_CHECK_THRESH
  ) where

#include "HsVersions.h"

import ByteCodeInstr
import ByteCodeItbls	( ItblEnv, mkITbls )

import Name		( Name, getName )
import NameSet
import FiniteMap	( addToFM, lookupFM, emptyFM )
import Literal		( Literal(..) )
import TyCon		( TyCon )
import PrimOp		( PrimOp )
import Constants	( wORD_SIZE )
import FastString	( FastString(..) )
import SMRep		( CgRep(..), StgWord )
import FiniteMap
import Outputable

import Control.Monad	( foldM )
import Control.Monad.ST	( runST )

import GHC.Word		( Word(..) )
import Data.Array.MArray
import Data.Array.Unboxed ( listArray )
import Data.Array.Base	( UArray(..) )
import Data.Array.ST	( castSTUArray )
import Foreign		( Word16, free )
import Data.Int		( Int64 )
import Data.Char	( ord )

import GHC.Base		( ByteArray# )
import GHC.IOBase	( IO(..) )
import GHC.Ptr		( Ptr(..) )

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
	unlinkedBCOInstrs :: ByteArray#,			 -- insns
	unlinkedBCOBitmap :: ByteArray#,			 -- bitmap
        unlinkedBCOLits   :: (SizedSeq (Either Word FastString)), -- literals
			-- Either literal words or a pointer to a asciiz
			-- string, denoting a label whose *address* should
			-- be determined at link time
        unlinkedBCOPtrs   :: (SizedSeq BCOPtr), 	-- ptrs
 	unlinkedBCOItbls  :: (SizedSeq Name)	 	-- itbl refs
   }

data BCOPtr
  = BCOPtrName   Name
  | BCOPtrPrimOp PrimOp
  | BCOPtrBCO    UnlinkedBCO

-- | Finds external references.  Remember to remove the names
-- defined by this group of BCOs themselves
bcoFreeNames :: UnlinkedBCO -> NameSet
bcoFreeNames bco
  = bco_refs bco `minusNameSet` mkNameSet [unlinkedBCOName bco]
  where
    bco_refs (UnlinkedBCO _ _ _ _ _ ptrs itbls)
	= unionManyNameSets (
	     mkNameSet [ n | BCOPtrName n <- ssElts ptrs ] :
	     mkNameSet (ssElts itbls) :
	     map bco_refs [ bco | BCOPtrBCO bco <- ssElts ptrs ]
	  )

instance Outputable UnlinkedBCO where
   ppr (UnlinkedBCO nm arity insns bitmap lits ptrs itbls)
      = sep [text "BCO", ppr nm, text "with", 
             int (sizeSS lits), text "lits",
             int (sizeSS ptrs), text "ptrs",
             int (sizeSS itbls), text "itbls"]

-- -----------------------------------------------------------------------------
-- The bytecode assembler

-- The object format for bytecodes is: 16 bits for the opcode, and 16
-- for each field -- so the code can be considered a sequence of
-- 16-bit ints.  Each field denotes either a stack offset or number of
-- items on the stack (eg SLIDE), and index into the pointer table (eg
-- PUSH_G), an index into the literal table (eg PUSH_I/D/L), or a
-- bytecode address in this BCO.

-- Top level assembler fn.
assembleBCOs :: [ProtoBCO Name] -> [TyCon] -> IO CompiledByteCode
assembleBCOs proto_bcos tycons
  = do	itblenv <- mkITbls tycons
	bcos    <- mapM assembleBCO proto_bcos
        return (ByteCode bcos itblenv)

assembleBCO :: ProtoBCO Name -> IO UnlinkedBCO
assembleBCO (ProtoBCO nm instrs bitmap bsize arity origin malloced)
   = let
         -- pass 1: collect up the offsets of the local labels.
         -- Remember that the first insn starts at offset 1 since offset 0
         -- (eventually) will hold the total # of insns.
         label_env = mkLabelEnv emptyFM 1 instrs

         mkLabelEnv env i_offset [] = env
         mkLabelEnv env i_offset (i:is)
            = let new_env 
                     = case i of LABEL n -> addToFM env n i_offset ; _ -> env
              in  mkLabelEnv new_env (i_offset + instrSize16s i) is

         findLabel lab
            = case lookupFM label_env lab of
                 Just bco_offset -> bco_offset
                 Nothing -> pprPanic "assembleBCO.findLabel" (int lab)
     in
     do  -- pass 2: generate the instruction, ptr and nonptr bits
         insns <- return emptySS :: IO (SizedSeq Word16)
         lits  <- return emptySS :: IO (SizedSeq (Either Word FastString))
         ptrs  <- return emptySS :: IO (SizedSeq BCOPtr)
         itbls <- return emptySS :: IO (SizedSeq Name)
         let init_asm_state = (insns,lits,ptrs,itbls)
         (final_insns, final_lits, final_ptrs, final_itbls) 
            <- mkBits findLabel init_asm_state instrs

	 let asm_insns = ssElts final_insns
	     n_insns   = sizeSS final_insns

             insns_arr
		 | n_insns > 65535 = panic "linkBCO: >= 64k insns in BCO"
                 | otherwise = mkInstrArray n_insns asm_insns
             insns_barr = case insns_arr of UArray _lo _hi barr -> barr

	     bitmap_arr = mkBitmapArray bsize bitmap
             bitmap_barr = case bitmap_arr of UArray _lo _hi barr -> barr

         let ul_bco = UnlinkedBCO nm arity insns_barr bitmap_barr final_lits 
					final_ptrs final_itbls

         -- 8 Aug 01: Finalisers aren't safe when attached to non-primitive
         -- objects, since they might get run too early.  Disable this until
         -- we figure out what to do.
         -- when (notNull malloced) (addFinalizer ul_bco (mapM_ zonk malloced))

         return ul_bco
     where
         zonk ptr = do -- putStrLn ("freeing malloc'd block at " ++ show (A# a#))
                           free ptr

mkBitmapArray :: Int -> [StgWord] -> UArray Int StgWord
mkBitmapArray bsize bitmap
  = listArray (0, length bitmap) (fromIntegral bsize : bitmap)

mkInstrArray :: Int -> [Word16]	-> UArray Int Word16
mkInstrArray n_insns asm_insns
  = listArray (0, n_insns) (fromIntegral n_insns : asm_insns)

-- instrs nonptrs ptrs itbls
type AsmState = (SizedSeq Word16, 
                 SizedSeq (Either Word FastString),
                 SizedSeq BCOPtr, 
                 SizedSeq Name)

data SizedSeq a = SizedSeq !Int [a]
emptySS = SizedSeq 0 []

-- Why are these two monadic???
addToSS (SizedSeq n r_xs) x = return (SizedSeq (n+1) (x:r_xs))
addListToSS (SizedSeq n r_xs) xs 
   = return (SizedSeq (n + length xs) (reverse xs ++ r_xs))

ssElts :: SizedSeq a -> [a]
ssElts (SizedSeq n r_xs) = reverse r_xs

sizeSS :: SizedSeq a -> Int
sizeSS (SizedSeq n r_xs) = n

-- Bring in all the bci_ bytecode constants.
#include "Bytecodes.h"

-- This is where all the action is (pass 2 of the assembler)
mkBits :: (Int -> Int) 			-- label finder
       -> AsmState
       -> [BCInstr]			-- instructions (in)
       -> IO AsmState

mkBits findLabel st proto_insns
  = foldM doInstr st proto_insns
    where
       doInstr :: AsmState -> BCInstr -> IO AsmState
       doInstr st i
          = case i of
               STKCHECK  n        -> instr2 st bci_STKCHECK n
               PUSH_L    o1       -> instr2 st bci_PUSH_L o1
               PUSH_LL   o1 o2    -> instr3 st bci_PUSH_LL o1 o2
               PUSH_LLL  o1 o2 o3 -> instr4 st bci_PUSH_LLL o1 o2 o3
               PUSH_G    nm       -> do (p, st2) <- ptr st (BCOPtrName nm)
                                        instr2 st2 bci_PUSH_G p
               PUSH_PRIMOP op     -> do (p, st2) <- ptr st (BCOPtrPrimOp op)
                                        instr2 st2 bci_PUSH_G p
               PUSH_BCO proto     -> do ul_bco <- assembleBCO proto
 					(p, st2) <- ptr st (BCOPtrBCO ul_bco)
                                        instr2 st2 bci_PUSH_G p
               PUSH_ALTS proto    -> do ul_bco <- assembleBCO proto
 					(p, st2) <- ptr st (BCOPtrBCO ul_bco)
                                        instr2 st2 bci_PUSH_ALTS p
               PUSH_ALTS_UNLIFTED proto pk -> do 
					ul_bco <- assembleBCO proto
 					(p, st2) <- ptr st (BCOPtrBCO ul_bco)
                                        instr2 st2 (push_alts pk) p
               PUSH_UBX  (Left lit) nws  
                                  -> do (np, st2) <- literal st lit
                                        instr3 st2 bci_PUSH_UBX np nws
               PUSH_UBX  (Right aa) nws  
                                  -> do (np, st2) <- addr st aa
                                        instr3 st2 bci_PUSH_UBX np nws

	       PUSH_APPLY_N	    -> do instr1 st bci_PUSH_APPLY_N
	       PUSH_APPLY_V	    -> do instr1 st bci_PUSH_APPLY_V
	       PUSH_APPLY_F	    -> do instr1 st bci_PUSH_APPLY_F
	       PUSH_APPLY_D	    -> do instr1 st bci_PUSH_APPLY_D
	       PUSH_APPLY_L	    -> do instr1 st bci_PUSH_APPLY_L
	       PUSH_APPLY_P	    -> do instr1 st bci_PUSH_APPLY_P
	       PUSH_APPLY_PP	    -> do instr1 st bci_PUSH_APPLY_PP
	       PUSH_APPLY_PPP	    -> do instr1 st bci_PUSH_APPLY_PPP
	       PUSH_APPLY_PPPP	    -> do instr1 st bci_PUSH_APPLY_PPPP
	       PUSH_APPLY_PPPPP	    -> do instr1 st bci_PUSH_APPLY_PPPPP
	       PUSH_APPLY_PPPPPP    -> do instr1 st bci_PUSH_APPLY_PPPPPP

               SLIDE     n by     -> instr3 st bci_SLIDE n by
               ALLOC_AP  n        -> instr2 st bci_ALLOC_AP n
               ALLOC_PAP arity n  -> instr3 st bci_ALLOC_PAP arity n
               MKAP      off sz   -> instr3 st bci_MKAP off sz
               MKPAP     off sz   -> instr3 st bci_MKPAP off sz
               UNPACK    n        -> instr2 st bci_UNPACK n
               PACK      dcon sz  -> do (itbl_no,st2) <- itbl st dcon
                                        instr3 st2 bci_PACK itbl_no sz
               LABEL     lab      -> return st
               TESTLT_I  i l      -> do (np, st2) <- int st i
                                        instr3 st2 bci_TESTLT_I np (findLabel l)
               TESTEQ_I  i l      -> do (np, st2) <- int st i
                                        instr3 st2 bci_TESTEQ_I np (findLabel l)
               TESTLT_F  f l      -> do (np, st2) <- float st f
                                        instr3 st2 bci_TESTLT_F np (findLabel l)
               TESTEQ_F  f l      -> do (np, st2) <- float st f
                                        instr3 st2 bci_TESTEQ_F np (findLabel l)
               TESTLT_D  d l      -> do (np, st2) <- double st d
                                        instr3 st2 bci_TESTLT_D np (findLabel l)
               TESTEQ_D  d l      -> do (np, st2) <- double st d
                                        instr3 st2 bci_TESTEQ_D np (findLabel l)
               TESTLT_P  i l      -> instr3 st bci_TESTLT_P i (findLabel l)
               TESTEQ_P  i l      -> instr3 st bci_TESTEQ_P i (findLabel l)
               CASEFAIL           -> instr1 st bci_CASEFAIL
               SWIZZLE   stkoff n -> instr3 st bci_SWIZZLE stkoff n
               JMP       l        -> instr2 st bci_JMP (findLabel l)
               ENTER              -> instr1 st bci_ENTER
               RETURN             -> instr1 st bci_RETURN
               RETURN_UBX rep     -> instr1 st (return_ubx rep)
               CCALL off m_addr   -> do (np, st2) <- addr st m_addr
                                        instr3 st2 bci_CCALL off np

       i2s :: Int -> Word16
       i2s = fromIntegral

       instr1 (st_i0,st_l0,st_p0,st_I0) i1
          = do st_i1 <- addToSS st_i0 i1
               return (st_i1,st_l0,st_p0,st_I0)

       instr2 (st_i0,st_l0,st_p0,st_I0) i1 i2
          = do st_i1 <- addToSS st_i0 (i2s i1)
               st_i2 <- addToSS st_i1 (i2s i2)
               return (st_i2,st_l0,st_p0,st_I0)

       instr3 (st_i0,st_l0,st_p0,st_I0) i1 i2 i3
          = do st_i1 <- addToSS st_i0 (i2s i1)
               st_i2 <- addToSS st_i1 (i2s i2)
               st_i3 <- addToSS st_i2 (i2s i3)
               return (st_i3,st_l0,st_p0,st_I0)

       instr4 (st_i0,st_l0,st_p0,st_I0) i1 i2 i3 i4
          = do st_i1 <- addToSS st_i0 (i2s i1)
               st_i2 <- addToSS st_i1 (i2s i2)
               st_i3 <- addToSS st_i2 (i2s i3)
               st_i4 <- addToSS st_i3 (i2s i4)
               return (st_i4,st_l0,st_p0,st_I0)

       float (st_i0,st_l0,st_p0,st_I0) f
          = do let ws = mkLitF f
               st_l1 <- addListToSS st_l0 (map Left ws)
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       double (st_i0,st_l0,st_p0,st_I0) d
          = do let ws = mkLitD d
               st_l1 <- addListToSS st_l0 (map Left ws)
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       int (st_i0,st_l0,st_p0,st_I0) i
          = do let ws = mkLitI i
               st_l1 <- addListToSS st_l0 (map Left ws)
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       int64 (st_i0,st_l0,st_p0,st_I0) i
          = do let ws = mkLitI64 i
               st_l1 <- addListToSS st_l0 (map Left ws)
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       addr (st_i0,st_l0,st_p0,st_I0) a
          = do let ws = mkLitPtr a
               st_l1 <- addListToSS st_l0 (map Left ws)
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       litlabel (st_i0,st_l0,st_p0,st_I0) fs
          = do st_l1 <- addListToSS st_l0 [Right fs]
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       ptr (st_i0,st_l0,st_p0,st_I0) p
          = do st_p1 <- addToSS st_p0 p
               return (sizeSS st_p0, (st_i0,st_l0,st_p1,st_I0))

       itbl (st_i0,st_l0,st_p0,st_I0) dcon
          = do st_I1 <- addToSS st_I0 (getName dcon)
               return (sizeSS st_I0, (st_i0,st_l0,st_p0,st_I1))

       literal st (MachLabel fs _) = litlabel st fs
       literal st (MachWord w)     = int st (fromIntegral w)
       literal st (MachInt j)      = int st (fromIntegral j)
       literal st (MachFloat r)    = float st (fromRational r)
       literal st (MachDouble r)   = double st (fromRational r)
       literal st (MachChar c)     = int st (ord c)
       literal st (MachInt64 ii)   = int64 st (fromIntegral ii)
       literal st (MachWord64 ii)  = int64 st (fromIntegral ii)
       literal st other            = pprPanic "ByteCodeLink.literal" (ppr other)


push_alts NonPtrArg = bci_PUSH_ALTS_N
push_alts FloatArg  = bci_PUSH_ALTS_F
push_alts DoubleArg = bci_PUSH_ALTS_D
push_alts VoidArg   = bci_PUSH_ALTS_V
push_alts LongArg   = bci_PUSH_ALTS_L
push_alts PtrArg    = bci_PUSH_ALTS_P

return_ubx NonPtrArg = bci_RETURN_N
return_ubx FloatArg  = bci_RETURN_F
return_ubx DoubleArg = bci_RETURN_D
return_ubx VoidArg   = bci_RETURN_V
return_ubx LongArg   = bci_RETURN_L
return_ubx PtrArg    = bci_RETURN_P


-- The size in 16-bit entities of an instruction.
instrSize16s :: BCInstr -> Int
instrSize16s instr
   = case instr of
        STKCHECK{}		-> 2
        PUSH_L{}		-> 2
        PUSH_LL{}		-> 3
        PUSH_LLL{}		-> 4
        PUSH_G{}		-> 2
        PUSH_PRIMOP{}		-> 2
        PUSH_BCO{}		-> 2
        PUSH_ALTS{}		-> 2
        PUSH_ALTS_UNLIFTED{}	-> 2
	PUSH_UBX{}		-> 3
	PUSH_APPLY_N{}		-> 1
	PUSH_APPLY_V{}		-> 1
	PUSH_APPLY_F{}		-> 1
	PUSH_APPLY_D{}		-> 1
	PUSH_APPLY_L{}		-> 1
	PUSH_APPLY_P{}		-> 1
	PUSH_APPLY_PP{}		-> 1
	PUSH_APPLY_PPP{}	-> 1
	PUSH_APPLY_PPPP{}	-> 1
	PUSH_APPLY_PPPPP{}	-> 1
	PUSH_APPLY_PPPPPP{}	-> 1
        SLIDE{}			-> 3
        ALLOC_AP{}		-> 2
        ALLOC_PAP{}		-> 3
        MKAP{}			-> 3
        MKPAP{}			-> 3
        UNPACK{}		-> 2
        PACK{}			-> 3
        LABEL{}			-> 0	-- !!
        TESTLT_I{}		-> 3
        TESTEQ_I{}		-> 3
        TESTLT_F{}		-> 3
        TESTEQ_F{}		-> 3
        TESTLT_D{}		-> 3
        TESTEQ_D{}		-> 3
        TESTLT_P{}		-> 3
        TESTEQ_P{}		-> 3
        JMP{}			-> 2
        CASEFAIL{}		-> 1
        ENTER{}			-> 1
        RETURN{}		-> 1
        RETURN_UBX{}		-> 1
	CCALL{}			-> 3
        SWIZZLE{}		-> 3

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

iNTERP_STACK_CHECK_THRESH = (INTERP_STACK_CHECK_THRESH :: Int)
\end{code}
