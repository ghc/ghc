%
% (c) The University of Glasgow 2000
%
\section[ByteCodeLink]{Bytecode assembler and linker}

\begin{code}

{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module ByteCodeAsm (  
	assembleBCOs, assembleBCO,

	CompiledByteCode(..), 
	UnlinkedBCO(..), UnlinkedBCOExpr, nameOfUnlinkedBCO, bcosFreeNames,
	SizedSeq, sizeSS, ssElts,
	iNTERP_STACK_CHECK_THRESH
  ) where

#include "HsVersions.h"

import ByteCodeInstr	( BCInstr(..), ProtoBCO(..) )
import ByteCodeItbls	( ItblEnv, mkITbls )

import Name		( Name, getName )
import NameSet
import FiniteMap	( addToFM, lookupFM, emptyFM )
import CoreSyn
import Literal		( Literal(..) )
import TyCon		( TyCon )
import PrimOp		( PrimOp )
import PrimRep		( PrimRep(..), isFollowableRep )
import Constants	( wORD_SIZE )
import FastString	( FastString(..), unpackFS )
import FiniteMap
import Outputable

import Control.Monad	( foldM )
import Control.Monad.ST	( runST )

import GHC.Word		( Word(..) )
import Data.Array.MArray ( MArray, newArray_, readArray, writeArray )
import Data.Array.ST	( castSTUArray )
import Foreign.Ptr	( nullPtr )
import Foreign		( Word16, free )
import Data.Int		( Int64 )

#if __GLASGOW_HASKELL__ >= 503
import GHC.IOBase	( IO(..) )
import GHC.Ptr		( Ptr(..) )
#else
import PrelIOBase	( IO(..) )
import Ptr		( Ptr(..) )
#endif
\end{code}



%************************************************************************
%*									*
			Unlinked BCOs
%*									*
%************************************************************************

\begin{code}
-- CompiledByteCode represents the result of byte-code 
-- compiling a bunch of functions and data types

data CompiledByteCode 
  = ByteCode [UnlinkedBCO] -- Bunch of interpretable bindings
	     ItblEnv       -- A mapping from DataCons to their itbls

instance Outputable CompiledByteCode where
  ppr (ByteCode bcos _) = ppr bcos


data UnlinkedBCO
   = UnlinkedBCO Name
                 (SizedSeq Word16)		 -- insns
                 (SizedSeq (Either Word FastString))	 -- literals
			-- Either literal words or a pointer to a asciiz
			-- string, denoting a label whose *address* should
			-- be determined at link time
                 (SizedSeq (Either Name PrimOp)) -- ptrs
                 (SizedSeq Name)		 -- itbl refs

nameOfUnlinkedBCO (UnlinkedBCO nm _ _ _ _) = nm

bcosFreeNames :: [UnlinkedBCO] -> NameSet
-- Finds external references.  Remember to remove the names
-- defined by this group of BCOs themselves
bcosFreeNames bcos
  = free_names `minusNameSet` defined_names
  where
    defined_names = mkNameSet (map nameOfUnlinkedBCO bcos)
    free_names    = foldr (unionNameSets . bco_refs) emptyNameSet bcos

    bco_refs (UnlinkedBCO _ _ _ ptrs itbls)
	= mkNameSet [n | Left n <- ssElts ptrs] `unionNameSets`
	  mkNameSet (ssElts itbls)

-- When translating expressions, we need to distinguish the root
-- BCO for the expression
type UnlinkedBCOExpr = (UnlinkedBCO, [UnlinkedBCO])

instance Outputable UnlinkedBCO where
   ppr (UnlinkedBCO nm insns lits ptrs itbls)
      = sep [text "BCO", ppr nm, text "with", 
             int (sizeSS insns), text "insns",
             int (sizeSS lits), text "lits",
             int (sizeSS ptrs), text "ptrs",
             int (sizeSS itbls), text "itbls"]
\end{code}


%************************************************************************
%*									*
\subsection{The bytecode assembler}
%*									*
%************************************************************************

The object format for bytecodes is: 16 bits for the opcode, and 16 for
each field -- so the code can be considered a sequence of 16-bit ints.
Each field denotes either a stack offset or number of items on the
stack (eg SLIDE), and index into the pointer table (eg PUSH_G), an
index into the literal table (eg PUSH_I/D/L), or a bytecode address in
this BCO.

\begin{code}
-- Top level assembler fn.
assembleBCOs :: [ProtoBCO Name] -> [TyCon] -> IO CompiledByteCode
assembleBCOs proto_bcos tycons
  = do	itblenv <- mkITbls tycons
	bcos    <- mapM assembleBCO proto_bcos
        return (ByteCode bcos itblenv)

assembleBCO :: ProtoBCO Name -> IO UnlinkedBCO

assembleBCO (ProtoBCO nm instrs origin malloced)
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
         ptrs  <- return emptySS :: IO (SizedSeq (Either Name PrimOp))
         itbls <- return emptySS :: IO (SizedSeq Name)
         let init_asm_state = (insns,lits,ptrs,itbls)
         (final_insns, final_lits, final_ptrs, final_itbls) 
            <- mkBits findLabel init_asm_state instrs

         let ul_bco = UnlinkedBCO nm final_insns final_lits final_ptrs final_itbls

         -- 8 Aug 01: Finalisers aren't safe when attached to non-primitive
         -- objects, since they might get run too early.  Disable this until
         -- we figure out what to do.
         -- when (notNull malloced) (addFinalizer ul_bco (mapM_ zonk malloced))

         return ul_bco
     where
         zonk ptr = do -- putStrLn ("freeing malloc'd block at " ++ show (A# a#))
                           free ptr

-- instrs nonptrs ptrs itbls
type AsmState = (SizedSeq Word16, 
                 SizedSeq (Either Word FastString),
                 SizedSeq (Either Name PrimOp), 
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
               SWIZZLE   stkoff n -> instr3 st i_SWIZZLE stkoff n
               ARGCHECK  n        -> instr2 st i_ARGCHECK n
               STKCHECK  n        -> instr2 st i_STKCHECK n
               PUSH_L    o1       -> instr2 st i_PUSH_L o1
               PUSH_LL   o1 o2    -> instr3 st i_PUSH_LL o1 o2
               PUSH_LLL  o1 o2 o3 -> instr4 st i_PUSH_LLL o1 o2 o3
               PUSH_G    nm       -> do (p, st2) <- ptr st nm
                                        instr2 st2 i_PUSH_G p
               PUSH_AS   nm pk    -> do (p, st2)  <- ptr st (Left nm)
                                        (np, st3) <- ctoi_itbl st2 pk
                                        instr3 st3 i_PUSH_AS p np
               PUSH_UBX  (Left lit) nws  
                                  -> do (np, st2) <- literal st lit
                                        instr3 st2 i_PUSH_UBX np nws
               PUSH_UBX  (Right aa) nws  
                                  -> do (np, st2) <- addr st aa
                                        instr3 st2 i_PUSH_UBX np nws

               PUSH_TAG  tag      -> instr2 st i_PUSH_TAG tag
               SLIDE     n by     -> instr3 st i_SLIDE n by
               ALLOC     n        -> instr2 st i_ALLOC n
               MKAP      off sz   -> instr3 st i_MKAP off sz
               UNPACK    n        -> instr2 st i_UNPACK n
               UPK_TAG   n m k    -> instr4 st i_UPK_TAG n m k
               PACK      dcon sz  -> do (itbl_no,st2) <- itbl st dcon
                                        instr3 st2 i_PACK itbl_no sz
               LABEL     lab      -> return st
               TESTLT_I  i l      -> do (np, st2) <- int st i
                                        instr3 st2 i_TESTLT_I np (findLabel l)
               TESTEQ_I  i l      -> do (np, st2) <- int st i
                                        instr3 st2 i_TESTEQ_I np (findLabel l)
               TESTLT_F  f l      -> do (np, st2) <- float st f
                                        instr3 st2 i_TESTLT_F np (findLabel l)
               TESTEQ_F  f l      -> do (np, st2) <- float st f
                                        instr3 st2 i_TESTEQ_F np (findLabel l)
               TESTLT_D  d l      -> do (np, st2) <- double st d
                                        instr3 st2 i_TESTLT_D np (findLabel l)
               TESTEQ_D  d l      -> do (np, st2) <- double st d
                                        instr3 st2 i_TESTEQ_D np (findLabel l)
               TESTLT_P  i l      -> instr3 st i_TESTLT_P i (findLabel l)
               TESTEQ_P  i l      -> instr3 st i_TESTEQ_P i (findLabel l)
               CASEFAIL           -> instr1 st i_CASEFAIL
               JMP       l        -> instr2 st i_JMP (findLabel l)
               ENTER              -> instr1 st i_ENTER
               RETURN    rep      -> do (itbl_no,st2) <- itoc_itbl st rep
                                        instr2 st2 i_RETURN itbl_no
               CCALL     m_addr   -> do (np, st2) <- addr st m_addr
                                        instr2 st2 i_CCALL np

       i2s :: Int -> Word16
       i2s = fromIntegral

       instr1 (st_i0,st_l0,st_p0,st_I0) i1
          = do st_i1 <- addToSS st_i0 (i2s i1)
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

       literal st (MachLabel fs)  = litlabel st fs
       literal st (MachWord w)    = int st (fromIntegral w)
       literal st (MachInt j)     = int st (fromIntegral j)
       literal st (MachFloat r)   = float st (fromRational r)
       literal st (MachDouble r)  = double st (fromRational r)
       literal st (MachChar c)    = int st c
       literal st (MachInt64 ii)  = int64 st (fromIntegral ii)
       literal st (MachWord64 ii) = int64 st (fromIntegral ii)
       literal st other           = pprPanic "ByteCodeLink.literal" (ppr other)

       ctoi_itbl st pk
          = addr st ret_itbl_addr
            where
               ret_itbl_addr 
                  = case pk of
                       WordRep   -> stg_ctoi_ret_R1n_info
                       IntRep    -> stg_ctoi_ret_R1n_info
                       AddrRep   -> stg_ctoi_ret_R1n_info
                       CharRep   -> stg_ctoi_ret_R1n_info
                       FloatRep  -> stg_ctoi_ret_F1_info
                       DoubleRep -> stg_ctoi_ret_D1_info
                       VoidRep   -> stg_ctoi_ret_V_info
                       other | isFollowableRep pk -> stg_ctoi_ret_R1p_info
				-- Includes ArrayRep, ByteArrayRep, as well as
				-- the obvious PtrRep
			     | otherwise
			     -> pprPanic "ByteCodeLink.ctoi_itbl" (ppr pk)

       itoc_itbl st pk
          = addr st ret_itbl_addr
            where
               ret_itbl_addr 
                  = case pk of
                       CharRep   -> stg_gc_unbx_r1_info
                       IntRep    -> stg_gc_unbx_r1_info
                       WordRep   -> stg_gc_unbx_r1_info
                       AddrRep   -> stg_gc_unbx_r1_info
                       FloatRep  -> stg_gc_f1_info
                       DoubleRep -> stg_gc_d1_info
                       VoidRep   -> nullPtr	-- Interpreter.c spots this special case
                       other | isFollowableRep pk -> stg_gc_unpt_r1_info
			     | otherwise
			    -> pprPanic "ByteCodeLink.itoc_itbl" (ppr pk)
                     
foreign label "stg_ctoi_ret_R1p_info" stg_ctoi_ret_R1p_info :: Ptr ()
foreign label "stg_ctoi_ret_R1n_info" stg_ctoi_ret_R1n_info :: Ptr ()
foreign label "stg_ctoi_ret_F1_info"  stg_ctoi_ret_F1_info :: Ptr ()
foreign label "stg_ctoi_ret_D1_info"  stg_ctoi_ret_D1_info :: Ptr ()
foreign label "stg_ctoi_ret_V_info"   stg_ctoi_ret_V_info :: Ptr ()

foreign label "stg_gc_unbx_r1_info" stg_gc_unbx_r1_info :: Ptr ()
foreign label "stg_gc_unpt_r1_info" stg_gc_unpt_r1_info :: Ptr ()
foreign label "stg_gc_f1_info"      stg_gc_f1_info :: Ptr ()
foreign label "stg_gc_d1_info"      stg_gc_d1_info :: Ptr ()

-- The size in 16-bit entities of an instruction.
instrSize16s :: BCInstr -> Int
instrSize16s instr
   = case instr of
        STKCHECK _     -> 2
        ARGCHECK _     -> 2
        PUSH_L   _     -> 2
        PUSH_LL  _ _   -> 3
        PUSH_LLL _ _ _ -> 4
        PUSH_G   _     -> 2
        PUSH_AS  _ _   -> 3
        PUSH_UBX _ _   -> 3
        PUSH_TAG _     -> 2
        SLIDE    _ _   -> 3
        ALLOC    _     -> 2
        MKAP     _ _   -> 3
        UNPACK   _     -> 2
        UPK_TAG  _ _ _ -> 4
        PACK     _ _   -> 3
        LABEL    _     -> 0	-- !!
        TESTLT_I _ _   -> 3
        TESTEQ_I _ _   -> 3
        TESTLT_F _ _   -> 3
        TESTEQ_F _ _   -> 3
        TESTLT_D _ _   -> 3
        TESTEQ_D _ _   -> 3
        TESTLT_P _ _   -> 3
        TESTEQ_P _ _   -> 3
        JMP      _     -> 2
        CASEFAIL       -> 1
        ENTER          -> 1
        RETURN   _     -> 2


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
\end{code}

%************************************************************************
%*									*
\subsection{Connect to actual values for bytecode opcodes}
%*									*
%************************************************************************

\begin{code}

#include "Bytecodes.h"

i_ARGCHECK = (bci_ARGCHECK :: Int)
i_PUSH_L   = (bci_PUSH_L :: Int)
i_PUSH_LL  = (bci_PUSH_LL :: Int)
i_PUSH_LLL = (bci_PUSH_LLL :: Int)
i_PUSH_G   = (bci_PUSH_G :: Int)
i_PUSH_AS  = (bci_PUSH_AS :: Int)
i_PUSH_UBX = (bci_PUSH_UBX :: Int)
i_PUSH_TAG = (bci_PUSH_TAG :: Int)
i_SLIDE    = (bci_SLIDE :: Int)
i_ALLOC    = (bci_ALLOC :: Int)
i_MKAP     = (bci_MKAP :: Int)
i_UNPACK   = (bci_UNPACK :: Int)
i_UPK_TAG  = (bci_UPK_TAG :: Int)
i_PACK     = (bci_PACK :: Int)
i_TESTLT_I = (bci_TESTLT_I :: Int)
i_TESTEQ_I = (bci_TESTEQ_I :: Int)
i_TESTLT_F = (bci_TESTLT_F :: Int)
i_TESTEQ_F = (bci_TESTEQ_F :: Int)
i_TESTLT_D = (bci_TESTLT_D :: Int)
i_TESTEQ_D = (bci_TESTEQ_D :: Int)
i_TESTLT_P = (bci_TESTLT_P :: Int)
i_TESTEQ_P = (bci_TESTEQ_P :: Int)
i_CASEFAIL = (bci_CASEFAIL :: Int)
i_ENTER    = (bci_ENTER :: Int)
i_RETURN   = (bci_RETURN :: Int)
i_STKCHECK = (bci_STKCHECK :: Int)
i_JMP      = (bci_JMP :: Int)
#ifdef bci_CCALL
i_CCALL    = (bci_CCALL :: Int)
i_SWIZZLE  = (bci_SWIZZLE :: Int)
#else
i_CCALL    = error "Sorry pal, you need to bootstrap to use i_CCALL."
i_SWIZZLE  = error "Sorry pal, you need to bootstrap to use i_SWIZZLE."
#endif

iNTERP_STACK_CHECK_THRESH = (INTERP_STACK_CHECK_THRESH :: Int)
\end{code}

