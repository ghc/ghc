%
% (c) The University of Glasgow 2000
%
\section[ByteCodeLink]{Bytecode assembler and linker}

\begin{code}

{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module ByteCodeLink ( UnlinkedBCO, UnlinkedBCOExpr, assembleBCO,
		      ClosureEnv, HValue, filterNameMap,
		      linkIModules, linkIExpr, linkFail,
		      iNTERP_STACK_CHECK_THRESH
		   ) where

#include "HsVersions.h"

import Outputable
import Name		( Name, getName, nameModule, toRdrName, isGlobalName )
import RdrName		( rdrNameOcc, rdrNameModule )
import OccName		( occNameString )
import FiniteMap	( FiniteMap, addListToFM, filterFM,
			  addToFM, lookupFM, emptyFM )
import CoreSyn
import Literal		( Literal(..) )
import PrimOp		( PrimOp, primOpOcc )
import PrimRep		( PrimRep(..) )
import Constants	( wORD_SIZE )
import Module		( ModuleName, moduleName, moduleNameFS )
import Linker		( lookupSymbol )
import FastString	( FastString(..) )
import ByteCodeInstr	( BCInstr(..), ProtoBCO(..) )
import ByteCodeItbls	( ItblEnv, ItblPtr )


import Monad		( when, foldM )
import ST		( runST )
import IArray		( array )
import MArray		( castSTUArray, 
			  newInt64Array, writeInt64Array,
			  newFloatArray, writeFloatArray,
			  newDoubleArray, writeDoubleArray,
			  newIntArray, writeIntArray,
			  newAddrArray, writeAddrArray,
			  readWordArray )
import Foreign		( Word16, Ptr(..), free )
import Addr 		( Word, Addr(..), nullAddr )
import Weak		( addFinalizer )
import FiniteMap

import PrelBase		( Int(..) )
import PrelGHC		( BCO#, newBCO#, unsafeCoerce#, 
			  ByteArray#, Array#, addrToHValue#, mkApUpd0# )
import IOExts		( fixIO )
import Exception        ( throwDyn )
import Panic            ( GhcException(..) )
import PrelArr		( Array(..) )
import ArrayBase	( UArray(..) )
import PrelIOBase	( IO(..) )
import Int		( Int64 )

\end{code}

%************************************************************************
%*									*
\subsection{Top-level stuff}
%*									*
%************************************************************************

\begin{code}
-- Linking stuff
linkIModules :: ItblEnv    -- incoming global itbl env; returned updated
	     -> ClosureEnv -- incoming global closure env; returned updated
	     -> [([UnlinkedBCO], ItblEnv)]
	     -> IO ([HValue], ItblEnv, ClosureEnv)
linkIModules gie gce mods 
   = do let (bcoss, ies) = unzip mods
            bcos         = concat bcoss
            final_gie    = foldr plusFM gie ies
        (final_gce, linked_bcos) <- linkSomeBCOs True final_gie gce bcos
        return (linked_bcos, final_gie, final_gce)


linkIExpr :: ItblEnv -> ClosureEnv -> UnlinkedBCOExpr
          -> IO HValue 	  -- IO BCO# really
linkIExpr ie ce (root_ul_bco, aux_ul_bcos)
   = do (aux_ce, _) <- linkSomeBCOs False ie ce aux_ul_bcos
        (_, [root_bco]) <- linkSomeBCOs False ie aux_ce [root_ul_bco]
        return root_bco

-- Link a bunch of BCOs and return them + updated closure env.
linkSomeBCOs :: Bool 	-- False <=> add _all_ BCOs to returned closure env
                        -- True  <=> add only toplevel BCOs to closure env
             -> ItblEnv 
             -> ClosureEnv 
             -> [UnlinkedBCO]
             -> IO (ClosureEnv, [HValue])
linkSomeBCOs toplevs_only ie ce_in ul_bcos
   = do let nms = map nameOfUnlinkedBCO ul_bcos
        hvals <- fixIO 
                    ( \ hvs -> let ce_out = addListToFM ce_in (zipLazily nms hvs)
                               in  mapM (linkBCO ie ce_out) ul_bcos )

        let ce_all_additions = zip nms hvals
            ce_top_additions = filter (isGlobalName.fst) ce_all_additions
            ce_additions     = if toplevs_only then ce_top_additions 
                                               else ce_all_additions
            ce_out = -- make sure we're not inserting duplicate names into the 
		     -- closure environment, which leads to trouble.
		     ASSERT (all (not . (`elemFM` ce_in)) (map fst ce_additions))
		     addListToFM ce_in ce_additions
        return (ce_out, hvals)
     where
        -- A lazier zip, in which no demand is propagated to the second
        -- list unless some demand is propagated to the snd of one of the
        -- result list elems.
        zipLazily []     ys = []
        zipLazily (x:xs) ys = (x, head ys) : zipLazily xs (tail ys)


data UnlinkedBCO
   = UnlinkedBCO Name
                 (SizedSeq Word16)		 -- insns
                 (SizedSeq Word)		 -- literals
                 (SizedSeq (Either Name PrimOp)) -- ptrs
                 (SizedSeq Name)		 -- itbl refs

nameOfUnlinkedBCO (UnlinkedBCO nm _ _ _ _) = nm

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


-- these need a proper home
type ClosureEnv = FiniteMap Name HValue
data HValue     = HValue  -- dummy type, actually a pointer to some Real Code.

-- remove all entries for a given set of modules from the environment;
-- note that this removes all local names too (ie. temporary bindings from
-- the command line).
filterNameMap :: [ModuleName] -> FiniteMap Name a -> FiniteMap Name a
filterNameMap mods env 
   = filterFM (\n _ -> isGlobalName n 
			&& moduleName (nameModule n) `elem` mods) env
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
         lits  <- return emptySS :: IO (SizedSeq Word)
         ptrs  <- return emptySS :: IO (SizedSeq (Either Name PrimOp))
         itbls <- return emptySS :: IO (SizedSeq Name)
         let init_asm_state = (insns,lits,ptrs,itbls)
         (final_insns, final_lits, final_ptrs, final_itbls) 
            <- mkBits findLabel init_asm_state instrs

         let ul_bco = UnlinkedBCO nm final_insns final_lits final_ptrs final_itbls

         -- 8 Aug 01: Finalisers aren't safe when attached to non-primitive
         -- objects, since they might get run too early.  Disable this until
         -- we figure out what to do.
         -- when (not (null malloced)) (addFinalizer ul_bco (mapM_ zonk malloced))

         return ul_bco
     where
         zonk (A# a#) = do -- putStrLn ("freeing malloc'd block at " ++ show (A# a#))
                           free (Ptr a#)

-- instrs nonptrs ptrs itbls
type AsmState = (SizedSeq Word16, SizedSeq Word, 
                 SizedSeq (Either Name PrimOp), SizedSeq Name)

data SizedSeq a = SizedSeq !Int [a]
emptySS = SizedSeq 0 []
addToSS (SizedSeq n r_xs) x = return (SizedSeq (n+1) (x:r_xs))
addListToSS (SizedSeq n r_xs) xs 
   = return (SizedSeq (n + length xs) (reverse xs ++ r_xs))
sizeSS (SizedSeq n r_xs) = n
listFromSS (SizedSeq n r_xs) = return (reverse r_xs)


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
               st_l1 <- addListToSS st_l0 ws
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       double (st_i0,st_l0,st_p0,st_I0) d
          = do let ws = mkLitD d
               st_l1 <- addListToSS st_l0 ws
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       int (st_i0,st_l0,st_p0,st_I0) i
          = do let ws = mkLitI i
               st_l1 <- addListToSS st_l0 ws
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       int64 (st_i0,st_l0,st_p0,st_I0) i
          = do let ws = mkLitI64 i
               st_l1 <- addListToSS st_l0 ws
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       addr (st_i0,st_l0,st_p0,st_I0) a
          = do let ws = mkLitA a
               st_l1 <- addListToSS st_l0 ws
               return (sizeSS st_l0, (st_i0,st_l1,st_p0,st_I0))

       ptr (st_i0,st_l0,st_p0,st_I0) p
          = do st_p1 <- addToSS st_p0 p
               return (sizeSS st_p0, (st_i0,st_l0,st_p1,st_I0))

       itbl (st_i0,st_l0,st_p0,st_I0) dcon
          = do st_I1 <- addToSS st_I0 (getName dcon)
               return (sizeSS st_I0, (st_i0,st_l0,st_p0,st_I1))

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
                       PtrRep    -> stg_ctoi_ret_R1p_info
                       WordRep   -> stg_ctoi_ret_R1n_info
                       IntRep    -> stg_ctoi_ret_R1n_info
                       AddrRep   -> stg_ctoi_ret_R1n_info
                       CharRep   -> stg_ctoi_ret_R1n_info
                       FloatRep  -> stg_ctoi_ret_F1_info
                       DoubleRep -> stg_ctoi_ret_D1_info
                       VoidRep   -> stg_ctoi_ret_V_info
                       other     -> pprPanic "ByteCodeLink.ctoi_itbl" (ppr pk)

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
                       VoidRep   -> nullAddr  
                       -- Interpreter.c spots this special case
                       other     -> pprPanic "ByteCodeLink.itoc_itbl" (ppr pk)
                     
foreign label "stg_ctoi_ret_R1p_info" stg_ctoi_ret_R1p_info :: Addr
foreign label "stg_ctoi_ret_R1n_info" stg_ctoi_ret_R1n_info :: Addr
foreign label "stg_ctoi_ret_F1_info"  stg_ctoi_ret_F1_info :: Addr
foreign label "stg_ctoi_ret_D1_info"  stg_ctoi_ret_D1_info :: Addr
foreign label "stg_ctoi_ret_V_info"   stg_ctoi_ret_V_info :: Addr

foreign label "stg_gc_unbx_r1_info" stg_gc_unbx_r1_info :: Addr
foreign label "stg_gc_f1_info"      stg_gc_f1_info :: Addr
foreign label "stg_gc_d1_info"      stg_gc_d1_info :: Addr

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
mkLitA   :: Addr   -> [Word]
mkLitI64 :: Int64  -> [Word]

mkLitF f
   = runST (do
        arr <- newFloatArray ((0::Int),0)
        writeFloatArray arr 0 f
        f_arr <- castSTUArray arr
        w0 <- readWordArray f_arr 0
        return [w0]
     )

mkLitD d
   | wORD_SIZE == 4
   = runST (do
        arr <- newDoubleArray ((0::Int),1)
        writeDoubleArray arr 0 d
        d_arr <- castSTUArray arr
        w0 <- readWordArray d_arr 0
        w1 <- readWordArray d_arr 1
        return [w0,w1]
     )
   | wORD_SIZE == 8
   = runST (do
        arr <- newDoubleArray ((0::Int),0)
        writeDoubleArray arr 0 d
        d_arr <- castSTUArray arr
        w0 <- readWordArray d_arr 0
        return [w0]
     )

mkLitI64 ii
   | wORD_SIZE == 4
   = runST (do
        arr <- newInt64Array ((0::Int),1)
        writeInt64Array arr 0 ii
        d_arr <- castSTUArray arr
        w0 <- readWordArray d_arr 0
        w1 <- readWordArray d_arr 1
        return [w0,w1]
     )
   | wORD_SIZE == 8
   = runST (do
        arr <- newInt64Array ((0::Int),0)
        writeInt64Array arr 0 ii
        d_arr <- castSTUArray arr
        w0 <- readWordArray d_arr 0
        return [w0]
     )

mkLitI i
   = runST (do
        arr <- newIntArray ((0::Int),0)
        writeIntArray arr 0 i
        i_arr <- castSTUArray arr
        w0 <- readWordArray i_arr 0
        return [w0]
     )

mkLitA a
   = runST (do
        arr <- newAddrArray ((0::Int),0)
        writeAddrArray arr 0 a
        a_arr <- castSTUArray arr
        w0 <- readWordArray a_arr 0
        return [w0]
     )

\end{code}

%************************************************************************
%*									*
\subsection{Linking interpretables into something we can run}
%*									*
%************************************************************************

\begin{code}

{- 
data BCO# = BCO# ByteArray# 		-- instrs   :: Array Word16#
                 ByteArray# 		-- literals :: Array Word32#
                 PtrArray# 		-- ptrs     :: Array HValue
                 ByteArray#		-- itbls    :: Array Addr#
-}

linkBCO ie ce (UnlinkedBCO nm insnsSS literalsSS ptrsSS itblsSS)
   = do insns    <- listFromSS insnsSS
        literals <- listFromSS literalsSS
        ptrs     <- listFromSS ptrsSS
        itbls    <- listFromSS itblsSS

        linked_ptrs  <- mapM (lookupCE ce) ptrs
        linked_itbls <- mapM (lookupIE ie) itbls

        let n_insns    = sizeSS insnsSS
            n_literals = sizeSS literalsSS
            n_ptrs     = sizeSS ptrsSS
            n_itbls    = sizeSS itblsSS

        let ptrs_arr = array (0, n_ptrs-1) (indexify linked_ptrs)
                       :: Array Int HValue
            ptrs_parr = case ptrs_arr of Array lo hi parr -> parr

            itbls_arr = array (0, n_itbls-1) (indexify linked_itbls)
                        :: UArray Int ItblPtr
            itbls_barr = case itbls_arr of UArray lo hi barr -> barr

            insns_arr | n_insns > 65535
                      = panic "linkBCO: >= 64k insns in BCO"
                      | otherwise 
                      = array (0, n_insns) 
                              (indexify (fromIntegral n_insns:insns))
                        :: UArray Int Word16
            insns_barr = case insns_arr of UArray lo hi barr -> barr

            literals_arr = array (0, n_literals-1) (indexify literals)
                           :: UArray Int Word
            literals_barr = case literals_arr of UArray lo hi barr -> barr

            indexify :: [a] -> [(Int, a)]
            indexify xs = zip [0..] xs

        BCO bco# <- newBCO insns_barr literals_barr ptrs_parr itbls_barr

        -- WAS: return (unsafeCoerce# bco#)
        case mkApUpd0# (unsafeCoerce# bco#) of
           (# final_bco #) -> return final_bco


data BCO = BCO BCO#

newBCO :: ByteArray# -> ByteArray# -> Array# a -> ByteArray# -> IO BCO
newBCO a b c d
   = IO (\s -> case newBCO# a b c d s of (# s1, bco #) -> (# s1, BCO bco #))


lookupCE :: ClosureEnv -> Either Name PrimOp -> IO HValue
lookupCE ce (Right primop)
   = do let sym_to_find = primopToCLabel primop "closure"
        m <- lookupSymbol sym_to_find
        case m of
           Just (Ptr addr) -> case addrToHValue# addr of
                                 (# hval #) -> return hval
           Nothing -> linkFail "ByteCodeLink.lookupCE(primop)" sym_to_find
lookupCE ce (Left nm)
   = case lookupFM ce nm of
        Just aa -> return aa
        Nothing 
           -> ASSERT2(isGlobalName nm, ppr nm)
	      do let sym_to_find = nameToCLabel nm "closure"
                 m <- lookupSymbol sym_to_find
                 case m of
                    Just (Ptr addr) -> case addrToHValue# addr of
                                          (# hval #) -> return hval
                    Nothing         -> linkFail "ByteCodeLink.lookupCE" sym_to_find

lookupIE :: ItblEnv -> Name -> IO (Ptr a)
lookupIE ie con_nm 
   = case lookupFM ie con_nm of
        Just (Ptr a) -> return (Ptr a)
        Nothing
           -> do -- try looking up in the object files.
                 let sym_to_find1 = nameToCLabel con_nm "con_info"
                 m <- lookupSymbol sym_to_find1
                 case m of
                    Just addr -> return addr
                    Nothing 
                       -> do -- perhaps a nullary constructor?
                             let sym_to_find2 = nameToCLabel con_nm "static_info"
                             n <- lookupSymbol sym_to_find2
                             case n of
                                Just addr -> return addr
                                Nothing   -> linkFail "ByteCodeLink.lookupIE" 
                                                (sym_to_find1 ++ " or " ++ sym_to_find2)

linkFail :: String -> String -> IO a
linkFail who what
   = throwDyn (ProgramError $
        unlines [ ""
	        , "During interactive linking, GHCi couldn't find the following symbol:"
		, ' ' : ' ' : what 
		, "This may be due to you not asking GHCi to load extra object files,"
		, "archives or DLLs needed by your current session.  Restart GHCi, specifying"
		, "the missing library using the -L/path/to/object/dir and -lmissinglibname"
		, "flags, or simply by naming the relevant files on the GHCi command line."
		, "Alternatively, this link failure might indicate a bug in GHCi."
		, "If you suspect the latter, please send a bug report to:"
		, "  glasgow-haskell-bugs@haskell.org"
		])

-- HACKS!!!  ToDo: cleaner
nameToCLabel :: Name -> String{-suffix-} -> String
nameToCLabel n suffix
   = _UNPK_(moduleNameFS (rdrNameModule rn)) 
     ++ '_':occNameString(rdrNameOcc rn) ++ '_':suffix
     where rn = toRdrName n

primopToCLabel :: PrimOp -> String{-suffix-} -> String
primopToCLabel primop suffix
   = let str = "PrelPrimopWrappers_" ++ occNameString (primOpOcc primop) ++ '_':suffix
     in --trace ("primopToCLabel: " ++ str)
        str

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
