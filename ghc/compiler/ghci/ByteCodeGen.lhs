%
% (c) The University of Glasgow 2000
%
\section[ByteCodeGen]{Generate bytecode from Core}

\begin{code}
module ByteCodeGen ( byteCodeGen, assembleBCO ) where

#include "HsVersions.h"

import Outputable
import Name		( Name, getName )
import Id		( Id, idType, isDataConId_maybe )
import OrdList		( OrdList, consOL, snocOL, appOL, unitOL, 
			  nilOL, toOL, concatOL, fromOL )
import FiniteMap	( FiniteMap, addListToFM, listToFM, 
			  addToFM, lookupFM, fmToList, emptyFM )
import CoreSyn
import Literal		( Literal(..) )
import PrimRep		( PrimRep(..) )
import CoreFVs		( freeVars )
import Type		( typePrimRep )
import DataCon		( DataCon, dataConTag, fIRST_TAG )
import Util		( zipEqual, zipWith4Equal, naturalMergeSortLe )
import VarSet		( VarSet, varSetElems )
import PrimRep		( getPrimRepSize, isFollowableRep )
import Constants	( wORD_SIZE )

import Foreign		( Addr, Word16, Word32, nullAddr )
import ST		( runST )
import MutableArray	( readWord32Array,
			  newFloatArray, writeFloatArray,
			  newDoubleArray, writeDoubleArray,
			  newIntArray, writeIntArray,
			  newAddrArray, writeAddrArray )
\end{code}

Entry point.

\begin{code}
byteCodeGen :: [CoreBind] -> [ProtoBCO Name]
byteCodeGen binds
   = let flatBinds = concatMap getBind binds
         getBind (NonRec bndr rhs) = [(bndr, freeVars rhs)]
         getBind (Rec binds)       = [(bndr, freeVars rhs) | (bndr,rhs) <- binds]
         final_state = runBc (BcM_State [] 0) 
                             (mapBc schemeR flatBinds `thenBc_` returnBc ())
     in  
         case final_state of
            BcM_State bcos final_ctr -> bcos
\end{code}


%************************************************************************
%*									*
\subsection{Bytecodes, and Outputery.}
%*									*
%************************************************************************

\begin{code}

type LocalLabel = Int

data BCInstr
   -- Messing with the stack
   = ARGCHECK  Int
   | PUSH_L    Int{-size-} Int{-offset-}
   | PUSH_G    Name
   | PUSHT_I   Int
   | PUSHT_F   Float
   | PUSHT_D   Double
   | PUSHU_I   Int
   | PUSHU_F   Float
   | PUSHU_D   Double
   | SLIDE     Int{-this many-} Int{-down by this much-}
   -- To do with the heap
   | ALLOC     Int
   | MKAP      Int{-place ptr to heap this far down stack-} Int{-# words-}
   | UNPACK    Int
   | PACK      DataCon Int
   -- For doing case trees
   | LABEL     LocalLabel
   | TESTLT_I  Int    LocalLabel
   | TESTEQ_I  Int    LocalLabel
   | TESTLT_F  Float  LocalLabel
   | TESTEQ_F  Float  LocalLabel
   | TESTLT_D  Double LocalLabel
   | TESTEQ_D  Double LocalLabel
   | TESTLT_P  Int    LocalLabel
   | TESTEQ_P  Int    LocalLabel
   | CASEFAIL
   -- To Infinity And Beyond
   | ENTER

instance Outputable BCInstr where
   ppr (ARGCHECK n)          = text "ARGCHECK" <+> int n
   ppr (PUSH_L sz offset)    = text "PUSH_L  " <+> int sz <+> int offset
   ppr (PUSH_G nm)           = text "PUSH_G  " <+> ppr nm
   ppr (PUSHT_I i)           = text "PUSHT_I " <+> int i
   ppr (SLIDE n d)           = text "SLIDE   " <+> int n <+> int d
   ppr (ALLOC sz)            = text "ALLOC   " <+> int sz
   ppr (MKAP offset sz)      = text "MKAP    " <+> int offset <+> int sz
   ppr (UNPACK sz)           = text "UNPACK  " <+> int sz
   ppr (PACK dcon sz)        = text "PACK    " <+> ppr dcon <+> ppr sz
   ppr ENTER                 = text "ENTER"

pprAltCode discrs_n_codes
   = vcat (map f discrs_n_codes)
     where f (discr, code) = ppr discr <> colon <+> vcat (map ppr (fromOL code))

instance Outputable a => Outputable (ProtoBCO a) where
   ppr (ProtoBCO name instrs)
      = (text "ProtoBCO" <+> ppr name <> colon)
        $$ nest 6 (vcat (map ppr (fromOL instrs)))

\end{code}

%************************************************************************
%*									*
\subsection{Compilation schema for the bytecode generator.}
%*									*
%************************************************************************

\begin{code}

type BCInstrList = OrdList BCInstr

data ProtoBCO a = ProtoBCO a BCInstrList

type Sequel = Int	-- back off to this depth before ENTER

-- Maps Ids to the offset from the stack _base_ so we don't have
-- to mess with it after each push/pop.
type BCEnv = FiniteMap Id Int	-- To find vars on the stack



-- Compile code for the right hand side of a let binding.
-- Park the resulting BCO in the monad.  Also requires the
-- variable to which this value was bound, so as to give the
-- resulting BCO a name.
schemeR :: (Id, AnnExpr Id VarSet) -> BcM ()
schemeR (nm, rhs) = schemeR_wrk nm (collect [] rhs)

collect xs (_, AnnLam x e) = collect (x:xs) e
collect xs not_lambda      = (reverse xs, not_lambda)

schemeR_wrk nm (args, body)
   = let fvs       = fst body
         all_args  = varSetElems fvs ++ args
         szsw_args = map taggedIdSizeW all_args
         szw_args  = sum szsw_args
         p_init    = listToFM (zip all_args (scanl (+) 0 szsw_args))
         argcheck  = if null args then nilOL else unitOL (ARGCHECK szw_args)
     in
     schemeE szw_args 0 p_init body 		`thenBc` \ body_code ->
     emitBc (ProtoBCO (getName nm) (appOL argcheck body_code))


-- Compile code to apply the given expression to the remaining args
-- on the stack, returning a HNF.
schemeE :: Int -> Sequel -> BCEnv -> AnnExpr Id VarSet -> BcM BCInstrList

-- Delegate tail-calls to schemeT.
schemeE d s p e@(fvs, AnnApp f a) 
   = returnBc (schemeT (should_args_be_tagged e) d s 0 p (fvs, AnnApp f a))
schemeE d s p e@(fvs, AnnVar v)
   = returnBc (schemeT (should_args_be_tagged e) d s 0 p (fvs, AnnVar v))

schemeE d s p (fvs, AnnLet binds b)
   = let (xs,rhss) = case binds of AnnNonRec x rhs  -> ([x],[rhs])
                                   AnnRec xs_n_rhss -> unzip xs_n_rhss
     in
     mapBc schemeR (zip xs rhss)			`thenBc_`
     let n     = length xs
         fvss  = map (varSetElems.fst) rhss
         sizes = map (\rhs_fvs -> 1 + sum (map taggedIdSizeW rhs_fvs)) fvss
         p'    = addListToFM p (zipE xs [d .. d+n-1])
         d'    = d + n
         infos = zipE4 fvss sizes xs [n, n-1 .. 1]
         zipE  = zipEqual "schemeE"
         zipE4 = zipWith4Equal "schemeE" (\a b c d -> (a,b,c,d))

         -- ToDo: don't build thunks for things with no free variables
         buildThunk (fvs, size, id, off)
            = case unzip (map (pushAtom True d' p . AnnVar) (reverse fvs)) of
                (push_codes, pushed_szsw) 
                   -> ASSERT(sum pushed_szsw == size - 1)
                            (toOL push_codes `snocOL` PUSH_G (getName id) 
                                             `appOL` unitOL (MKAP off size))

         thunkCode = concatOL (map buildThunk infos)
         allocCode = toOL (map ALLOC sizes)
     in
     schemeE d' s p' b   				`thenBc` \ bodyCode ->
     mapBc schemeR (zip xs rhss) 			`thenBc` \_ ->
     returnBc (allocCode `appOL` thunkCode `appOL` bodyCode)


schemeE d s p (fvs, AnnCase scrut bndr alts)
   = let
        -- Top of stack is the return itbl, as usual.
        -- underneath it is the pointer to the alt_code BCO.
        -- When an alt is entered, it assumes the returned value is
        -- on top of the itbl.
        ret_frame_sizeW = 2

        -- Env and depth in which to compile the alts, not including
        -- any vars bound by the alts themselves
        d' = d + ret_frame_sizeW + taggedIdSizeW bndr
        p' = addToFM p bndr d'

        isAlgCase
           = case typePrimRep (idType bndr) of
                IntRep -> False ; FloatRep -> False ; DoubleRep -> False
                PtrRep -> True
                other  -> pprPanic "ByteCodeGen.schemeE" (ppr other)

        -- given an alt, return a discr and code for it.
        codeAlt alt@(discr, binds, rhs)
           | isAlgCase 
           = let binds_szsw = map untaggedIdSizeW binds
                 binds_szw  = sum binds_szsw
                 p'' = addListToFM p' (zip binds (scanl (+) d' binds_szsw))
                 d'' = d' + binds_szw
             in schemeE d'' s p'' rhs	`thenBc` \ rhs_code -> 
                returnBc (my_discr alt, UNPACK binds_szw `consOL` rhs_code)
           | otherwise 
           = ASSERT(null binds) 
             schemeE d' s p' rhs	`thenBc` \ rhs_code ->
             returnBc (my_discr alt, rhs_code)

        my_discr (DEFAULT, binds, rhs)  = NoDiscr
        my_discr (DataAlt dc, binds, rhs) = DiscrP (dataConTag dc - fIRST_TAG)
        my_discr (LitAlt l, binds, rhs)
           = case l of MachInt i     -> DiscrI (fromInteger i)
                       MachFloat r   -> DiscrF (fromRational r)
                       MachDouble r  -> DiscrD (fromRational r)

     in 
     mapBc codeAlt alts 				`thenBc` \ alt_stuff ->
     mkMultiBranch alt_stuff				`thenBc` \ alt_final ->
     let 
         alt_bco_name = getName bndr
         alt_bco      = ProtoBCO alt_bco_name alt_final
     in
     schemeE (d + ret_frame_sizeW) 
             (d + ret_frame_sizeW) p scrut		`thenBc` \ scrut_code ->

     emitBc alt_bco 					`thenBc_`
     returnBc (PUSH_G alt_bco_name `consOL` scrut_code)


-- Compile code to do a tail call.  Doesn't need to be monadic.
schemeT :: Bool 	-- do tagging?
        -> Int 		-- Stack depth
        -> Sequel 	-- Sequel depth
        -> Int 		-- # arg words so far
        -> BCEnv 	-- stack env
        -> AnnExpr Id VarSet -> BCInstrList

schemeT enTag d s narg_words p (_, AnnApp f a) 
   = let (push, arg_words) = pushAtom enTag d p (snd a)
     in push 
        `consOL` schemeT enTag (d+arg_words) s (narg_words+arg_words) p f

schemeT enTag d s narg_words p (_, AnnVar f)
   | Just con <- isDataConId_maybe f
   = ASSERT(enTag == False)
     PACK con narg_words `consOL` SLIDE 1 (d-s-1) `consOL` unitOL ENTER
   | otherwise
   = ASSERT(enTag == True)
     let (push, arg_words) = pushAtom True d p (AnnVar f)
     in push 
        `consOL` SLIDE (narg_words+arg_words) (d - s - narg_words)
        `consOL` unitOL ENTER

should_args_be_tagged (_, AnnVar v)
   = case isDataConId_maybe v of
        Just dcon -> False; Nothing -> True
should_args_be_tagged (_, AnnApp f a)
   = should_args_be_tagged f
should_args_be_tagged (_, other)
   = panic "should_args_be_tagged: tail call to non-con, non-var"

-- Push an atom onto the stack, returning suitable code & number of
-- stack words used.  Pushes it either tagged or untagged, since 
-- pushAtom is used to set up the stack prior to copying into the
-- heap for both APs (requiring tags) and constructors (which don't).
--
-- NB this means NO GC between pushing atoms for a constructor and
-- copying them into the heap.  It probably also means that 
-- tail calls MUST be of the form atom{atom ... atom} since if the
-- expression head was allowed to be arbitrary, there could be GC
-- in between pushing the arg atoms and completing the head.
-- (not sure; perhaps the allocate/doYouWantToGC interface means this
-- isn't a problem; but only if arbitrary graph construction for the
-- head doesn't leave this BCO, since GC might happen at the start of
-- each BCO (we consult doYouWantToGC there).
--
-- Blargh.  JRS 001206
--
pushAtom True{-tagged-} d p (AnnVar v) 
   = case lookupBCEnv_maybe p v of
        Just offset -> (PUSH_L sz offset, sz)
        Nothing     -> ASSERT(sz == 1) (PUSH_G nm, sz)
     where
        nm = getName v
        sz = taggedIdSizeW v

pushAtom False{-not tagged-} d p (AnnVar v) 
   = case lookupBCEnv_maybe p v of
        Just offset -> (PUSH_L sz (offset+1), sz-1)
        Nothing     -> ASSERT(sz == 1) (PUSH_G nm, sz)
     where
        nm = getName v
        sz = untaggedIdSizeW v

pushAtom True d p (AnnLit lit)
   = case lit of
        MachInt i    -> (PUSHT_I (fromInteger i),  taggedSizeW IntRep)
        MachFloat r  -> (PUSHT_F (fromRational r), taggedSizeW FloatRep)
        MachDouble r -> (PUSHT_D (fromRational r), taggedSizeW DoubleRep)

pushAtom False d p (AnnLit lit)
   = case lit of
        MachInt i    -> (PUSHU_I (fromInteger i),  untaggedSizeW IntRep)
        MachFloat r  -> (PUSHU_F (fromRational r), untaggedSizeW FloatRep)
        MachDouble r -> (PUSHU_D (fromRational r), untaggedSizeW DoubleRep)


-- Given a bunch of alts code and their discrs, do the donkey work
-- of making a multiway branch using a switch tree.
-- What a load of hassle!
mkMultiBranch :: [(Discr, BCInstrList)] -> BcM BCInstrList
mkMultiBranch raw_ways
   = let d_way     = filter (isNoDiscr.fst) raw_ways
         notd_ways = naturalMergeSortLe 
                        (\w1 w2 -> leAlt (fst w1) (fst w2))
                        (filter (not.isNoDiscr.fst) raw_ways)

         mkTree :: [(Discr, BCInstrList)] -> Discr -> Discr -> BcM BCInstrList
         mkTree [] range_lo range_hi = returnBc the_default

         mkTree [val] range_lo range_hi
            | range_lo `eqAlt` range_hi 
            = returnBc (snd val)
            | otherwise
            = getLabelBc 				`thenBc` \ label_neq ->
              returnBc (mkTestEQ (fst val) label_neq 
			`consOL` (snd val
			`appOL`   unitOL (LABEL label_neq)
			`appOL`   the_default))

         mkTree vals range_lo range_hi
            = let n = length vals `div` 2
                  vals_lo = take n vals
                  vals_hi = drop n vals
                  v_mid = fst (head vals_hi)
              in
              getLabelBc 				`thenBc` \ label_geq ->
              mkTree vals_lo range_lo (dec v_mid) 	`thenBc` \ code_lo ->
              mkTree vals_hi v_mid range_hi 		`thenBc` \ code_hi ->
              returnBc (mkTestLT v_mid label_geq
                        `consOL` (code_lo
			`appOL`   unitOL (LABEL label_geq)
			`appOL`   code_hi))
 
         the_default 
            = case d_way of [] -> unitOL CASEFAIL
                            [(_, def)] -> def

         -- None of these will be needed if there are no non-default alts
         (mkTestLT, mkTestEQ, init_lo, init_hi)
            | null notd_ways
            = panic "mkMultiBranch: awesome foursome"
            | otherwise
            = case fst (head notd_ways) of {
              DiscrI _ -> ( \(DiscrI i) fail_label -> TESTLT_I i fail_label,
                            \(DiscrI i) fail_label -> TESTEQ_I i fail_label,
                            DiscrI minBound,
                            DiscrI maxBound );
              DiscrF _ -> ( \(DiscrF f) fail_label -> TESTLT_F f fail_label,
                            \(DiscrF f) fail_label -> TESTEQ_F f fail_label,
                            DiscrF minF,
                            DiscrF maxF );
              DiscrD _ -> ( \(DiscrD d) fail_label -> TESTLT_D d fail_label,
                            \(DiscrD d) fail_label -> TESTEQ_D d fail_label,
                            DiscrD minD,
                            DiscrD maxD );
              DiscrP _ -> ( \(DiscrP i) fail_label -> TESTLT_P i fail_label,
                            \(DiscrP i) fail_label -> TESTEQ_P i fail_label,
                            DiscrP minBound,
                            DiscrP maxBound )
              }

         (DiscrI i1) `eqAlt` (DiscrI i2) = i1 == i2
         (DiscrF f1) `eqAlt` (DiscrF f2) = f1 == f2
         (DiscrD d1) `eqAlt` (DiscrD d2) = d1 == d2
         (DiscrP i1) `eqAlt` (DiscrP i2) = i1 == i2
         NoDiscr     `eqAlt` NoDiscr     = True
         _           `eqAlt` _           = False

         (DiscrI i1) `leAlt` (DiscrI i2) = i1 <= i2
         (DiscrF f1) `leAlt` (DiscrF f2) = f1 <= f2
         (DiscrD d1) `leAlt` (DiscrD d2) = d1 <= d2
         (DiscrP i1) `leAlt` (DiscrP i2) = i1 <= i2
         NoDiscr     `leAlt` NoDiscr     = True
         _           `leAlt` _           = False

         isNoDiscr NoDiscr = True
         isNoDiscr _       = False

         dec (DiscrI i) = DiscrI (i-1)
         dec (DiscrP i) = DiscrP (i-1)
         dec other      = other		-- not really right, but if you
		-- do cases on floating values, you'll get what you deserve

         -- same snotty comment applies to the following
         minF, maxF :: Float
         minD, maxD :: Double
         minF = -1.0e37
         maxF =  1.0e37
         minD = -1.0e308
         maxD =  1.0e308
     in
         mkTree notd_ways init_lo init_hi

\end{code}

%************************************************************************
%*									*
\subsection{Supporting junk for the compilation schemes}
%*									*
%************************************************************************

\begin{code}

-- Describes case alts
data Discr 
   = DiscrI Int
   | DiscrF Float
   | DiscrD Double
   | DiscrP Int
   | NoDiscr

instance Outputable Discr where
   ppr (DiscrI i) = int i
   ppr (DiscrF f) = text (show f)
   ppr (DiscrD d) = text (show d)
   ppr (DiscrP i) = int i
   ppr NoDiscr    = text "DEF"


-- Find things in the BCEnv (the what's-on-the-stack-env)
lookupBCEnv :: BCEnv -> Id -> Int
lookupBCEnv env nm
   = case lookupFM env nm of
        Nothing -> pprPanic "lookupBCEnv" 
                            (ppr nm $$ char ' ' $$ vcat (map ppr (fmToList env)))
        Just xx -> xx

lookupBCEnv_maybe :: BCEnv -> Id -> Maybe Int
lookupBCEnv_maybe = lookupFM


-- When I push one of these on the stack, how much does Sp move by?
taggedSizeW :: PrimRep -> Int
taggedSizeW pr
   | isFollowableRep pr = 1
   | otherwise          = 1{-the tag-} + getPrimRepSize pr


-- The plain size of something, without tag.
untaggedSizeW :: PrimRep -> Int
untaggedSizeW pr
   | isFollowableRep pr = 1
   | otherwise          = getPrimRepSize pr


taggedIdSizeW, untaggedIdSizeW :: Id -> Int
taggedIdSizeW   = taggedSizeW   . typePrimRep . idType
untaggedIdSizeW = untaggedSizeW . typePrimRep . idType

\end{code}

%************************************************************************
%*									*
\subsection{The bytecode generator's monad}
%*									*
%************************************************************************

\begin{code}
data BcM_State 
   = BcM_State { bcos      :: [ProtoBCO Name],	-- accumulates completed BCOs
                 nextlabel :: Int }		-- for generating local labels

type BcM result = BcM_State -> (result, BcM_State)

mkBcM_State :: [ProtoBCO Name] -> Int -> BcM_State
mkBcM_State = BcM_State

runBc :: BcM_State -> BcM () -> BcM_State
runBc init_st m = case m init_st of { (r,st) -> st }

thenBc :: BcM a -> (a -> BcM b) -> BcM b
thenBc expr cont st
  = case expr st of { (result, st') -> cont result st' }

thenBc_ :: BcM a -> BcM b -> BcM b
thenBc_ expr cont st
  = case expr st of { (result, st') -> cont st' }

returnBc :: a -> BcM a
returnBc result st = (result, st)

mapBc :: (a -> BcM b) -> [a] -> BcM [b]
mapBc f []     = returnBc []
mapBc f (x:xs)
  = f x          `thenBc` \ r  ->
    mapBc f xs   `thenBc` \ rs ->
    returnBc (r:rs)

emitBc :: ProtoBCO Name -> BcM ()
emitBc bco st
   = ((), st{bcos = bco : bcos st})

getLabelBc :: BcM Int
getLabelBc st
   = (nextlabel st, st{nextlabel = 1 + nextlabel st})

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
-- An (almost) assembled BCO.
data BCO a = BCO [Word16] 	-- instructions
                 [Word32] 	-- literal pool
                 [a] 		-- Names or HValues

-- Top level assembler fn.
assembleBCO :: ProtoBCO Name -> BCO Name
assembleBCO (ProtoBCO nm instrs_ordlist)
   = let
         -- pass 1: collect up the offsets of the local labels
         instrs = fromOL instrs_ordlist
         label_env = mkLabelEnv emptyFM 0 instrs

         mkLabelEnv env i_offset [] = env
         mkLabelEnv env i_offset (i:is)
            = let new_env 
                     = case i of LABEL n -> addToFM env n i_offset ; _ -> env
              in  mkLabelEnv new_env (i_offset + instrSizeB i) is

         findLabel lab
            = case lookupFM label_env lab of
                 Just bco_offset -> bco_offset
                 Nothing -> pprPanic "assembleBCO.findLabel" (int lab)

         -- pass 2: generate the instruction, ptr and nonptr bits
         (insnW16s, litW32s, ptrs) = mkBits findLabel [] 0 [] 0 [] 0 instrs
     in
         BCO insnW16s litW32s ptrs


-- This is where all the action is (pass 2 of the assembler)
mkBits :: (Int -> Int) 		-- label finder
       -> [Word16] -> Int 	-- reverse acc instr bits
       -> [Word32] -> Int 	-- reverse acc literal bits
       -> [Name] -> Int		-- reverse acc ptrs
       -> [BCInstr]		-- insns!
       -> ([Word16], [Word32], [Name])

mkBits findLabel r_is n_is r_lits n_lits r_ptrs n_ptrs []
   = (reverse r_is, reverse r_lits, reverse r_ptrs)
mkBits findLabel r_is n_is r_lits n_lits r_ptrs n_ptrs (instr:instrs)
   = case instr of
        ARGCHECK  n    -> boring2 i_ARGCHECK n
        PUSH_L    sz off -> boring3 i_PUSH_L sz off
        PUSH_G    nm   -> exciting2_P i_PUSH_G n_ptrs nm
        PUSHT_I   i    -> exciting2_I i_PUSHT_I n_lits i
        PUSHT_F   f    -> exciting2_F i_PUSHT_F n_lits f
        PUSHT_D   d    -> exciting2_D i_PUSHT_D n_lits d
        PUSHU_I   i    -> exciting2_I i_PUSHU_I n_lits i
        PUSHU_F   f    -> exciting2_F i_PUSHU_F n_lits f
        PUSHU_D   d    -> exciting2_D i_PUSHU_D n_lits d
        SLIDE     n by -> boring3 i_SLIDE n by
        ALLOC     n    -> boring2 i_ALLOC n
        MKAP      off sz -> boring3 i_MKAP off sz
        UNPACK    n    -> boring2 i_UNPACK n
        PACK      dcon sz -> exciting3_A i_PACK sz n_lits nullAddr {-findItbl dcon-}
        LABEL     lab  -> nop
        TESTLT_I  i l  -> exciting3_I i_TESTLT_I n_lits (findLabel l) i
        TESTEQ_I  i l  -> exciting3_I i_TESTEQ_I n_lits (findLabel l) i
        TESTLT_F  f l  -> exciting3_F i_TESTLT_F n_lits (findLabel l) f
        TESTEQ_F  f l  -> exciting3_F i_TESTEQ_F n_lits (findLabel l) f
        TESTLT_D  d l  -> exciting3_D i_TESTLT_D n_lits (findLabel l) d
        TESTEQ_D  d l  -> exciting3_D i_TESTEQ_D n_lits (findLabel l) d
        TESTLT_P  i l  -> exciting3_I i_TESTLT_P n_lits (findLabel l) i
        TESTEQ_P  i l  -> exciting3_I i_TESTEQ_P n_lits (findLabel l) i
        CASEFAIL       -> boring1 i_CASEFAIL
        ENTER          -> boring1 i_ENTER
     where
        r_mkILit = reverse . mkILit
        r_mkFLit = reverse . mkFLit
        r_mkDLit = reverse . mkDLit
        r_mkALit = reverse . mkALit

        mkw :: Int -> Word16
        mkw = fromIntegral

        nop
           = mkBits findLabel r_is n_is r_lits n_lits r_ptrs n_ptrs instrs
        boring1 i1
           = mkBits findLabel (mkw i1 : r_is) (n_is+1) 
                    r_lits n_lits r_ptrs n_ptrs instrs
        boring2 i1 i2 
           = mkBits findLabel (mkw i2 : mkw i1 : r_is) (n_is+2) 
                    r_lits n_lits r_ptrs n_ptrs instrs
        boring3 i1 i2 i3
           = mkBits findLabel (mkw i3 : mkw i2 : mkw i1 : r_is) (n_is+3) 
                    r_lits n_lits r_ptrs n_ptrs instrs

        exciting2_P i1 i2 p
           = mkBits findLabel (mkw i2 : mkw i1 : r_is) (n_is+2) r_lits n_lits
                    (p:r_ptrs) (n_ptrs+1) instrs
        exciting3_P i1 i2 i3 p
           = mkBits findLabel (mkw i3 : mkw i2 : mkw i1 : r_is) (n_is+3) r_lits n_lits
                    (p:r_ptrs) (n_ptrs+1) instrs

        exciting2_I i1 i2 i
           = mkBits findLabel (mkw i2 : mkw i1 : r_is) (n_is+2) 
                    (r_mkILit i ++ r_lits) (n_lits + intLitSz32s)
                    r_ptrs n_ptrs instrs
        exciting3_I i1 i2 i3 i
           = mkBits findLabel (mkw i3 : mkw i2 : mkw i1 : r_is) (n_is+3) 
                    (r_mkILit i ++ r_lits) (n_lits + intLitSz32s)
                    r_ptrs n_ptrs instrs

        exciting2_F i1 i2 f
           = mkBits findLabel (mkw i2 : mkw i1 : r_is) (n_is+2) 
                    (r_mkFLit f ++ r_lits) (n_lits + floatLitSz32s)
                    r_ptrs n_ptrs instrs
        exciting3_F i1 i2 i3 f
           = mkBits findLabel (mkw i3 : mkw i2 : mkw i1 : r_is) (n_is+3) 
                    (r_mkFLit f ++ r_lits) (n_lits + floatLitSz32s)
                    r_ptrs n_ptrs instrs

        exciting2_D i1 i2 d
           = mkBits findLabel (mkw i2 : mkw i1 : r_is) (n_is+2) 
                    (r_mkDLit d ++ r_lits) (n_lits + doubleLitSz32s)
                    r_ptrs n_ptrs instrs
        exciting3_D i1 i2 i3 d
           = mkBits findLabel (mkw i3 : mkw i2 : mkw i1 : r_is) (n_is+3) 
                    (r_mkDLit d ++ r_lits) (n_lits + doubleLitSz32s)
                    r_ptrs n_ptrs instrs

        exciting3_A i1 i2 i3 d
           = mkBits findLabel (mkw i3 : mkw i2 : mkw i1 : r_is) (n_is+3) 
                    (r_mkALit d ++ r_lits) (n_lits + addrLitSz32s)
                    r_ptrs n_ptrs instrs


-- The size in bytes of an instruction.
instrSizeB :: BCInstr -> Int
instrSizeB instr
   = case instr of
        ARGCHECK _   -> 4
        PUSH_L   _ _ -> 6
        PUSH_G   _   -> 4
        PUSHT_I  _   -> 4
        PUSHT_F  _   -> 4
        PUSHT_D  _   -> 4
        PUSHU_I  _   -> 4
        PUSHU_F  _   -> 4
        PUSHU_D  _   -> 4
        SLIDE    _ _ -> 6
        ALLOC    _   -> 4
        MKAP     _ _ -> 6
        UNPACK   _   -> 4
        PACK     _ _ -> 6
        LABEL    _   -> 4
        TESTLT_I _ _ -> 6
        TESTEQ_I _ _ -> 6
        TESTLT_F _ _ -> 6
        TESTEQ_F _ _ -> 6
        TESTLT_D _ _ -> 6
        TESTEQ_D _ _ -> 6
        TESTLT_P _ _ -> 6
        TESTEQ_P _ _ -> 6
        CASEFAIL     -> 2
        ENTER        -> 2


-- Sizes of Int, Float and Double literals, in units of 32-bitses
intLitSz32s, floatLitSz32s, doubleLitSz32s, addrLitSz32s :: Int
intLitSz32s    = wORD_SIZE `div` 4
floatLitSz32s  = 1	-- Assume IEEE floats
doubleLitSz32s = 2
addrLitSz32s   = intLitSz32s

-- Make lists of 32-bit words for literals, so that when the
-- words are placed in memory at increasing addresses, the
-- bit pattern is correct for the host's word size and endianness.
mkILit :: Int    -> [Word32]
mkFLit :: Float  -> [Word32]
mkDLit :: Double -> [Word32]
mkALit :: Addr   -> [Word32]

mkFLit f
   = runST (do
        arr <- newFloatArray ((0::Int),0)
        writeFloatArray arr 0 f
        w0 <- readWord32Array arr 0
        return [w0]
     )

mkDLit d
   = runST (do
        arr <- newDoubleArray ((0::Int),0)
        writeDoubleArray arr 0 d
        w0 <- readWord32Array arr 0
        w1 <- readWord32Array arr 1
        return [w0,w1]
     )

mkILit i
   | wORD_SIZE == 4
   = runST (do
        arr <- newIntArray ((0::Int),0)
        writeIntArray arr 0 i
        w0 <- readWord32Array arr 0
        return [w0]
     )
   | wORD_SIZE == 8
   = runST (do
        arr <- newIntArray ((0::Int),0)
        writeIntArray arr 0 i
        w0 <- readWord32Array arr 0
        w1 <- readWord32Array arr 1
        return [w0,w1]
     )
   
mkALit a
   | wORD_SIZE == 4
   = runST (do
        arr <- newAddrArray ((0::Int),0)
        writeAddrArray arr 0 a
        w0 <- readWord32Array arr 0
        return [w0]
     )
   | wORD_SIZE == 8
   = runST (do
        arr <- newAddrArray ((0::Int),0)
        writeAddrArray arr 0 a
        w0 <- readWord32Array arr 0
        w1 <- readWord32Array arr 1
        return [w0,w1]
     )
   


#include "../rts/Bytecodes.h"

i_ARGCHECK = (bci_ARGCHECK :: Int)
i_PUSH_L   = (bci_PUSH_L   :: Int)
i_PUSH_G   = (bci_PUSH_G   :: Int)
i_PUSHT_I  = (bci_PUSHT_I  :: Int)
i_PUSHT_F  = (bci_PUSHT_F  :: Int)
i_PUSHT_D  = (bci_PUSHT_D  :: Int)
i_PUSHU_I  = (bci_PUSHU_I  :: Int)
i_PUSHU_F  = (bci_PUSHU_F  :: Int)
i_PUSHU_D  = (bci_PUSHU_D  :: Int)
i_SLIDE    = (bci_SLIDE    :: Int)
i_ALLOC    = (bci_ALLOC    :: Int)
i_MKAP     = (bci_MKAP     :: Int)
i_UNPACK   = (bci_UNPACK   :: Int)
i_PACK     = (bci_PACK     :: Int)
i_LABEL    = (bci_LABEL    :: Int)
i_TESTLT_I = (bci_TESTLT_I :: Int)
i_TESTEQ_I = (bci_TESTEQ_I :: Int)
i_TESTLT_F = (bci_TESTLT_F :: Int)
i_TESTEQ_F = (bci_TESTEQ_F :: Int)
i_TESTLT_D = (bci_TESTLT_D :: Int)
i_TESTEQ_D = (bci_TESTEQ_D :: Int)
i_TESTLT_P = (bci_TESTLT_P :: Int)
i_TESTEQ_P = (bci_TESTEQ_P :: Int)
i_CASEFAIL = (bci_CASEFAIL :: Int)
i_ENTER    = (bci_ENTER    :: Int)

\end{code}
