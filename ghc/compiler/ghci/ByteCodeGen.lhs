%
% (c) The University of Glasgow 2000
%
\section[ByteCodeGen]{Generate bytecode from Core}

\begin{code}
module ByteCodeGen ( UnlinkedBCO, UnlinkedBCOExpr, ItblEnv, ClosureEnv, HValue,
		     filterNameMap,
                     byteCodeGen, coreExprToBCOs, 
		     linkIModules, linkIExpr
		   ) where

#include "HsVersions.h"

import Outputable
import Name		( Name, getName, nameModule, mkSysLocalName, toRdrName )
import RdrName		( rdrNameOcc, rdrNameModule )
import OccName		( occNameString )
import Id		( Id, idType, isDataConId_maybe, mkVanillaId )
import OrdList		( OrdList, consOL, snocOL, appOL, unitOL, 
			  nilOL, toOL, concatOL, fromOL )
import FiniteMap	( FiniteMap, addListToFM, listToFM, filterFM,
			  addToFM, lookupFM, fmToList, emptyFM, plusFM )
import CoreSyn
import PprCore		( pprCoreExpr, pprCoreAlt )
import Literal		( Literal(..), literalPrimRep )
import PrimRep		( PrimRep(..) )
import CoreFVs		( freeVars )
import Type		( typePrimRep )
import DataCon		( DataCon, dataConTag, fIRST_TAG, dataConTyCon, 
			  dataConRepArgTys )
import TyCon		( TyCon, tyConFamilySize, isDataTyCon, tyConDataCons )
import Class		( Class, classTyCon )
import Util		( zipEqual, zipWith4Equal, naturalMergeSortLe, nOfThem, global )
import Var		( isTyVar )
import VarSet		( VarSet, varSetElems )
import PrimRep		( getPrimRepSize, isFollowableRep )
import Constants	( wORD_SIZE )
import CmdLineOpts	( DynFlags, DynFlag(..) )
import ErrUtils		( showPass, dumpIfSet_dyn )
import ClosureInfo	( mkVirtHeapOffsets )
import Module		( ModuleName, moduleName, moduleNameFS )
import Unique		( mkPseudoUnique3 )
import Linker		( lookupSymbol )

import List		( intersperse )
import Monad		( foldM )
import ST		( runST )
import MArray		( castSTUArray, 
			  newFloatArray, writeFloatArray,
			  newDoubleArray,  writeDoubleArray,
			  newIntArray, writeIntArray,
			  newAddrArray, writeAddrArray )
import Foreign		( Storable(..), Word8, Word16, Word32, Ptr(..), 
			  malloc, castPtr, plusPtr )
import Addr		( Word, Addr, addrToInt, nullAddr )
import Bits		( Bits(..), shiftR )

import PrelGHC		( BCO#, newBCO#, unsafeCoerce#, ByteArray#, Array# )
import IOExts		( IORef, fixIO )
import ArrayBase	
import PrelArr		( Array(..) )
import PrelIOBase	( IO(..) )

\end{code}

%************************************************************************
%*									*
\subsection{Functions visible from outside this module.}
%*									*
%************************************************************************

\begin{code}

byteCodeGen :: DynFlags
            -> [CoreBind] 
            -> [TyCon] -> [Class]
            -> IO ([UnlinkedBCO], ItblEnv)
byteCodeGen dflags binds local_tycons local_classes
   = do showPass dflags "ByteCodeGen"
        let tycs = local_tycons ++ map classTyCon local_classes
        itblenv <- mkITbls tycs

        let flatBinds = concatMap getBind binds
            getBind (NonRec bndr rhs) = [(bndr, freeVars rhs)]
            getBind (Rec binds)       = [(bndr, freeVars rhs) | (bndr,rhs) <- binds]
            final_state = runBc (BcM_State [] 0) 
                                (mapBc schemeR flatBinds `thenBc_` returnBc ())
            (BcM_State proto_bcos final_ctr) = final_state

        dumpIfSet_dyn dflags Opt_D_dump_BCOs
           "Proto-bcos" (vcat (intersperse (char ' ') (map ppr proto_bcos)))

        bcos <- mapM assembleBCO proto_bcos

        return (bcos, itblenv)
        

-- Returns: (the root BCO for this expression, 
--           a list of auxilary BCOs resulting from compiling closures)
coreExprToBCOs :: DynFlags
	       -> CoreExpr
               -> IO UnlinkedBCOExpr
coreExprToBCOs dflags expr
 = do showPass dflags "ByteCodeGen"

      -- create a totally bogus name for the top-level BCO; this
      -- should be harmless, since it's never used for anything
      let invented_name = mkSysLocalName (mkPseudoUnique3 0) SLIT("Expr-Top-Level")
      let invented_id   = mkVanillaId invented_name (panic "invented_id's type")

      let (BcM_State all_proto_bcos final_ctr) 
             = runBc (BcM_State [] 0) 
                     (schemeR (invented_id, freeVars expr))
      dumpIfSet_dyn dflags Opt_D_dump_BCOs
         "Proto-bcos" (vcat (intersperse (char ' ') (map ppr all_proto_bcos)))

      let root_proto_bco 
             = case filter ((== invented_name).nameOfProtoBCO) all_proto_bcos of
                  [root_bco] -> root_bco
          auxiliary_proto_bcos
             = filter ((/= invented_name).nameOfProtoBCO) all_proto_bcos

      auxiliary_bcos <- mapM assembleBCO auxiliary_proto_bcos
      root_bco <- assembleBCO root_proto_bco

      return (root_bco, auxiliary_bcos)


-- Linking stuff
linkIModules :: ItblEnv    -- incoming global itbl env; returned updated
	     -> ClosureEnv -- incoming global closure env; returned updated
	     -> [([UnlinkedBCO], ItblEnv)]
	     -> IO ([HValue], ItblEnv, ClosureEnv)
linkIModules gie gce mods = do
  let (bcoss, ies) = unzip mods
      bcos = concat bcoss
      top_level_binders = map nameOfUnlinkedBCO bcos
      final_gie = foldr plusFM gie ies
  
  (new_bcos, new_gce) <-
    fixIO (\ ~(new_bcos, new_gce) -> do
      new_bcos <- linkBCOs final_gie new_gce bcos
      let new_gce = addListToFM gce (zip top_level_binders new_bcos)
      return (new_bcos, new_gce))

  return (new_bcos, final_gie, new_gce)


linkIExpr :: ItblEnv -> ClosureEnv -> UnlinkedBCOExpr
          -> IO HValue 	  -- IO BCO# really
linkIExpr ie ce (root_ul_bco, aux_ul_bcos)
   = do let aux_ul_binders = map nameOfUnlinkedBCO aux_ul_bcos
        (aux_bcos, aux_ce) 
           <- fixIO 
                (\ ~(aux_bcos, new_ce) 
                 -> do new_bcos <- linkBCOs ie new_ce aux_ul_bcos
                       let new_ce = addListToFM ce (zip aux_ul_binders new_bcos)
                       return (new_bcos, new_ce)
                )
        [root_bco]
           <- linkBCOs ie aux_ce [root_ul_bco]
        return root_bco



data UnlinkedBCO
   = UnlinkedBCO Name
                 (SizedSeq Word16)	-- insns
                 (SizedSeq Word)	-- literals
                 (SizedSeq Name)	-- ptrs
                 (SizedSeq Name)	-- itbl refs

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
type ItblEnv    = FiniteMap Name (Ptr StgInfoTable)
type ClosureEnv = FiniteMap Name HValue
data HValue     = HValue  -- dummy type, actually a pointer to some Real Code.

-- remove all entries for a given set of modules from the environment
filterNameMap :: [ModuleName] -> FiniteMap Name a -> FiniteMap Name a
filterNameMap mods env 
   = filterFM (\n _ -> moduleName (nameModule n) `notElem` mods) env
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
   -- Push locals (existing bits of the stack)
   | PUSH_L    Int{-offset-}
   | PUSH_LL   Int Int{-2 offsets-}
   | PUSH_LLL  Int Int Int{-3 offsets-}
   -- Push a ptr
   | PUSH_G    Name
   -- Push an alt continuation
   | PUSH_AS   Name PrimRep	-- push alts and BCO_ptr_ret_info
				-- PrimRep so we know which itbl
   -- Pushing literals
   | PUSH_UBX  Literal	Int 
                        -- push this int/float/double, NO TAG, on the stack
			-- Int is # of words to copy from literal pool
   | PUSH_TAG  Int      -- push this tag on the stack

   | SLIDE     Int{-this many-} Int{-down by this much-}
   -- To do with the heap
   | ALLOC     Int	-- make an AP_UPD with this many payload words, zeroed
   | MKAP      Int{-ptr to AP_UPD is this far down stack-} Int{-# words-}
   | UNPACK    Int	-- unpack N ptr words from t.o.s Constr
   | UPK_TAG   Int Int Int
			-- unpack N non-ptr words from offset M in constructor
			-- K words down the stack
   | PACK      DataCon Int
			-- after assembly, the DataCon is an index into the
			-- itbl array
   -- For doing case trees
   | LABEL     LocalLabel
   | TESTLT_I  Int    LocalLabel
   | TESTEQ_I  Int    LocalLabel
   | TESTLT_F  Float  LocalLabel
   | TESTEQ_F  Float  LocalLabel
   | TESTLT_D  Double LocalLabel
   | TESTEQ_D  Double LocalLabel

   -- The Int value is a constructor number and therefore
   -- stored in the insn stream rather than as an offset into
   -- the literal pool.
   | TESTLT_P  Int    LocalLabel
   | TESTEQ_P  Int    LocalLabel

   | CASEFAIL
   -- To Infinity And Beyond
   | ENTER
   | RETURN	PrimRep
		-- unboxed value on TOS.  Use tag to find underlying ret itbl
		-- and return as per that.


instance Outputable BCInstr where
   ppr (ARGCHECK n)          = text "ARGCHECK" <+> int n
   ppr (PUSH_L offset)       = text "PUSH_L  " <+> int offset
   ppr (PUSH_LL o1 o2)       = text "PUSH_LL " <+> int o1 <+> int o2
   ppr (PUSH_LLL o1 o2 o3)   = text "PUSH_LLL" <+> int o1 <+> int o2 <+> int o3
   ppr (PUSH_G nm)           = text "PUSH_G  " <+> ppr nm
   ppr (PUSH_AS nm pk)       = text "PUSH_AS " <+> ppr nm <+> ppr pk
   ppr (PUSH_UBX lit nw)     = text "PUSH_UBX" <+> parens (int nw) <+> ppr lit
   ppr (PUSH_TAG n)          = text "PUSH_TAG" <+> int n
   ppr (SLIDE n d)           = text "SLIDE   " <+> int n <+> int d
   ppr (ALLOC sz)            = text "ALLOC   " <+> int sz
   ppr (MKAP offset sz)      = text "MKAP    " <+> int offset <+> int sz
   ppr (UNPACK sz)           = text "UNPACK  " <+> int sz
   ppr (UPK_TAG n m k)       = text "UPK_TAG " <+> int n <> text "words" 
                                               <+> int m <> text "conoff"
                                               <+> int k <> text "stkoff"
   ppr (PACK dcon sz)        = text "PACK    " <+> ppr dcon <+> ppr sz
   ppr (LABEL     lab)       = text "__"       <> int lab <> colon
   ppr (TESTLT_I  i lab)     = text "TESTLT_I" <+> int i <+> text "__" <> int lab
   ppr (TESTEQ_I  i lab)     = text "TESTEQ_I" <+> int i <+> text "__" <> int lab
   ppr (TESTLT_F  f lab)     = text "TESTLT_F" <+> float f <+> text "__" <> int lab
   ppr (TESTEQ_F  f lab)     = text "TESTEQ_F" <+> float f <+> text "__" <> int lab
   ppr (TESTLT_D  d lab)     = text "TESTLT_D" <+> double d <+> text "__" <> int lab
   ppr (TESTEQ_D  d lab)     = text "TESTEQ_D" <+> double d <+> text "__" <> int lab
   ppr (TESTLT_P  i lab)     = text "TESTLT_P" <+> int i <+> text "__" <> int lab
   ppr (TESTEQ_P  i lab)     = text "TESTEQ_P" <+> int i <+> text "__" <> int lab
   ppr CASEFAIL              = text "CASEFAIL"
   ppr ENTER                 = text "ENTER"
   ppr (RETURN pk)           = text "RETURN  " <+> ppr pk

instance Outputable a => Outputable (ProtoBCO a) where
   ppr (ProtoBCO name instrs origin)
      = (text "ProtoBCO" <+> ppr name <> colon)
        $$ nest 6 (vcat (map ppr instrs))
        $$ case origin of
              Left alts -> vcat (map (pprCoreAlt.deAnnAlt) alts)
              Right rhs -> pprCoreExpr (deAnnotate rhs)
\end{code}

%************************************************************************
%*									*
\subsection{Compilation schema for the bytecode generator.}
%*									*
%************************************************************************

\begin{code}

type BCInstrList = OrdList BCInstr

data ProtoBCO a 
   = ProtoBCO a 			-- name, in some sense
              [BCInstr] 		-- instrs
					-- what the BCO came from
              (Either [AnnAlt Id VarSet]
                      (AnnExpr Id VarSet))

nameOfProtoBCO (ProtoBCO nm insns origin) = nm


type Sequel = Int	-- back off to this depth before ENTER

-- Maps Ids to the offset from the stack _base_ so we don't have
-- to mess with it after each push/pop.
type BCEnv = FiniteMap Id Int	-- To find vars on the stack


-- Create a BCO and do a spot of peephole optimisation on the insns
-- at the same time.
mkProtoBCO nm instrs_ordlist origin
   = ProtoBCO nm (peep (fromOL instrs_ordlist)) origin
     where
        peep (PUSH_L off1 : PUSH_L off2 : PUSH_L off3 : rest)
           = PUSH_LLL off1 (off2-1) (off3-2) : peep rest
        peep (PUSH_L off1 : PUSH_L off2 : rest)
           = PUSH_LL off1 off2 : peep rest
        peep (i:rest)
           = i : peep rest
        peep []
           = []


-- Compile code for the right hand side of a let binding.
-- Park the resulting BCO in the monad.  Also requires the
-- variable to which this value was bound, so as to give the
-- resulting BCO a name.
schemeR :: (Id, AnnExpr Id VarSet) -> BcM ()
schemeR (nm, rhs) = schemeR_wrk rhs nm (collect [] rhs)

collect xs (_, AnnLam x e) 
   = collect (if isTyVar x then xs else (x:xs)) e
collect xs not_lambda
   = (reverse xs, not_lambda)

schemeR_wrk original_body nm (args, body)
   = let fvs       = filter (not.isTyVar) (varSetElems (fst original_body))
         all_args  = fvs ++ reverse args
         szsw_args = map taggedIdSizeW all_args
         szw_args  = sum szsw_args
         p_init    = listToFM (zip all_args (mkStackOffsets 0 szsw_args))
         argcheck  = if null args then nilOL else unitOL (ARGCHECK szw_args)
     in
     schemeE szw_args 0 p_init body 		`thenBc` \ body_code ->
     emitBc (mkProtoBCO (getName nm) (appOL argcheck body_code) (Right original_body))

-- Let szsw be the sizes in words of some items pushed onto the stack,
-- which has initial depth d'.  Return the values which the stack environment
-- should map these items to.
mkStackOffsets :: Int -> [Int] -> [Int]
mkStackOffsets original_depth szsw
   = map (subtract 1) (tail (scanl (+) original_depth szsw))

-- Compile code to apply the given expression to the remaining args
-- on the stack, returning a HNF.
schemeE :: Int -> Sequel -> BCEnv -> AnnExpr Id VarSet -> BcM BCInstrList

-- Delegate tail-calls to schemeT.
schemeE d s p e@(fvs, AnnApp f a) 
   = returnBc (schemeT (should_args_be_tagged e) d s 0 p (fvs, AnnApp f a))
schemeE d s p e@(fvs, AnnVar v)
   | isFollowableRep v_rep
   = returnBc (schemeT (should_args_be_tagged e) d s 0 p (fvs, AnnVar v))
   | otherwise
   = -- returning an unboxed value.  Heave it on the stack, SLIDE, and RETURN.
     let (push, szw) = pushAtom True d p (AnnVar v)
     in  returnBc (push 			-- value onto stack
                   `snocOL` SLIDE szw (d-s) 	-- clear to sequel
                   `snocOL` RETURN v_rep)	-- go
   where
      v_rep = typePrimRep (idType v)

schemeE d s p (fvs, AnnLit literal)
   = let (push, szw) = pushAtom True d p (AnnLit literal)
         l_rep = literalPrimRep literal
     in  returnBc (push 			-- value onto stack
                   `snocOL` SLIDE szw (d-s) 	-- clear to sequel
                   `snocOL` RETURN l_rep)		-- go

schemeE d s p (fvs, AnnLet binds b)
   = let (xs,rhss) = case binds of AnnNonRec x rhs  -> ([x],[rhs])
                                   AnnRec xs_n_rhss -> unzip xs_n_rhss
         n     = length xs
         fvss  = map (filter (not.isTyVar).varSetElems.fst) rhss
         sizes = map (\rhs_fvs -> 1 + sum (map taggedIdSizeW rhs_fvs)) fvss

         -- This p', d' defn is safe because all the items being pushed
         -- are ptrs, so all have size 1.  d' and p' reflect the stack
         -- after the closures have been allocated in the heap (but not
         -- filled in), and pointers to them parked on the stack.
         p'    = addListToFM p (zipE xs (mkStackOffsets d (nOfThem n 1)))
         d'    = d + n

         infos = zipE4 fvss sizes xs [n, n-1 .. 1]
         zipE  = zipEqual "schemeE"
         zipE4 = zipWith4Equal "schemeE" (\a b c d -> (a,b,c,d))

         -- ToDo: don't build thunks for things with no free variables
         buildThunk dd ([], size, id, off)
            = PUSH_G (getName id) 
              `consOL` unitOL (MKAP (off+size-1) size)
         buildThunk dd ((fv:fvs), size, id, off)
            = case pushAtom True dd p' (AnnVar fv) of
                 (push_code, pushed_szw)
                    -> push_code `appOL`
                       buildThunk (dd+pushed_szw) (fvs, size, id, off)

         thunkCode = concatOL (map (buildThunk d') infos)
         allocCode = toOL (map ALLOC sizes)
     in
     schemeE d' s p' b   				`thenBc`  \ bodyCode ->
     mapBc schemeR (zip xs rhss) 			`thenBc_`
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
        p' = addToFM p bndr (d' - 1)

        scrut_primrep = typePrimRep (idType bndr)
        isAlgCase
           = case scrut_primrep of
                IntRep -> False ; FloatRep -> False ; DoubleRep -> False
                PtrRep -> True
                other  -> pprPanic "ByteCodeGen.schemeE" (ppr other)

        -- given an alt, return a discr and code for it.
        codeAlt alt@(discr, binds_f, rhs)
           | isAlgCase 
           = let binds_r      = reverse binds_f
                 binds_r_szsw = map untaggedIdSizeW binds_r
                 binds_szw    = sum binds_r_szsw
                 p''          = addListToFM 
                                   p' (zip binds_r (mkStackOffsets d' binds_r_szsw))
                 d''          = d' + binds_szw
                 unpack_code  = mkUnpackCode 0 0 (map (typePrimRep.idType) binds_f)
             in schemeE d'' s p'' rhs	`thenBc` \ rhs_code -> 
                returnBc (my_discr alt, unpack_code `appOL` rhs_code)
           | otherwise 
           = ASSERT(null binds_f) 
             schemeE d' s p' rhs	`thenBc` \ rhs_code ->
             returnBc (my_discr alt, rhs_code)

        my_discr (DEFAULT, binds, rhs)  = NoDiscr
        my_discr (DataAlt dc, binds, rhs) = DiscrP (dataConTag dc)
        my_discr (LitAlt l, binds, rhs)
           = case l of MachInt i     -> DiscrI (fromInteger i)
                       MachFloat r   -> DiscrF (fromRational r)
                       MachDouble r  -> DiscrD (fromRational r)

        maybe_ncons 
           | not isAlgCase = Nothing
           | otherwise 
           = case [dc | (DataAlt dc, _, _) <- alts] of
                []     -> Nothing
                (dc:_) -> Just (tyConFamilySize (dataConTyCon dc))

     in 
     mapBc codeAlt alts 				`thenBc` \ alt_stuff ->
     mkMultiBranch maybe_ncons alt_stuff		`thenBc` \ alt_final ->
     let 
         alt_bco_name = getName bndr
         alt_bco      = mkProtoBCO alt_bco_name alt_final (Left alts)
     in
     schemeE (d + ret_frame_sizeW) 
             (d + ret_frame_sizeW) p scrut		`thenBc` \ scrut_code ->

     emitBc alt_bco 					`thenBc_`
     returnBc (PUSH_AS alt_bco_name scrut_primrep `consOL` scrut_code)


schemeE d s p (fvs, AnnNote note body)
   = schemeE d s p body

schemeE d s p other
   = pprPanic "ByteCodeGen.schemeE: unhandled case" 
               (pprCoreExpr (deAnnotate other))


-- Compile code to do a tail call.  Doesn't need to be monadic.
schemeT :: Bool 	-- do tagging?
        -> Int 		-- Stack depth
        -> Sequel 	-- Sequel depth
        -> Int 		-- # arg words so far
        -> BCEnv 	-- stack env
        -> AnnExpr Id VarSet 
        -> BCInstrList

schemeT enTag d s narg_words p (_, AnnApp f a)
   = case snd a of
        AnnType _ -> schemeT enTag d s narg_words p f
        other
           -> let (push, arg_words) = pushAtom enTag d p (snd a)
              in push 
                 `appOL` schemeT enTag (d+arg_words) s (narg_words+arg_words) p f

schemeT enTag d s narg_words p (_, AnnVar f)
   | Just con <- isDataConId_maybe f
   = ASSERT(enTag == False)
     PACK con narg_words `consOL` (mkSLIDE 1 (d-s-1) `snocOL` ENTER)
   | otherwise
   = ASSERT(enTag == True)
     let (push, arg_words) = pushAtom True d p (AnnVar f)
     in  push 
         `appOL`  mkSLIDE (narg_words+arg_words) (d - s - narg_words)
         `snocOL` ENTER

mkSLIDE n d 
   = if d == 0 then nilOL else unitOL (SLIDE n d)

should_args_be_tagged (_, AnnVar v)
   = case isDataConId_maybe v of
        Just dcon -> False; Nothing -> True
should_args_be_tagged (_, AnnApp f a)
   = should_args_be_tagged f
should_args_be_tagged (_, other)
   = panic "should_args_be_tagged: tail call to non-con, non-var"


-- Make code to unpack a constructor onto the stack, adding
-- tags for the unboxed bits.  Takes the PrimReps of the constructor's
-- arguments, and a travelling offset along both the constructor
-- (off_h) and the stack (off_s).
mkUnpackCode :: Int -> Int -> [PrimRep] -> BCInstrList
mkUnpackCode off_h off_s [] = nilOL
mkUnpackCode off_h off_s (r:rs)
   | isFollowableRep r
   = let (rs_ptr, rs_nptr) = span isFollowableRep (r:rs)
         ptrs_szw = sum (map untaggedSizeW rs_ptr) 
     in  ASSERT(ptrs_szw == length rs_ptr)
         ASSERT(off_h == 0)
         ASSERT(off_s == 0)
         UNPACK ptrs_szw 
         `consOL` mkUnpackCode (off_h + ptrs_szw) (off_s + ptrs_szw) rs_nptr
   | otherwise
   = case r of
        IntRep    -> approved
        FloatRep  -> approved
        DoubleRep -> approved
     where
        approved = UPK_TAG usizeW off_h off_s   `consOL` theRest
        theRest  = mkUnpackCode (off_h + usizeW) (off_s + tsizeW) rs
        usizeW   = untaggedSizeW r
        tsizeW   = taggedSizeW r

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
-- NB (further) that the env p must map each variable to the highest-
-- numbered stack slot for it.  For example, if the stack has depth 4 
-- and we tagged-ly push (v :: Int#) on it, the value will be in stack[4],
-- the tag in stack[5], the stack will have depth 6, and p must map v to
-- 5 and not to 4.  Stack locations are numbered from zero, so a depth
-- 6 stack has valid words 0 .. 5.

pushAtom :: Bool -> Int -> BCEnv -> AnnExpr' Id VarSet -> (BCInstrList, Int)
pushAtom tagged d p (AnnVar v) 
   = let str = "\npushAtom " ++ showSDocDebug (ppr v) ++ ", depth = " ++ show d
               ++ ", env =\n" ++ 
               showSDocDebug (nest 4 (vcat (map ppr (fmToList p))))
               ++ " -->\n" ++
               showSDoc (nest 4 (vcat (map ppr (fromOL (fst result)))))
               ++ "\nendPushAtom " ++ showSDocDebug (ppr v)
         str' = if str == str then str else str

         result
            = case lookupBCEnv_maybe p v of
                 Just d_v -> (toOL (nOfThem nwords (PUSH_L (d-d_v+sz_t-2))), sz_t)
                 Nothing  -> ASSERT(sz_t == 1) (unitOL (PUSH_G nm), sz_t)

         nm     = getName v
         sz_t   = taggedIdSizeW v
         sz_u   = untaggedIdSizeW v
         nwords = if tagged then sz_t else sz_u
     in
         --trace str'
         result

pushAtom True d p (AnnLit lit)
   = let (ubx_code, ubx_size) = pushAtom False d p (AnnLit lit)
     in  (ubx_code `snocOL` PUSH_TAG ubx_size, 1 + ubx_size)

pushAtom False d p (AnnLit lit)
   = case lit of
        MachInt i    -> code IntRep
        MachFloat r  -> code FloatRep
        MachDouble r -> code DoubleRep
     where
        code rep
           = let size_host_words = untaggedSizeW rep
             in (unitOL (PUSH_UBX lit size_host_words), size_host_words)

pushAtom tagged d p (AnnApp f (_, AnnType _))
   = pushAtom tagged d p (snd f)

pushAtom tagged d p other
   = pprPanic "ByteCodeGen.pushAtom" 
              (pprCoreExpr (deAnnotate (undefined, other)))


-- Given a bunch of alts code and their discrs, do the donkey work
-- of making a multiway branch using a switch tree.
-- What a load of hassle!
mkMultiBranch :: Maybe Int	-- # datacons in tycon, if alg alt
				-- a hint; generates better code
				-- Nothing is always safe
              -> [(Discr, BCInstrList)] 
              -> BcM BCInstrList
mkMultiBranch maybe_ncons raw_ways
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
                            DiscrP algMinBound,
                            DiscrP algMaxBound )
              }

         (algMinBound, algMaxBound)
            = case maybe_ncons of
                 Just n  -> (fIRST_TAG, fIRST_TAG + n - 1)
                 Nothing -> (minBound, maxBound)

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
-- See comment preceding pushAtom for precise meaning of env contents
--lookupBCEnv :: BCEnv -> Id -> Int
--lookupBCEnv env nm
--   = case lookupFM env nm of
--        Nothing -> pprPanic "lookupBCEnv" 
--                            (ppr nm $$ char ' ' $$ vcat (map ppr (fmToList env)))
--        Just xx -> xx

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
-- Top level assembler fn.
assembleBCO :: ProtoBCO Name -> IO UnlinkedBCO

assembleBCO (ProtoBCO nm instrs origin)
   = let
         -- pass 1: collect up the offsets of the local labels.
         -- Remember that the first insn starts at offset 1 since offset 0
         -- (eventually) will hold the total # of insns.
         label_env = mkLabelEnv emptyFM 1 instrs

         mkLabelEnv env i_offset [] = env
         mkLabelEnv env i_offset (i:is)
            = let new_env 
                     = case i of LABEL n -> addToFM env n i_offset ; _ -> env
              in  mkLabelEnv new_env (i_offset + instrSizeB i) is

         findLabel lab
            = case lookupFM label_env lab of
                 Just bco_offset -> bco_offset
                 Nothing -> pprPanic "assembleBCO.findLabel" (int lab)
     in
     do  -- pass 2: generate the instruction, ptr and nonptr bits
         insns <- return emptySS :: IO (SizedSeq Word16)
         lits  <- return emptySS :: IO (SizedSeq Word)
         ptrs  <- return emptySS :: IO (SizedSeq Name)
         itbls <- return emptySS :: IO (SizedSeq Name)
         let init_asm_state = (insns,lits,ptrs,itbls)
         (final_insns, final_lits, final_ptrs, final_itbls) 
            <- mkBits findLabel init_asm_state instrs         

         return (UnlinkedBCO nm final_insns final_lits final_ptrs final_itbls)

-- instrs nonptrs ptrs itbls
type AsmState = (SizedSeq Word16, SizedSeq Word, SizedSeq Name, SizedSeq Name)

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
               ARGCHECK  n        -> instr2 st i_ARGCHECK n
               PUSH_L    o1       -> instr2 st i_PUSH_L o1
               PUSH_LL   o1 o2    -> instr3 st i_PUSH_LL o1 o2
               PUSH_LLL  o1 o2 o3 -> instr4 st i_PUSH_LLL o1 o2 o3
               PUSH_G    nm       -> do (p, st2) <- ptr st nm
                                        instr2 st2 i_PUSH_G p
               PUSH_AS   nm pk    -> do (p, st2)  <- ptr st nm
                                        (np, st3) <- ctoi_itbl st2 pk
                                        instr3 st3 i_PUSH_AS p np
               PUSH_UBX  lit nws  -> do (np, st2) <- literal st lit
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
               ENTER              -> instr1 st i_ENTER
               RETURN rep         -> do (itbl_no,st2) <- itoc_itbl st rep
                                        instr2 st2 i_RETURN itbl_no

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

       literal st (MachInt j)    = int st (fromIntegral j)
       literal st (MachFloat r)  = float st (fromRational r)
       literal st (MachDouble r) = double st (fromRational r)

       ctoi_itbl st pk
          = addr st ret_itbl_addr
            where
               ret_itbl_addr = case pk of
                                  IntRep    -> stg_ctoi_ret_R1_info
                                  FloatRep  -> stg_ctoi_ret_F1_info
                                  DoubleRep -> stg_ctoi_ret_D1_info
                               where  -- TEMP HACK
                                  stg_ctoi_ret_F1_info = nullAddr
                                  stg_ctoi_ret_D1_info = nullAddr

       itoc_itbl st pk
          = addr st ret_itbl_addr
            where
               ret_itbl_addr = case pk of
                                  IntRep    -> stg_gc_unbx_r1_info
                                  FloatRep  -> stg_gc_f1_info
                                  DoubleRep -> stg_gc_d1_info
                     
foreign label "stg_ctoi_ret_R1_info" stg_ctoi_ret_R1_info :: Addr
--foreign label "stg_ctoi_ret_F1_info" stg_ctoi_ret_F1_info :: Addr
--foreign label "stg_ctoi_ret_D1_info" stg_ctoi_ret_D1_info :: Addr

foreign label "stg_gc_unbx_r1_info" stg_gc_unbx_r1_info :: Addr
foreign label "stg_gc_f1_info"      stg_gc_f1_info :: Addr
foreign label "stg_gc_d1_info"      stg_gc_d1_info :: Addr

-- The size in bytes of an instruction.
instrSizeB :: BCInstr -> Int
instrSizeB instr
   = case instr of
        ARGCHECK _     -> 4
        PUSH_L   _     -> 4
        PUSH_LL  _ _   -> 6
        PUSH_LLL _ _ _ -> 8
        PUSH_G   _     -> 4
        PUSH_AS  _ _   -> 6
        PUSH_UBX _ _   -> 6
        PUSH_TAG _     -> 4
        SLIDE    _ _   -> 6
        ALLOC    _     -> 4
        MKAP     _ _   -> 6
        UNPACK   _     -> 4
        UPK_TAG  _ _ _ -> 8
        PACK     _ _   -> 6
        LABEL    _     -> 0	-- !!
        TESTLT_I _ _   -> 6
        TESTEQ_I _ _   -> 6
        TESTLT_F _ _   -> 6
        TESTEQ_F _ _   -> 6
        TESTLT_D _ _   -> 6
        TESTEQ_D _ _   -> 6
        TESTLT_P _ _   -> 6
        TESTEQ_P _ _   -> 6
        CASEFAIL       -> 2
        ENTER          -> 2
        RETURN   _     -> 4


-- Make lists of host-sized words for literals, so that when the
-- words are placed in memory at increasing addresses, the
-- bit pattern is correct for the host's word size and endianness.
mkLitI :: Int    -> [Word]
mkLitF :: Float  -> [Word]
mkLitD :: Double -> [Word]
mkLitA :: Addr   -> [Word]

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
        arr <- newDoubleArray ((0::Int),0)
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
data BCO# = BCO# ByteArray# 		-- instrs   :: array Word16#
                 ByteArray# 		-- literals :: array Word32#
                 PtrArray# 		-- ptrs     :: Array HValue
                 ByteArray#		-- itbls    :: Array Addr#
-}

GLOBAL_VAR(v_cafTable, [], [HValue])

--addCAF :: HValue -> IO ()
--addCAF x = do xs <- readIORef v_cafTable; writeIORef v_cafTable (x:xs)

--bcosToHValue :: ItblEnv -> ClosureEnv -> UnlinkedBCOExpr -> IO HValue
--bcosToHValue ie ce (root_bco, other_bcos)
--   = do linked_expr <- linkIExpr ie ce (root_bco, other_bcos)
--	return linked_expr


linkBCOs :: ItblEnv -> ClosureEnv -> [UnlinkedBCO] 
         -> IO [HValue]   -- IO [BCO#] really
linkBCOs ie ce binds = mapM (linkBCO ie ce) binds

linkBCO ie ce (UnlinkedBCO nm insnsSS literalsSS ptrsSS itblsSS)
   = do insns    <- listFromSS insnsSS
        literals <- listFromSS literalsSS
        ptrs     <- listFromSS ptrsSS
        itbls    <- listFromSS itblsSS

        let linked_ptrs  = map (lookupCE ce) ptrs
        linked_itbls <- mapM (lookupIE ie) itbls

        let n_insns    = sizeSS insnsSS
            n_literals = sizeSS literalsSS
            n_ptrs     = sizeSS ptrsSS
            n_itbls    = sizeSS itblsSS

        let ptrs_arr = array (0, n_ptrs-1) (indexify linked_ptrs)
                       :: Array Int HValue
            ptrs_parr = case ptrs_arr of Array lo hi parr -> parr

            itbls_arr = array (0, n_itbls-1) (indexify linked_itbls)
                        :: UArray Int Addr
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

        return (unsafeCoerce# bco#)


data BCO = BCO BCO#

newBCO :: ByteArray# -> ByteArray# -> Array# a -> ByteArray# -> IO BCO
newBCO a b c d
   = IO (\s -> case newBCO# a b c d s of (# s1, bco #) -> (# s1, BCO bco #))


lookupCE :: ClosureEnv -> Name -> HValue
lookupCE ce nm 
   = case lookupFM ce nm of
        Just aa -> unsafeCoerce# aa
        Nothing -> pprPanic "ByteCodeGen.lookupCE" (ppr nm)

lookupIE :: ItblEnv -> Name -> IO Addr
lookupIE ie con_nm 
   = case lookupFM ie con_nm of
        Just (Ptr a) -> return a
        Nothing      
           -> do -- try looking up in the object files.
                 m <- lookupSymbol (nameToCLabel con_nm "con_info")
                 case m of
                    Just addr -> return addr
                    Nothing   -> pprPanic "ByteCodeGen.lookupIE" (ppr con_nm)

-- HACK!!!  ToDo: cleaner
nameToCLabel :: Name -> String{-suffix-} -> String
nameToCLabel n suffix
   = _UNPK_(moduleNameFS (rdrNameModule rn)) 
     ++ '_':occNameString(rdrNameOcc rn) ++ '_':suffix
     where rn = toRdrName n


{-
lookupCon ie con = 
  case lookupFM ie con of
    Just (Ptr addr) -> return addr
    Nothing   -> do
	-- try looking up in the object files.
        m <- lookupSymbol (nameToCLabel con "con_info")
	case m of
	    Just addr -> return addr
  	    Nothing   -> pprPanic "linkIExpr" (ppr con)

-- nullary constructors don't have normal _con_info tables.
lookupNullaryCon ie con =
  case lookupFM ie con of
    Just (Ptr addr) -> return (ConApp addr)
    Nothing -> do
	-- try looking up in the object files.
	m <- lookupSymbol (nameToCLabel con "closure")
	case m of
	    Just (A# addr) -> return (Native (unsafeCoerce# addr))
	    Nothing   -> pprPanic "lookupNullaryCon" (ppr con)


lookupNative ce var =
  unsafeInterleaveIO (do
      case lookupFM ce var of
    	Just e  -> return (Native e)
    	Nothing -> do
    	    -- try looking up in the object files.
    	    let lbl = (nameToCLabel var "closure")
    	    m <- lookupSymbol lbl
    	    case m of
    		Just (A# addr)
		    -> do addCAF (unsafeCoerce# addr)
			  return (Native (unsafeCoerce# addr))
    		Nothing   -> pprPanic "linkIExpr" (ppr var)
  )

-- some VarI/VarP refer to top-level interpreted functions; we change
-- them into Natives here.
lookupVar ce f v =
  unsafeInterleaveIO (
	case lookupFM ce (getName v) of
	    Nothing -> return (f v)
	    Just e  -> return (Native e)
  )
-}
\end{code}

%************************************************************************
%*									*
\subsection{Manufacturing of info tables for DataCons}
%*									*
%************************************************************************

\begin{code}

#if __GLASGOW_HASKELL__ <= 408
type ItblPtr = Addr
#else
type ItblPtr = Ptr StgInfoTable
#endif

-- Make info tables for the data decls in this module
mkITbls :: [TyCon] -> IO ItblEnv
mkITbls [] = return emptyFM
mkITbls (tc:tcs) = do itbls  <- mkITbl tc
                      itbls2 <- mkITbls tcs
                      return (itbls `plusFM` itbls2)

mkITbl :: TyCon -> IO ItblEnv
mkITbl tc
--   | trace ("TYCON: " ++ showSDoc (ppr tc)) False
--   = error "?!?!"
   | not (isDataTyCon tc) 
   = return emptyFM
   | n == length dcs  -- paranoia; this is an assertion.
   = make_constr_itbls dcs
     where
        dcs = tyConDataCons tc
        n   = tyConFamilySize tc

cONSTR :: Int
cONSTR = 1  -- as defined in ghc/includes/ClosureTypes.h

-- Assumes constructors are numbered from zero, not one
make_constr_itbls :: [DataCon] -> IO ItblEnv
make_constr_itbls cons
   | length cons <= 8
   = do is <- mapM mk_vecret_itbl (zip cons [0..])
	return (listToFM is)
   | otherwise
   = do is <- mapM mk_dirret_itbl (zip cons [0..])
	return (listToFM is)
     where
        mk_vecret_itbl (dcon, conNo)
           = mk_itbl dcon conNo (vecret_entry conNo)
        mk_dirret_itbl (dcon, conNo)
           = mk_itbl dcon conNo stg_interp_constr_entry

        mk_itbl :: DataCon -> Int -> Addr -> IO (Name,ItblPtr)
        mk_itbl dcon conNo entry_addr
           = let (tot_wds, ptr_wds, _) 
                    = mkVirtHeapOffsets typePrimRep (dataConRepArgTys dcon)
                 ptrs = ptr_wds
                 nptrs  = tot_wds - ptr_wds
                 itbl  = StgInfoTable {
                           ptrs = fromIntegral ptrs, nptrs = fromIntegral nptrs,
                           tipe = fromIntegral cONSTR,
                           srtlen = fromIntegral conNo,
                           code0 = fromIntegral code0, code1 = fromIntegral code1,
                           code2 = fromIntegral code2, code3 = fromIntegral code3,
                           code4 = fromIntegral code4, code5 = fromIntegral code5,
                           code6 = fromIntegral code6, code7 = fromIntegral code7 
                        }
                 -- Make a piece of code to jump to "entry_label".
                 -- This is the only arch-dependent bit.
                 -- On x86, if entry_label has an address 0xWWXXYYZZ,
                 -- emit   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
                 -- which is
                 -- B8 ZZ YY XX WW FF E0
                 (code0,code1,code2,code3,code4,code5,code6,code7)
                    = (0xB8, byte 0 entry_addr_w, byte 1 entry_addr_w, 
                             byte 2 entry_addr_w, byte 3 entry_addr_w, 
                       0xFF, 0xE0, 
                       0x90 {-nop-})

                 entry_addr_w :: Word32
                 entry_addr_w = fromIntegral (addrToInt entry_addr)
             in
                 do addr <- malloc
                    --putStrLn ("SIZE of itbl is " ++ show (sizeOf itbl))
                    --putStrLn ("# ptrs  of itbl is " ++ show ptrs)
                    --putStrLn ("# nptrs of itbl is " ++ show nptrs)
                    poke addr itbl
                    return (getName dcon, addr `plusPtr` 8)


byte :: Int -> Word32 -> Word32
byte 0 w = w .&. 0xFF
byte 1 w = (w `shiftR` 8) .&. 0xFF
byte 2 w = (w `shiftR` 16) .&. 0xFF
byte 3 w = (w `shiftR` 24) .&. 0xFF


vecret_entry 0 = stg_interp_constr1_entry
vecret_entry 1 = stg_interp_constr2_entry
vecret_entry 2 = stg_interp_constr3_entry
vecret_entry 3 = stg_interp_constr4_entry
vecret_entry 4 = stg_interp_constr5_entry
vecret_entry 5 = stg_interp_constr6_entry
vecret_entry 6 = stg_interp_constr7_entry
vecret_entry 7 = stg_interp_constr8_entry

-- entry point for direct returns for created constr itbls
foreign label "stg_interp_constr_entry" stg_interp_constr_entry :: Addr
-- and the 8 vectored ones
foreign label "stg_interp_constr1_entry" stg_interp_constr1_entry :: Addr
foreign label "stg_interp_constr2_entry" stg_interp_constr2_entry :: Addr
foreign label "stg_interp_constr3_entry" stg_interp_constr3_entry :: Addr
foreign label "stg_interp_constr4_entry" stg_interp_constr4_entry :: Addr
foreign label "stg_interp_constr5_entry" stg_interp_constr5_entry :: Addr
foreign label "stg_interp_constr6_entry" stg_interp_constr6_entry :: Addr
foreign label "stg_interp_constr7_entry" stg_interp_constr7_entry :: Addr
foreign label "stg_interp_constr8_entry" stg_interp_constr8_entry :: Addr





-- Ultra-minimalist version specially for constructors
data StgInfoTable = StgInfoTable {
   ptrs :: Word16,
   nptrs :: Word16,
   srtlen :: Word16,
   tipe :: Word16,
   code0, code1, code2, code3, code4, code5, code6, code7 :: Word8
}


instance Storable StgInfoTable where

   sizeOf itbl 
      = (sum . map (\f -> f itbl))
        [fieldSz ptrs, fieldSz nptrs, fieldSz srtlen, fieldSz tipe,
         fieldSz code0, fieldSz code1, fieldSz code2, fieldSz code3, 
         fieldSz code4, fieldSz code5, fieldSz code6, fieldSz code7]

   alignment itbl 
      = (sum . map (\f -> f itbl))
        [fieldAl ptrs, fieldAl nptrs, fieldAl srtlen, fieldAl tipe,
         fieldAl code0, fieldAl code1, fieldAl code2, fieldAl code3, 
         fieldAl code4, fieldAl code5, fieldAl code6, fieldAl code7]

   poke a0 itbl
      = do a1 <- store (ptrs   itbl) (castPtr a0)
           a2 <- store (nptrs  itbl) a1
           a3 <- store (tipe   itbl) a2
           a4 <- store (srtlen itbl) a3
           a5 <- store (code0  itbl) a4
           a6 <- store (code1  itbl) a5
           a7 <- store (code2  itbl) a6
           a8 <- store (code3  itbl) a7
           a9 <- store (code4  itbl) a8
           aA <- store (code5  itbl) a9
           aB <- store (code6  itbl) aA
           aC <- store (code7  itbl) aB
           return ()

   peek a0
      = do (a1,ptrs)   <- load (castPtr a0)
           (a2,nptrs)  <- load a1
           (a3,tipe)   <- load a2
           (a4,srtlen) <- load a3
           (a5,code0)  <- load a4
           (a6,code1)  <- load a5
           (a7,code2)  <- load a6
           (a8,code3)  <- load a7
           (a9,code4)  <- load a8
           (aA,code5)  <- load a9
           (aB,code6)  <- load aA
           (aC,code7)  <- load aB
           return StgInfoTable { ptrs = ptrs, nptrs = nptrs, 
                                 srtlen = srtlen, tipe = tipe,
                                 code0 = code0, code1 = code1, code2 = code2,
                                 code3 = code3, code4 = code4, code5 = code5,
                                 code6 = code6, code7 = code7 }

fieldSz :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldSz sel x = sizeOf (sel x)

fieldAl :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldAl sel x = alignment (sel x)

store :: Storable a => a -> Ptr a -> IO (Ptr b)
store x addr = do poke addr x
                  return (castPtr (addr `plusPtr` sizeOf x))

load :: Storable a => Ptr a -> IO (Ptr b, a)
load addr = do x <- peek addr
               return (castPtr (addr `plusPtr` sizeOf x), x)

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

\end{code}
