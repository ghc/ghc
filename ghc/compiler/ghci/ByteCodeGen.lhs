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
import Name		( Name, getName, mkSysLocalName )
import Id		( Id, idType, isDataConId_maybe, mkVanillaId,
			  isPrimOpId_maybe, idPrimRep )
import OrdList		( OrdList, consOL, snocOL, appOL, unitOL, 
			  nilOL, toOL, concatOL, fromOL )
import FiniteMap	( FiniteMap, addListToFM, listToFM,
			  addToFM, lookupFM, fmToList, plusFM )
import CoreSyn
import PprCore		( pprCoreExpr )
import Literal		( Literal(..), literalPrimRep )
import PrimRep		( PrimRep(..) )
import PrimOp		( PrimOp(..)  )
import CoreFVs		( freeVars )
import Type		( typePrimRep )
import DataCon		( dataConTag, fIRST_TAG, dataConTyCon, dataConWrapId )
import TyCon		( TyCon, tyConFamilySize )
import Class		( Class, classTyCon )
import Util		( zipEqual, zipWith4Equal, naturalMergeSortLe, nOfThem )
import Var		( isTyVar )
import VarSet		( VarSet, varSetElems )
import PrimRep		( getPrimRepSize, isFollowableRep )
import CmdLineOpts	( DynFlags, DynFlag(..) )
import ErrUtils		( showPass, dumpIfSet_dyn )
import Unique		( mkPseudoUnique3 )
import FastString	( FastString(..) )
import PprType		( pprType )
import ByteCodeInstr	( BCInstr(..), ProtoBCO(..), nameOfProtoBCO )
import ByteCodeItbls	( ItblEnv, mkITbls )
import ByteCodeLink	( UnlinkedBCO, UnlinkedBCOExpr, assembleBCO,
			  ClosureEnv, HValue, linkSomeBCOs, filterNameMap )

import List		( intersperse, sortBy )
import Foreign		( Ptr(..), mallocBytes )
import Addr		( Addr(..), addrToInt, writeCharOffAddr )
import CTypes		( CInt )

import PrelBase		( Int(..) )
import PrelGHC		( ByteArray# )
import IOExts		( unsafePerformIO )
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
                                (mapBc (schemeR True) flatBinds
					`thenBc_` returnBc ())
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
                     (schemeR True (invented_id, freeVars expr))
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
linkIModules gie gce mods 
   = do let (bcoss, ies) = unzip mods
            bcos = concat bcoss
            final_gie = foldr plusFM gie ies
        (final_gce, linked_bcos) <- linkSomeBCOs final_gie gce bcos
        return (linked_bcos, final_gie, final_gce)


linkIExpr :: ItblEnv -> ClosureEnv -> UnlinkedBCOExpr
          -> IO HValue 	  -- IO BCO# really
linkIExpr ie ce (root_ul_bco, aux_ul_bcos)
   = do (aux_ce, _) <- linkSomeBCOs ie ce aux_ul_bcos
        (_, [root_bco]) <- linkSomeBCOs ie aux_ce [root_ul_bco]
        return root_bco
\end{code}

%************************************************************************
%*									*
\subsection{Compilation schema for the bytecode generator.}
%*									*
%************************************************************************

\begin{code}

type BCInstrList = OrdList BCInstr

type Sequel = Int	-- back off to this depth before ENTER

-- Maps Ids to the offset from the stack _base_ so we don't have
-- to mess with it after each push/pop.
type BCEnv = FiniteMap Id Int	-- To find vars on the stack

ppBCEnv :: BCEnv -> SDoc
ppBCEnv p
   = text "begin-env"
     $$ nest 4 (vcat (map pp_one (sortBy cmp_snd (fmToList p))))
     $$ text "end-env"
     where
        pp_one (var, offset) = int offset <> colon <+> ppr var
        cmp_snd x y = compare (snd x) (snd y)

-- Create a BCO and do a spot of peephole optimisation on the insns
-- at the same time.
mkProtoBCO nm instrs_ordlist origin
   = ProtoBCO nm (peep (fromOL instrs_ordlist)) origin
     where
        peep (PUSH_L off1 : PUSH_L off2 : PUSH_L off3 : rest)
           = PUSH_LLL off1 (off2-1) (off3-2) : peep rest
        peep (PUSH_L off1 : PUSH_L off2 : rest)
           = PUSH_LL off1 (off2-1) : peep rest
        peep (i:rest)
           = i : peep rest
        peep []
           = []


-- Compile code for the right hand side of a let binding.
-- Park the resulting BCO in the monad.  Also requires the
-- variable to which this value was bound, so as to give the
-- resulting BCO a name.  Bool indicates top-levelness.

schemeR :: Bool -> (Id, AnnExpr Id VarSet) -> BcM ()
schemeR is_top (nm, rhs) 
{-
   | trace (showSDoc (
              (char ' '
               $$ (ppr.filter (not.isTyVar).varSetElems.fst) rhs
               $$ pprCoreExpr (deAnnotate rhs)
               $$ char ' '
              ))) False
   = undefined
-}
   | otherwise
   = schemeR_wrk is_top rhs nm (collect [] rhs)


collect xs (_, AnnNote note e)
   = collect xs e
collect xs (_, AnnLam x e) 
   = collect (if isTyVar x then xs else (x:xs)) e
collect xs not_lambda
   = (reverse xs, not_lambda)

schemeR_wrk is_top original_body nm (args, body)
   | Just dcon <- maybe_toplevel_null_con_rhs
   = --trace ("nullary constructor! " ++ showSDocDebug (ppr nm)) (
     emitBc (mkProtoBCO (getName nm) (toOL [PACK dcon 0, ENTER])
                                     (Right original_body))
     --)

   | otherwise
   = let fvs       = filter (not.isTyVar) (varSetElems (fst original_body))
         all_args  = reverse args ++ fvs
         szsw_args = map taggedIdSizeW all_args
         szw_args  = sum szsw_args
         p_init    = listToFM (zip all_args (mkStackOffsets 0 szsw_args))
         argcheck  = unitOL (ARGCHECK szw_args)
     in
     schemeE szw_args 0 p_init body 		`thenBc` \ body_code ->
     emitBc (mkProtoBCO (getName nm) (appOL argcheck body_code) 
                                     (Right original_body))

     where
        maybe_toplevel_null_con_rhs
           | is_top && null args
           = case snd body of
                AnnVar v_wrk 
                   -> case isDataConId_maybe v_wrk of
                         Nothing -> Nothing
                         Just dc_wrk |  nm == dataConWrapId dc_wrk
                                     -> Just dc_wrk
                                     |  otherwise 
                                     -> Nothing
                other -> Nothing
           | otherwise
           = Nothing

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
   = returnBc (schemeT d s p (fvs, AnnApp f a))
schemeE d s p e@(fvs, AnnVar v)
   | isFollowableRep v_rep
   = returnBc (schemeT d s p (fvs, AnnVar v))

   | otherwise
   = -- returning an unboxed value.  Heave it on the stack, SLIDE, and RETURN.
     let (push, szw) = pushAtom True d p (AnnVar v)
     in  returnBc (push 			-- value onto stack
                   `appOL`  mkSLIDE szw (d-s) 	-- clear to sequel
                   `snocOL` RETURN v_rep)	-- go
   where
      v_rep = typePrimRep (idType v)

schemeE d s p (fvs, AnnLit literal)
   = let (push, szw) = pushAtom True d p (AnnLit literal)
         l_rep = literalPrimRep literal
     in  returnBc (push 			-- value onto stack
                   `appOL`  mkSLIDE szw (d-s) 	-- clear to sequel
                   `snocOL` RETURN l_rep)	-- go

schemeE d s p (fvs, AnnLet binds b)
   = let (xs,rhss) = case binds of AnnNonRec x rhs  -> ([x],[rhs])
                                   AnnRec xs_n_rhss -> unzip xs_n_rhss
         n     = length xs
         fvss  = map (filter (not.isTyVar).varSetElems.fst) rhss

         -- Sizes of tagged free vars, + 1 for the fn
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
            = PUSH_G (Left (getName id))
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
     mapBc (schemeR False) (zip xs rhss)		`thenBc_`
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
                CharRep -> False ; AddrRep -> False ; WordRep -> False
                IntRep -> False ; FloatRep -> False ; DoubleRep -> False
                PtrRep -> True
                other  -> pprPanic "ByteCodeGen.schemeE" (ppr other)

        -- given an alt, return a discr and code for it.
        codeAlt alt@(discr, binds_f, rhs)
           | isAlgCase 
           = let (unpack_code, d_after_unpack, p_after_unpack)
                    = mkUnpackCode binds_f d' p'
             in  schemeE d_after_unpack s p_after_unpack rhs
					`thenBc` \ rhs_code -> 
                 returnBc (my_discr alt, unpack_code `appOL` rhs_code)
           | otherwise 
           = ASSERT(null binds_f) 
             schemeE d' s p' rhs	`thenBc` \ rhs_code ->
             returnBc (my_discr alt, rhs_code)

        my_discr (DEFAULT, binds, rhs)  = NoDiscr
        my_discr (DataAlt dc, binds, rhs) = DiscrP (dataConTag dc - fIRST_TAG)
        my_discr (LitAlt l, binds, rhs)
           = case l of MachInt i     -> DiscrI (fromInteger i)
                       MachFloat r   -> DiscrF (fromRational r)
                       MachDouble r  -> DiscrD (fromRational r)
                       MachChar i    -> DiscrI i
                       _ -> pprPanic "schemeE(AnnCase).my_discr" (ppr l)

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
         alt_final_ac = ARGCHECK (taggedIdSizeW bndr) `consOL` alt_final
         alt_bco_name = getName bndr
         alt_bco      = mkProtoBCO alt_bco_name alt_final_ac (Left alts)
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


-- Compile code to do a tail call.  Three cases:
--
-- 1.  A nullary constructor.  Push its closure on the stack 
--     and SLIDE and RETURN.
--
-- 2.  Application of a non-nullary constructor, by defn saturated.
--     Split the args into ptrs and non-ptrs, and push the nonptrs, 
--     then the ptrs, and then do PACK and RETURN.
--
-- 3.  Otherwise, it must be a function call.  Push the args
--     right to left, SLIDE and ENTER.

schemeT :: Int 		-- Stack depth
        -> Sequel 	-- Sequel depth
        -> BCEnv 	-- stack env
        -> AnnExpr Id VarSet 
        -> BCInstrList

schemeT d s p app
--   | trace ("schemeT: env in = \n" ++ showSDocDebug (ppBCEnv p)) False
--   = panic "schemeT ?!?!"

   -- Handle case 1
   | is_con_call && null args_r_to_l
   = (PUSH_G (Left (getName con)) `consOL` mkSLIDE 1 (d-s))
     `snocOL` ENTER

   -- Cases 2 and 3
   | otherwise
   = code

     where
         -- Extract the args (R->L) and fn
         (args_r_to_l_raw, fn) = chomp app
         chomp expr
            = case snd expr of
                 AnnVar v    -> ([], v)
                 AnnApp f a  -> case chomp f of (az, f) -> (snd a:az, f)
                 AnnNote n e -> chomp e
                 other       -> pprPanic "schemeT" 
                                   (ppr (deAnnotate (panic "schemeT.chomp", other)))
         
         args_r_to_l = filter (not.isTypeAtom) args_r_to_l_raw
         isTypeAtom (AnnType _) = True
         isTypeAtom _           = False

         -- decide if this is a constructor call, and rearrange
         -- args appropriately.
         maybe_dcon  = isDataConId_maybe fn
         is_con_call = case maybe_dcon of Nothing -> False; Just _ -> True
         (Just con)  = maybe_dcon

         args_final_r_to_l
            | not is_con_call
            = args_r_to_l
            | otherwise
            = filter (not.isPtr) args_r_to_l ++ filter isPtr args_r_to_l
              where isPtr = isFollowableRep . atomRep

         -- make code to push the args and then do the SLIDE-ENTER thing
         code = do_pushery d args_final_r_to_l

         tag_when_push = not is_con_call
         narg_words    = sum (map (get_arg_szw . atomRep) args_r_to_l)
         get_arg_szw   = if tag_when_push then taggedSizeW else untaggedSizeW

         do_pushery d (arg:args)
            = let (push, arg_words) = pushAtom tag_when_push d p arg
              in  push `appOL` do_pushery (d+arg_words) args
         do_pushery d []
            = case maybe_dcon of
                 Just con -> PACK con narg_words `consOL` (
                             mkSLIDE 1 (d - narg_words - s) `snocOL` ENTER)
                 Nothing
                    -> let (push, arg_words) = pushAtom True d p (AnnVar fn)
                       in  push 
                           `appOL` mkSLIDE (narg_words+arg_words) 
                                           (d - s - narg_words)
                           `snocOL` ENTER

mkSLIDE n d 
   = if d == 0 then nilOL else unitOL (SLIDE n d)

atomRep (AnnVar v)    = typePrimRep (idType v)
atomRep (AnnLit l)    = literalPrimRep l
atomRep (AnnNote n b) = atomRep (snd b)
atomRep (AnnApp f (_, AnnType _)) = atomRep (snd f)
atomRep (AnnLam x e) | isTyVar x = atomRep (snd e)
atomRep other = pprPanic "atomRep" (ppr (deAnnotate (undefined,other)))


-- Make code to unpack the top-of-stack constructor onto the stack, 
-- adding tags for the unboxed bits.  Takes the PrimReps of the 
-- constructor's arguments.  off_h and off_s are travelling offsets
-- along the constructor and the stack.
--
-- Supposing a constructor in the heap has layout
--
--      Itbl p_1 ... p_i np_1 ... np_j
--
-- then we add to the stack, shown growing down, the following:
--
--    (previous stack)
--         p_i
--         ...
--         p_1
--         np_j
--         tag_for(np_j)
--         ..
--         np_1
--         tag_for(np_1)
--
-- so that in the common case (ptrs only) a single UNPACK instr can
-- copy all the payload of the constr onto the stack with no further ado.

mkUnpackCode :: [Id] 	-- constr args
             -> Int 	-- depth before unpack
             -> BCEnv 	-- env before unpack
             -> (BCInstrList, Int, BCEnv)
mkUnpackCode vars d p
   = --trace ("mkUnpackCode: " ++ showSDocDebug (ppr vars)
     --       ++ " --> " ++ show d' ++ "\n" ++ showSDocDebug (ppBCEnv p')
     --       ++ "\n") (
     (code_p `appOL` code_np, d', p')
     --)
     where
        -- vars with reps
        vreps = [(var, typePrimRep (idType var)) | var <- vars]

        -- ptrs and nonptrs, forward
        vreps_p  = filter (isFollowableRep.snd) vreps
        vreps_np = filter (not.isFollowableRep.snd) vreps

        -- the order in which we will augment the environment
        vreps_env = reverse vreps_p ++ reverse vreps_np

        -- new env and depth
        vreps_env_tszsw = map (taggedSizeW.snd) vreps_env
        p' = addListToFM p (zip (map fst vreps_env) 
                                (mkStackOffsets d vreps_env_tszsw))
        d' = d + sum vreps_env_tszsw

        -- code to unpack the ptrs
        ptrs_szw = sum (map (untaggedSizeW.snd) vreps_p)
        code_p | null vreps_p = nilOL
               | otherwise    = unitOL (UNPACK ptrs_szw)

        -- code to unpack the nonptrs
        vreps_env_uszw = sum (map (untaggedSizeW.snd) vreps_env)
        code_np = do_nptrs vreps_env_uszw ptrs_szw (reverse (map snd vreps_np))
        do_nptrs off_h off_s [] = nilOL
        do_nptrs off_h off_s (npr:nprs)
           = case npr of
                IntRep -> approved ; FloatRep -> approved
                DoubleRep -> approved ; AddrRep -> approved
                CharRep -> approved
                _ -> pprPanic "ByteCodeGen.mkUnpackCode" (ppr npr)
             where
                approved = UPK_TAG usizeW (off_h-usizeW) off_s   `consOL` theRest
                theRest  = do_nptrs (off_h-usizeW) (off_s + tsizeW) nprs
                usizeW   = untaggedSizeW npr
                tsizeW   = taggedSizeW npr


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

   | idPrimRep v == VoidRep
   = ASSERT(tagged)
     (unitOL (PUSH_TAG 0), 1)

   | Just primop <- isPrimOpId_maybe v
   = case primop of
        CCallOp _ -> panic "pushAtom: byte code generator can't handle CCalls"
        other     -> (unitOL (PUSH_G (Right primop)), 1)

   | otherwise
   = let str = "\npushAtom " ++ showSDocDebug (ppr v) 
               ++ " :: " ++ showSDocDebug (pprType (idType v))
               ++ ", depth = " ++ show d
               ++ ", tagged = " ++ show tagged ++ ", env =\n" ++ 
               showSDocDebug (ppBCEnv p)
               ++ " --> words: " ++ show (snd result) ++ "\n" ++
               showSDoc (nest 4 (vcat (map ppr (fromOL (fst result)))))
               ++ "\nendPushAtom " ++ showSDocDebug (ppr v)
                  where
                     cmp_snd x y = compare (snd x) (snd y)
         str' = if str == str then str else str

         result
            = case lookupBCEnv_maybe p v of
                 Just d_v -> (toOL (nOfThem nwords (PUSH_L (d-d_v+sz_t-2))), nwords)
                 Nothing  -> ASSERT(sz_t == 1) (unitOL (PUSH_G (Left nm)), nwords)

         nm = case isDataConId_maybe v of
                 Just c  -> getName c
                 Nothing -> getName v

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
        MachWord w   -> code WordRep
        MachInt i    -> code IntRep
        MachFloat r  -> code FloatRep
        MachDouble r -> code DoubleRep
        MachChar c   -> code CharRep
        MachStr s    -> pushStr s
     where
        code rep
           = let size_host_words = untaggedSizeW rep
             in (unitOL (PUSH_UBX lit size_host_words), size_host_words)

        pushStr s 
           = let mallocvilleAddr
                    = case s of
                         CharStr s i -> A# s

                         FastString _ l ba -> 
                            -- sigh, a string in the heap is no good to us.
                            -- We need a static C pointer, since the type of 
                            -- a string literal is Addr#.  So, copy the string 
                            -- into C land and introduce a memory leak 
                            -- at the same time.
                            let n = I# l
                            -- CAREFUL!  Chars are 32 bits in ghc 4.09+
                            in  unsafePerformIO (
                                   do (Ptr a#) <- mallocBytes (n+1)
                                      strncpy (Ptr a#) ba (fromIntegral n)
                                      writeCharOffAddr (A# a#) n '\0'
                                      return (A# a#)
                                   )
                         _ -> panic "StgInterp.lit2expr: unhandled string constant type"

                 addrLit 
                    = MachInt (toInteger (addrToInt mallocvilleAddr))
             in
                -- Get the addr on the stack, untaggedly
                (unitOL (PUSH_UBX addrLit 1), 1)





pushAtom tagged d p (AnnApp f (_, AnnType _))
   = pushAtom tagged d p (snd f)

pushAtom tagged d p (AnnNote note e)
   = pushAtom tagged d p (snd e)

pushAtom tagged d p (AnnLam x e) 
   | isTyVar x 
   = pushAtom tagged d p (snd e)

pushAtom tagged d p other
   = pprPanic "ByteCodeGen.pushAtom" 
              (pprCoreExpr (deAnnotate (undefined, other)))

foreign import "strncpy" strncpy :: Ptr a -> ByteArray# -> CInt -> IO ()


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
                 Just n  -> (0, n - 1)
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
