%
% (c) The University of Glasgow 2000
%
\section[ByteCodeGen]{Generate bytecode from Core}

\begin{code}
module ByteCodeGen ( UnlinkedBCO, UnlinkedBCOExpr, ItblEnv, ClosureEnv, HValue,
		     filterNameMap,
                     byteCodeGen, coreExprToBCOs
		   ) where

#include "HsVersions.h"

import Outputable
import Name		( Name, getName )
import Id		( Id, idType, isDataConId_maybe, isPrimOpId_maybe, isFCallId,
			  idPrimRep, mkSysLocal, idName, isFCallId_maybe )
import ForeignCall	( ForeignCall(..), CCallTarget(..), CCallSpec(..) )
import OrdList		( OrdList, consOL, snocOL, appOL, unitOL, 
			  nilOL, toOL, concatOL, fromOL )
import FiniteMap	( FiniteMap, addListToFM, listToFM,
			  addToFM, lookupFM, fmToList )
import CoreSyn
import PprCore		( pprCoreExpr )
import Literal		( Literal(..), literalPrimRep )
import PrimRep		( PrimRep(..) )
import PrimOp		( PrimOp(..) )
import CoreFVs		( freeVars )
import Type		( typePrimRep, splitTyConApp_maybe, isTyVarTy )
import DataCon		( dataConTag, fIRST_TAG, dataConTyCon, 
                          dataConWrapId, isUnboxedTupleCon )
import TyCon		( TyCon(..), tyConFamilySize, isDataTyCon, tyConDataCons,
			  isFunTyCon, isUnboxedTupleTyCon )
import Class		( Class, classTyCon )
import Type		( Type, repType, splitRepFunTys )
import Util		( zipEqual, zipWith4Equal, naturalMergeSortLe, nOfThem )
import Var		( isTyVar )
import VarSet		( VarSet, varSetElems )
import PrimRep		( isFollowableRep )
import CmdLineOpts	( DynFlags, DynFlag(..) )
import ErrUtils		( showPass, dumpIfSet_dyn )
import Unique		( mkPseudoUnique3 )
import FastString	( FastString(..) )
import Panic		( GhcException(..) )
import PprType		( pprType )
import SMRep		( arrWordsHdrSize, arrPtrsHdrSize )
import Constants	( wORD_SIZE )
import ByteCodeInstr	( BCInstr(..), ProtoBCO(..), nameOfProtoBCO, bciStackUse )
import ByteCodeItbls	( ItblEnv, mkITbls )
import ByteCodeLink	( UnlinkedBCO, UnlinkedBCOExpr, assembleBCO,
			  ClosureEnv, HValue, filterNameMap, linkFail,
			  iNTERP_STACK_CHECK_THRESH )
import ByteCodeFFI	( taggedSizeW, untaggedSizeW, mkMarshalCode, moan64 )
import Linker		( lookupSymbol )

import List		( intersperse, sortBy, zip4 )
import Foreign		( Ptr(..), mallocBytes )
import Addr		( Addr(..), writeCharOffAddr )
import CTypes		( CInt )
import Exception	( throwDyn )

import PrelBase		( Int(..) )
import PrelGHC		( ByteArray# )
import PrelIOBase	( IO(..) )
import Monad		( when )

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

        (BcM_State proto_bcos final_ctr mallocd, ())
           <- runBc (BcM_State [] 0 []) 
                    (mapBc (schemeR True) flatBinds `thenBc_` returnBc ())

        when (not (null mallocd))
             (panic "ByteCodeGen.byteCodeGen: missing final emitBc?")

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
      let invented_id   = mkSysLocal SLIT("Expr-Top-Level") (mkPseudoUnique3 0) 
				     (panic "invented_id's type")
      let invented_name = idName invented_id

      (BcM_State all_proto_bcos final_ctr mallocd, ()) 
         <- runBc (BcM_State [] 0 []) 
                  (schemeR True (invented_id, freeVars expr))

      when (not (null mallocd))
           (panic "ByteCodeGen.coreExprToBCOs: missing final emitBc?")

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
mkProtoBCO nm instrs_ordlist origin mallocd_blocks
   = ProtoBCO nm maybe_with_stack_check origin mallocd_blocks
     where
        -- Overestimate the stack usage (in words) of this BCO,
        -- and if >= iNTERP_STACK_CHECK_THRESH, add an explicit
        -- stack check.  (The interpreter always does a stack check
        -- for iNTERP_STACK_CHECK_THRESH words at the start of each
        -- BCO anyway, so we only need to add an explicit on in the
        -- (hopefully rare) cases when the (overestimated) stack use
        -- exceeds iNTERP_STACK_CHECK_THRESH.
        maybe_with_stack_check
           | stack_overest >= 65535
           = pprPanic "mkProtoBCO: stack use won't fit in 16 bits" 
                      (int stack_overest)
           | stack_overest >= iNTERP_STACK_CHECK_THRESH
           = (STKCHECK stack_overest) : peep_d
           | otherwise
           = peep_d	-- the supposedly common case
             
        stack_overest = sum (map bciStackUse peep_d)
                        + 10 {- just to be really really sure -}


        -- Merge local pushes
        peep_d = peep (fromOL instrs_ordlist)

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
           = case nukeTyArgs (snd body) of
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

        nukeTyArgs (AnnApp f (_, AnnType _)) = nukeTyArgs (snd f)
        nukeTyArgs other                     = other


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
   = schemeT d s p (fvs, AnnApp f a)

schemeE d s p e@(fvs, AnnVar v)
   | isFollowableRep v_rep
   =  -- Ptr-ish thing; push it in the normal way
     schemeT d s p (fvs, AnnVar v)

   | otherwise
   = -- returning an unboxed value.  Heave it on the stack, SLIDE, and RETURN.
     pushAtom True d p (AnnVar v)	`thenBc` \ (push, szw) ->
     returnBc (push 			-- value onto stack
               `appOL`  mkSLIDE szw (d-s) 	-- clear to sequel
               `snocOL` RETURN v_rep)	-- go
   where
      v_rep = typePrimRep (idType v)

schemeE d s p (fvs, AnnLit literal)
   = pushAtom True d p (AnnLit literal)	`thenBc` \ (push, szw) ->
     let l_rep = literalPrimRep literal
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
            = returnBc (PUSH_G (Left (getName id))
                        `consOL` unitOL (MKAP (off+size-1) size))
         buildThunk dd ((fv:fvs), size, id, off)
            = pushAtom True dd p' (AnnVar fv) 
					`thenBc` \ (push_code, pushed_szw) ->
              buildThunk (dd+pushed_szw) (fvs, size, id, off)
					`thenBc` \ more_push_code ->
              returnBc (push_code `appOL` more_push_code)

         genThunkCode = mapBc (buildThunk d') infos	`thenBc` \ tcodes ->
                        returnBc (concatOL tcodes)

         allocCode = toOL (map ALLOC sizes)
     in
     schemeE d' s p' b   				`thenBc`  \ bodyCode ->
     mapBc (schemeR False) (zip xs rhss)		`thenBc_`
     genThunkCode					`thenBc` \ thunkCode ->
     returnBc (allocCode `appOL` thunkCode `appOL` bodyCode)





schemeE d s p (fvs_case, AnnCase (fvs_scrut, scrut) bndr 
                                 [(DEFAULT, [], (fvs_rhs, rhs))])

   | let isFunType var_type 
            = case splitTyConApp_maybe var_type of
                 Just (tycon,_) | isFunTyCon tycon -> True
                 _ -> False
         ty_bndr = repType (idType bndr)
     in isFunType ty_bndr || isTyVarTy ty_bndr

   -- Nasty hack; treat
   --     case scrut::suspect of bndr { DEFAULT -> rhs }
   --     as 
   --     let bndr = scrut in rhs
   --     when suspect is polymorphic or arrowtyped
   -- So the required strictness properties are not observed.
   -- At some point, must fix this properly.
   = let new_expr
            = (fvs_case, 
               AnnLet 
                  (AnnNonRec bndr (fvs_scrut, scrut)) (fvs_rhs, rhs)
              )

     in  trace ("WARNING: ignoring polymorphic case in interpreted mode.\n" ++
                "   Possibly due to strict polymorphic/functional constructor args.\n" ++
                "   Your program may leak space unexpectedly.\n")
         (schemeE d s p new_expr)



{- Convert case .... of (# VoidRep'd-thing, a #) -> ...
      as
   case .... of a -> ...
   Use  a  as the name of the binder too.

   Also    case .... of (# a #) -> ...
      to
   case .... of a -> ...
-}
schemeE d s p (fvs, AnnCase scrut bndr [(DataAlt dc, [bind1, bind2], rhs)])
   | isUnboxedTupleCon dc && VoidRep == typePrimRep (idType bind1)
   = --trace "automagic mashing of case alts (# VoidRep, a #)" (
     schemeE d s p (fvs, AnnCase scrut bind2 [(DEFAULT, [bind2], rhs)])
     --)

schemeE d s p (fvs, AnnCase scrut bndr [(DataAlt dc, [bind1], rhs)])
   | isUnboxedTupleCon dc
   = --trace "automagic mashing of case alts (# a #)" (
     schemeE d s p (fvs, AnnCase scrut bind1 [(DEFAULT, [bind1], rhs)])
     --)

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
           | scrut_primrep == PtrRep
           = True
           | scrut_primrep `elem`
             [CharRep, AddrRep, WordRep, IntRep, FloatRep, DoubleRep,
              VoidRep, Int8Rep, Int16Rep, Int32Rep, Int64Rep,
              Word8Rep, Word16Rep, Word32Rep, Word64Rep]
           = False
           | otherwise
           =  pprPanic "ByteCodeGen.schemeE" (ppr scrut_primrep)

        -- given an alt, return a discr and code for it.
        codeAlt alt@(discr, binds_f, rhs)
           | isAlgCase 
           = let (unpack_code, d_after_unpack, p_after_unpack)
                    = mkUnpackCode (filter (not.isTyVar) binds_f) d' p'
             in  schemeE d_after_unpack s p_after_unpack rhs
					`thenBc` \ rhs_code -> 
                 returnBc (my_discr alt, unpack_code `appOL` rhs_code)
           | otherwise 
           = ASSERT(null binds_f) 
             schemeE d' s p' rhs	`thenBc` \ rhs_code ->
             returnBc (my_discr alt, rhs_code)

        my_discr (DEFAULT, binds, rhs) = NoDiscr
        my_discr (DataAlt dc, binds, rhs) 
           | isUnboxedTupleCon dc
           = unboxedTupleException
           | otherwise
           = DiscrP (dataConTag dc - fIRST_TAG)
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


-- Compile code to do a tail call.  Specifically, push the fn,
-- slide the on-stack app back down to the sequel depth,
-- and enter.  Four cases:
--
-- 0.  (Nasty hack).
--     An application "PrelGHC.tagToEnum# <type> unboxed-int".
--     The int will be on the stack.  Generate a code sequence
--     to convert it to the relevant constructor, SLIDE and ENTER.
--
-- 1.  A nullary constructor.  Push its closure on the stack 
--     and SLIDE and RETURN.
--
-- 2.  (Another nasty hack).  Spot (# a::VoidRep, b #) and treat
--     it simply as  b  -- since the representations are identical
--     (the VoidRep takes up zero stack space).  Also, spot
--     (# b #) and treat it as  b.
--
-- 3.  The fn denotes a ccall.  Defer to generateCCall.
--
-- 4.  Application of a non-nullary constructor, by defn saturated.
--     Split the args into ptrs and non-ptrs, and push the nonptrs, 
--     then the ptrs, and then do PACK and RETURN.
--
-- 5.  Otherwise, it must be a function call.  Push the args
--     right to left, SLIDE and ENTER.

schemeT :: Int 		-- Stack depth
        -> Sequel 	-- Sequel depth
        -> BCEnv 	-- stack env
        -> AnnExpr Id VarSet 
        -> BcM BCInstrList

schemeT d s p app

--   | trace ("schemeT: env in = \n" ++ showSDocDebug (ppBCEnv p)) False
--   = panic "schemeT ?!?!"

--   | trace ("\nschemeT\n" ++ showSDoc (pprCoreExpr (deAnnotate app)) ++ "\n") False
--   = error "?!?!" 

   -- Case 0
   | Just (arg, constr_names) <- maybe_is_tagToEnum_call
   = pushAtom True d p arg 		`thenBc` \ (push, arg_words) ->
     implement_tagToId constr_names	`thenBc` \ tagToId_sequence ->
     returnBc (push `appOL`  tagToId_sequence            
                    `appOL`  mkSLIDE 1 (d+arg_words-s)
                    `snocOL` ENTER)

   -- Case 1
   | is_con_call && null args_r_to_l
   = returnBc (
        (PUSH_G (Left (getName con)) `consOL` mkSLIDE 1 (d-s))
        `snocOL` ENTER
     )

   -- Case 2
   | let isVoidRepAtom (_, AnnVar v)    = VoidRep == typePrimRep (idType v)
         isVoidRepAtom (_, AnnNote n e) = isVoidRepAtom e
     in  is_con_call && isUnboxedTupleCon con 
         && ( (length args_r_to_l == 2 && isVoidRepAtom (last (args_r_to_l)))
              || (length args_r_to_l == 1)
            )
   = --trace (if length args_r_to_l == 1
     --       then "schemeT: unboxed singleton"
     --       else "schemeT: unboxed pair with Void first component") (
     schemeT d s p (head args_r_to_l)
     --)

   -- Case 3
   | Just (CCall ccall_spec) <- isFCallId_maybe fn
   = generateCCall d s p ccall_spec fn args_r_to_l

   -- Cases 4 and 5
   | otherwise
   = if   is_con_call && isUnboxedTupleCon con
     then unboxedTupleException
     else do_pushery d (map snd args_final_r_to_l)

   where
      -- Detect and extract relevant info for the tagToEnum kludge.
      maybe_is_tagToEnum_call
         = let extract_constr_Names ty
                  = case splitTyConApp_maybe (repType ty) of
                       (Just (tyc, [])) |  isDataTyCon tyc
                                        -> map getName (tyConDataCons tyc)
                       other -> panic "maybe_is_tagToEnum_call.extract_constr_Ids"
           in 
           case app of
              (_, AnnApp (_, AnnApp (_, AnnVar v) (_, AnnType t)) arg)
                 -> case isPrimOpId_maybe v of
                       Just TagToEnumOp -> Just (snd arg, extract_constr_Names t)
		       other		-> Nothing
              other -> Nothing

      -- Extract the args (R->L) and fn
      (args_r_to_l_raw, fn) = chomp app
      chomp expr
         = case snd expr of
              AnnVar v    -> ([], v)
              AnnApp f a  -> case chomp f of (az, f) -> (a:az, f)
              AnnNote n e -> chomp e
              other       -> pprPanic "schemeT" 
                                (ppr (deAnnotate (panic "schemeT.chomp", other)))
         
      args_r_to_l = filter (not.isTypeAtom.snd) args_r_to_l_raw
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
         = filter (not.isPtr.snd) args_r_to_l ++ filter (isPtr.snd) args_r_to_l
           where isPtr = isFollowableRep . atomRep

      -- make code to push the args and then do the SLIDE-ENTER thing
      tag_when_push = not is_con_call
      narg_words    = sum (map (get_arg_szw . atomRep . snd) args_r_to_l)
      get_arg_szw   = if tag_when_push then taggedSizeW else untaggedSizeW

      do_pushery d (arg:args)
         = pushAtom tag_when_push d p arg	`thenBc` \ (push, arg_words) ->
           do_pushery (d+arg_words) args	`thenBc` \ more_push_code ->
           returnBc (push `appOL` more_push_code)
      do_pushery d []
         | Just (CCall ccall_spec) <- isFCallId_maybe fn
         = panic "schemeT.do_pushery: unexpected ccall"
         | otherwise
         = case maybe_dcon of
              Just con -> returnBc (
                             (PACK con narg_words `consOL`
                              mkSLIDE 1 (d - narg_words - s)) `snocOL`
                              ENTER
                          )
              Nothing
                 -> pushAtom True d p (AnnVar fn)	
						`thenBc` \ (push, arg_words) ->
                    returnBc (push `appOL` mkSLIDE (narg_words+arg_words) 
                                                   (d - s - narg_words)
                              `snocOL` ENTER)



{- Deal with a CCall.  Taggedly push the args onto the stack R->L,
   deferencing ForeignObj#s and (ToDo: adjusting addrs to point to
   payloads in Ptr/Byte arrays).  Then, generate the marshalling
   (machine) code for the ccall, and create bytecodes to call that and
   then return in the right way.  
-}
generateCCall :: Int -> Sequel 		-- stack and sequel depths
              -> BCEnv
              -> CCallSpec		-- where to call
              -> Id 			-- of target, for type info
              -> [AnnExpr Id VarSet]	-- args (atoms)
              -> BcM BCInstrList

generateCCall d0 s p ccall_spec@(CCallSpec target cconv safety) fn args_r_to_l
   = let 
         -- useful constants
         addr_usizeW = untaggedSizeW AddrRep
         addr_tsizeW = taggedSizeW AddrRep

         -- Get the args on the stack, with tags and suitably
         -- dereferenced for the CCall.  For each arg, return the
         -- depth to the first word of the bits for that arg, and the
         -- PrimRep of what was actually pushed.

         pargs d [] = returnBc []
         pargs d ((_,a):az) 
            = let rep_arg = atomRep a
              in case rep_arg of
                    -- Don't push the FO; instead push the Addr# it
                    -- contains.
                    ForeignObjRep
                       -> pushAtom False{-irrelevant-} d p a
							`thenBc` \ (push_fo, _) ->
                          let foro_szW = taggedSizeW ForeignObjRep
                              d_now    = d + addr_tsizeW
                              code     = push_fo `appOL` toOL [
                                            UPK_TAG addr_usizeW 0 0,
                                            SLIDE addr_tsizeW foro_szW
                                         ]
                          in  pargs d_now az 		`thenBc` \ rest ->
                              returnBc ((code, AddrRep) : rest)

                    ArrayRep
                       -> pargs (d + addr_tsizeW) az	`thenBc` \ rest ->
                          parg_ArrayishRep arrPtrsHdrSize d p a
							`thenBc` \ code ->
                          returnBc ((code,AddrRep):rest)

                    ByteArrayRep
                       -> pargs (d + addr_tsizeW) az	`thenBc` \ rest ->
                          parg_ArrayishRep arrWordsHdrSize d p a
							`thenBc` \ code ->
                          returnBc ((code,AddrRep):rest)

                    -- Default case: push taggedly, but otherwise intact.
                    other
                       -> pushAtom True d p a		`thenBc` \ (code_a, sz_a) ->
                          pargs (d+sz_a) az		`thenBc` \ rest ->
                          returnBc ((code_a, rep_arg) : rest)

         -- Do magic for Ptr/Byte arrays.  Push a ptr to the array on
         -- the stack but then advance it over the headers, so as to
         -- point to the payload.
         parg_ArrayishRep hdrSizeW d p a
            = pushAtom False{-irrel-} d p a `thenBc` \ (push_fo, _) ->
              -- The ptr points at the header.  Advance it over the
              -- header and then pretend this is an Addr# (push a tag).
              returnBc (push_fo `snocOL` 
                        SWIZZLE 0 (hdrSizeW * untaggedSizeW PtrRep
                                            * wORD_SIZE) 
                        `snocOL`
                        PUSH_TAG addr_usizeW)

     in
         pargs d0 args_r_to_l				`thenBc` \ code_n_reps ->
     let
         (pushs_arg, a_reps_pushed_r_to_l) = unzip code_n_reps

         push_args    = concatOL pushs_arg
         d_after_args = d0 + sum (map taggedSizeW a_reps_pushed_r_to_l)
         a_reps_pushed_RAW
            | null a_reps_pushed_r_to_l || head a_reps_pushed_r_to_l /= VoidRep
            = panic "ByteCodeGen.generateCCall: missing or invalid World token?"
            | otherwise
            = reverse (tail a_reps_pushed_r_to_l)

         -- Now: a_reps_pushed_RAW are the reps which are actually on the stack.
         -- push_args is the code to do that.
         -- d_after_args is the stack depth once the args are on.

         -- Get the result rep.
         (returns_void, r_rep)
            = case maybe_getCCallReturnRep (idType fn) of
                 Nothing -> (True,  VoidRep)
                 Just rr -> (False, rr) 
         {-
         Because the Haskell stack grows down, the a_reps refer to 
         lowest to highest addresses in that order.  The args for the call
         are on the stack.  Now push an unboxed, tagged Addr# indicating
         the C function to call.  Then push a dummy placeholder for the 
         result.  Finally, emit a CCALL insn with an offset pointing to the 
         Addr# just pushed, and a literal field holding the mallocville
         address of the piece of marshalling code we generate.
         So, just prior to the CCALL insn, the stack looks like this 
         (growing down, as usual):
                 
            <arg_n>
            ...
            <arg_1>
            Addr# address_of_C_fn
            <placeholder-for-result#> (must be an unboxed type)

         The interpreter then calls the marshall code mentioned
         in the CCALL insn, passing it (& <placeholder-for-result#>), 
         that is, the addr of the topmost word in the stack.
         When this returns, the placeholder will have been
         filled in.  The placeholder is slid down to the sequel
         depth, and we RETURN.

         This arrangement makes it simple to do f-i-dynamic since the Addr#
         value is the first arg anyway.  It also has the virtue that the
         stack is GC-understandable at all times.

         The marshalling code is generated specifically for this
         call site, and so knows exactly the (Haskell) stack
         offsets of the args, fn address and placeholder.  It
         copies the args to the C stack, calls the stacked addr,
         and parks the result back in the placeholder.  The interpreter
         calls it as a normal C call, assuming it has a signature
            void marshall_code ( StgWord* ptr_to_top_of_stack )
         -}
         -- resolve static address
         get_target_info
            = case target of
                 DynamicTarget
                    -> returnBc (False, panic "ByteCodeGen.generateCCall(dyn)")
                 StaticTarget target
                    -> let sym_to_find = _UNPK_ target in
                       ioToBc (lookupSymbol sym_to_find) `thenBc` \res ->
                       case res of
                           Just aa -> case aa of Ptr a# -> returnBc (True, A# a#)
                           Nothing -> ioToBc (linkFail "ByteCodeGen.generateCCall" 
                                                       sym_to_find)
                 CasmTarget _
                    -> pprPanic "ByteCodeGen.generateCCall: casm" (ppr ccall_spec)
     in
         get_target_info	`thenBc` \ (is_static, static_target_addr) ->
     let

         -- Get the arg reps, zapping the leading Addr# in the dynamic case
         a_reps -- | trace (showSDoc (ppr a_reps_pushed_RAW)) False = error "???"
                | is_static = a_reps_pushed_RAW
                | otherwise = if null a_reps_pushed_RAW 
                              then panic "ByteCodeGen.generateCCall: dyn with no args"
                              else tail a_reps_pushed_RAW

         -- push the Addr#
         (push_Addr, d_after_Addr)
            | is_static
            = (toOL [PUSH_UBX (Right static_target_addr) addr_usizeW,
                     PUSH_TAG addr_usizeW],
               d_after_args + addr_tsizeW)
            | otherwise	-- is already on the stack
            = (nilOL, d_after_args)

         -- Push the return placeholder.  For a call returning nothing,
         -- this is a VoidRep (tag).
         r_usizeW  = untaggedSizeW r_rep
         r_tsizeW  = taggedSizeW r_rep
         d_after_r = d_after_Addr + r_tsizeW
         r_lit     = mkDummyLiteral r_rep
         push_r    = (if   returns_void 
                      then nilOL 
                      else unitOL (PUSH_UBX (Left r_lit) r_usizeW))
                      `appOL` 
                      unitOL (PUSH_TAG r_usizeW)

         -- generate the marshalling code we're going to call
         r_offW       = 0 
         addr_offW    = r_tsizeW
         arg1_offW    = r_tsizeW + addr_tsizeW
         args_offW    = map (arg1_offW +) 
                            (init (scanl (+) 0 (map taggedSizeW a_reps)))
     in
         ioToBc (mkMarshalCode cconv
                    (r_offW, r_rep) addr_offW
                    (zip args_offW a_reps))	`thenBc` \ addr_of_marshaller ->
         recordMallocBc addr_of_marshaller	`thenBc_`
     let
         -- do the call
         do_call      = unitOL (CCALL addr_of_marshaller)
         -- slide and return
         wrapup       = mkSLIDE r_tsizeW (d_after_r - r_tsizeW - s)
                        `snocOL` RETURN r_rep
     in
         --trace (show (arg1_offW, args_offW  ,  (map taggedSizeW a_reps) )) (
         returnBc (
         push_args `appOL`
         push_Addr `appOL` push_r `appOL` do_call `appOL` wrapup
         )
         --)


-- Make a dummy literal, to be used as a placeholder for FFI return
-- values on the stack.
mkDummyLiteral :: PrimRep -> Literal
mkDummyLiteral pr
   = case pr of
        CharRep   -> MachChar 0
        IntRep    -> MachInt 0
        WordRep   -> MachWord 0
        DoubleRep -> MachDouble 0
        FloatRep  -> MachFloat 0
        AddrRep   | taggedSizeW AddrRep == taggedSizeW WordRep -> MachWord 0
        _         -> moan64 "mkDummyLiteral" (ppr pr)


-- Convert (eg) 
--     PrelGHC.Char# -> PrelGHC.State# PrelGHC.RealWorld
--                   -> (# PrelGHC.State# PrelGHC.RealWorld, PrelGHC.Int# #)
--
-- to  Just IntRep
-- and check that an unboxed pair is returned wherein the first arg is VoidRep'd.
--
-- Alternatively, for call-targets returning nothing, convert
--
--     PrelGHC.Char# -> PrelGHC.State# PrelGHC.RealWorld
--                   -> (# PrelGHC.State# PrelGHC.RealWorld #)
--
-- to  Nothing

maybe_getCCallReturnRep :: Type -> Maybe PrimRep
maybe_getCCallReturnRep fn_ty
   = let (a_tys, r_ty) = splitRepFunTys fn_ty
         maybe_r_rep_to_go  
            = if length r_reps == 1 then Nothing else Just (r_reps !! 1)
         (r_tycon, r_reps) 
            = case splitTyConApp_maybe (repType r_ty) of
                      (Just (tyc, tys)) -> (tyc, map typePrimRep tys)
                      Nothing -> blargh
         ok = ( (length r_reps == 2 && VoidRep == head r_reps)
                || r_reps == [VoidRep] )
              && isUnboxedTupleTyCon r_tycon
              && case maybe_r_rep_to_go of
                    Nothing    -> True
                    Just r_rep -> r_rep /= PtrRep
                                  -- if it was, it would be impossible 
                                  -- to create a valid return value 
                                  -- placeholder on the stack
         blargh = pprPanic "maybe_getCCallReturn: can't handle:" 
                           (pprType fn_ty)
     in 
     --trace (showSDoc (ppr (a_reps, r_reps))) (
     if ok then maybe_r_rep_to_go else blargh
     --)

atomRep (AnnVar v)    = typePrimRep (idType v)
atomRep (AnnLit l)    = literalPrimRep l
atomRep (AnnNote n b) = atomRep (snd b)
atomRep (AnnApp f (_, AnnType _)) = atomRep (snd f)
atomRep (AnnLam x e) | isTyVar x = atomRep (snd e)
atomRep other = pprPanic "atomRep" (ppr (deAnnotate (undefined,other)))


-- Compile code which expects an unboxed Int on the top of stack,
-- (call it i), and pushes the i'th closure in the supplied list 
-- as a consequence.
implement_tagToId :: [Name] -> BcM BCInstrList
implement_tagToId names
   = ASSERT(not (null names))
     getLabelsBc (length names)			`thenBc` \ labels ->
     getLabelBc					`thenBc` \ label_fail ->
     getLabelBc 				`thenBc` \ label_exit ->
     zip4 labels (tail labels ++ [label_fail])
                 [0 ..] names			`bind`   \ infos ->
     map (mkStep label_exit) infos		`bind`   \ steps ->
     returnBc (concatOL steps
               `appOL` 
               toOL [LABEL label_fail, CASEFAIL, LABEL label_exit])
     where
        mkStep l_exit (my_label, next_label, n, name_for_n)
           = toOL [LABEL my_label, 
                   TESTEQ_I n next_label, 
                   PUSH_G (Left name_for_n), 
                   JMP l_exit]


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
           | npr `elem` [IntRep, WordRep, FloatRep, DoubleRep, CharRep, AddrRep]
           = approved
           | otherwise
           = moan64 "ByteCodeGen.mkUnpackCode" (ppr npr)
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

pushAtom :: Bool -> Int -> BCEnv -> AnnExpr' Id VarSet -> BcM (BCInstrList, Int)
pushAtom tagged d p (AnnVar v)

   | idPrimRep v == VoidRep
   = if tagged then returnBc (unitOL (PUSH_TAG 0), 1) 
               else panic "ByteCodeGen.pushAtom(VoidRep,untaggedly)"

   | isFCallId v
   = pprPanic "pushAtom: shouldn't get an FCallId here" (ppr v)

   | Just primop <- isPrimOpId_maybe v
   = returnBc (unitOL (PUSH_G (Right primop)), 1)

   | otherwise
   = let  {-
	  str = "\npushAtom " ++ showSDocDebug (ppr v) 
               ++ " :: " ++ showSDocDebug (pprType (idType v))
               ++ ", depth = " ++ show d
               ++ ", tagged = " ++ show tagged ++ ", env =\n" ++ 
               showSDocDebug (ppBCEnv p)
               ++ " --> words: " ++ show (snd result) ++ "\n" ++
               showSDoc (nest 4 (vcat (map ppr (fromOL (fst result)))))
               ++ "\nendPushAtom " ++ showSDocDebug (ppr v)
	 -}

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
         returnBc result

pushAtom True d p (AnnLit lit)
   = pushAtom False d p (AnnLit lit)		`thenBc` \ (ubx_code, ubx_size) ->
     returnBc (ubx_code `snocOL` PUSH_TAG ubx_size, 1 + ubx_size)

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
             in  returnBc (unitOL (PUSH_UBX (Left lit) size_host_words), 
                           size_host_words)

        pushStr s 
           = let getMallocvilleAddr
                    = case s of
                         CharStr s i -> returnBc (A# s)

                         FastString _ l ba -> 
                            -- sigh, a string in the heap is no good to us.
                            -- We need a static C pointer, since the type of 
                            -- a string literal is Addr#.  So, copy the string 
                            -- into C land and introduce a memory leak 
                            -- at the same time.
                            let n = I# l
                            -- CAREFUL!  Chars are 32 bits in ghc 4.09+
                            in  ioToBc (mallocBytes (n+1)) `thenBc` \ (Ptr a#) ->
                                recordMallocBc (A# a#)     `thenBc_`
                                ioToBc (
                                   do strncpy (Ptr a#) ba (fromIntegral n)
                                      writeCharOffAddr (A# a#) n '\0'
                                      return (A# a#)
                                   )
                         other -> panic "ByteCodeGen.pushAtom.pushStr"
             in
                getMallocvilleAddr `thenBc` \ addr ->
                -- Get the addr on the stack, untaggedly
                   returnBc (unitOL (PUSH_UBX (Right addr) 1), 1)





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


taggedIdSizeW, untaggedIdSizeW :: Id -> Int
taggedIdSizeW   = taggedSizeW   . typePrimRep . idType
untaggedIdSizeW = untaggedSizeW . typePrimRep . idType

unboxedTupleException :: a
unboxedTupleException 
   = throwDyn 
        (Panic 
           ("Bytecode generator can't handle unboxed tuples.  Possibly due\n" ++
            "\tto foreign import/export decls in source.  Workaround:\n" ++
            "\tcompile this module to a .o file, then restart session."))


mkSLIDE n d = if d == 0 then nilOL else unitOL (SLIDE n d)
bind x f    = f x

\end{code}

%************************************************************************
%*									*
\subsection{The bytecode generator's monad}
%*									*
%************************************************************************

\begin{code}
data BcM_State 
   = BcM_State { bcos      :: [ProtoBCO Name],	-- accumulates completed BCOs
                 nextlabel :: Int,		-- for generating local labels
                 malloced  :: [Addr] }		-- ptrs malloced for current BCO
                                                -- Should be free()d when it is GCd
type BcM r = BcM_State -> IO (BcM_State, r)

ioToBc :: IO a -> BcM a
ioToBc io st = do x <- io 
                  return (st, x)

runBc :: BcM_State -> BcM r -> IO (BcM_State, r)
runBc st0 m = do (st1, res) <- m st0
                 return (st1, res)

thenBc :: BcM a -> (a -> BcM b) -> BcM b
thenBc expr cont st0
   = do (st1, q) <- expr st0
        (st2, r) <- cont q st1
        return (st2, r)

thenBc_ :: BcM a -> BcM b -> BcM b
thenBc_ expr cont st0
   = do (st1, q) <- expr st0
        (st2, r) <- cont st1
        return (st2, r)

returnBc :: a -> BcM a
returnBc result st = return (st, result)


mapBc :: (a -> BcM b) -> [a] -> BcM [b]
mapBc f []     = returnBc []
mapBc f (x:xs)
  = f x          `thenBc` \ r  ->
    mapBc f xs   `thenBc` \ rs ->
    returnBc (r:rs)

emitBc :: ([Addr] -> ProtoBCO Name) -> BcM ()
emitBc bco st
   = return (st{bcos = bco (malloced st) : bcos st, malloced=[]}, ())

newbcoBc :: BcM ()
newbcoBc st
   | not (null (malloced st)) 
   = panic "ByteCodeGen.newbcoBc: missed prior emitBc?"
   | otherwise
   = return (st, ())

recordMallocBc :: Addr -> BcM ()
recordMallocBc a st
   = return (st{malloced = a : malloced st}, ())

getLabelBc :: BcM Int
getLabelBc st
   = return (st{nextlabel = 1 + nextlabel st}, nextlabel st)

getLabelsBc :: Int -> BcM [Int]
getLabelsBc n st
   = let ctr = nextlabel st 
     in return (st{nextlabel = ctr+n}, [ctr .. ctr+n-1])

\end{code}
