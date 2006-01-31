%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcPat]{Typechecking patterns}

\begin{code}
module TcPat ( tcPat, tcPats, tcOverloadedLit,
	       PatCtxt(..), badFieldCon, polyPatSig ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcSyntaxOp )
import HsSyn		( Pat(..), LPat, HsConDetails(..), HsLit(..), HsOverLit(..), HsExpr(..),
			  LHsBinds, emptyLHsBinds, isEmptyLHsBinds, 
			  collectPatsBinders, nlHsLit )
import TcHsSyn		( TcId, hsLitType )
import TcRnMonad
import Inst		( InstOrigin(..), shortCutFracLit, shortCutIntLit, 
			  newDicts, instToId, tcInstStupidTheta, isHsVar
			)
import Id		( Id, idType, mkLocalId )
import CoreFVs		( idFreeTyVars )
import Name		( Name, mkSystemVarName )
import TcSimplify	( tcSimplifyCheck, bindInstsOfLocalFuns )
import TcEnv		( newLocalName, tcExtendIdEnv1, tcExtendTyVarEnv2,
			  tcLookupClass, tcLookupDataCon, tcLookupId, refineEnvironment,
			  tcMetaTy )
import TcMType 		( newFlexiTyVarTy, arityErr, tcInstSkolTyVars, newBoxyTyVar, zonkTcType )
import TcType		( TcType, TcTyVar, TcSigmaType, TcRhoType, 
			  SkolemInfo(PatSkol), 
			  BoxySigmaType, BoxyRhoType, 
			  pprSkolTvBinding, isRefineableTy, isRigidTy, tcTyVarsOfTypes, mkTyVarTy, lookupTyVar, 
			  emptyTvSubst, substTyVar, substTy, mkTopTvSubst, zipTopTvSubst, zipOpenTvSubst,
			  mkTyVarTys, mkClassPred, mkTyConApp, isOverloadedTy,
			  mkFunTy, mkFunTys, exactTyVarsOfTypes,
			  tidyOpenTypes )
import VarSet		( elemVarSet, mkVarSet )
import Kind		( liftedTypeKind, openTypeKind )
import TcUnify		( boxySplitTyConApp, boxySplitListTy, 
			  unBox, stripBoxyType, zapToMonotype,
		  	  boxyMatchTypes, boxyUnify, boxyUnifyList, checkSigTyVarsWrt )
import TcHsType		( UserTypeCtxt(..), tcPatSig )
import TysWiredIn	( boolTy, parrTyCon, tupleTyCon )
import Unify		( MaybeErr(..), gadtRefineTys )
import Type		( substTys, substTheta )
import StaticFlags	( opt_IrrefutableTuples )
import TyCon		( TyCon )
import DataCon		( DataCon, dataConTyCon, isVanillaDataCon, 
			  dataConFieldLabels, dataConSourceArity, dataConSig )
import PrelNames	( integralClassName, fromIntegerName, integerTyConName, 
			  fromRationalName, rationalTyConName )
import BasicTypes	( isBoxed )
import SrcLoc		( Located(..), SrcSpan, noLoc )
import ErrUtils		( Message )
import Util		( takeList, zipEqual )
import Outputable
import FastString
\end{code}


%************************************************************************
%*									*
		External interface
%*									*
%************************************************************************

\begin{code}
tcPats	:: PatCtxt
	-> [LPat Name]			-- Patterns,
	-> [BoxySigmaType]		--   and their types
	-> BoxyRhoType 			-- Result type,
	-> (BoxyRhoType -> TcM a)	--   and the checker for the body
	-> TcM ([LPat TcId], a)

-- This is the externally-callable wrapper function
-- Typecheck the patterns, extend the environment to bind the variables,
-- do the thing inside, use any existentially-bound dictionaries to 
-- discharge parts of the returning LIE, and deal with pattern type
-- signatures

--   1. Initialise the PatState
--   2. Check the patterns
--   3. Apply the refinement
--   4. Check the body
--   5. Check that no existentials escape

tcPats ctxt pats tys res_ty thing_inside
  =  do	{ let init_state = PS { pat_ctxt = ctxt, pat_reft = emptyTvSubst }

	; (pats', ex_tvs, res) <- tc_lpats init_state pats tys $ \ pstate' ->
				  thing_inside (refineType (pat_reft pstate') res_ty)

	; tcCheckExistentialPat ctxt pats' ex_tvs tys res_ty

	; returnM (pats', res) }


-----------------
tcPat :: PatCtxt 
      -> LPat Name -> TcType 
      -> BoxyRhoType		-- Result type
      -> (BoxyRhoType -> TcM a)	-- Checker for body, given its result type
      -> TcM (LPat TcId, a)
tcPat ctxt pat pat_ty res_ty thing_inside
  = do	{ ([pat'],thing) <- tcPats ctxt [pat] [pat_ty] res_ty thing_inside
	; return (pat', thing) }


-----------------
tcCheckExistentialPat :: PatCtxt
		      -> [LPat TcId]		-- Patterns (just for error message)
		      -> [TcTyVar]		-- Existentially quantified tyvars bound by pattern
		      -> [BoxySigmaType]	-- Types of the patterns
		      -> BoxyRhoType		-- Type of the body of the match
		      				-- Tyvars in either of these must not escape
		      -> TcM ()
-- NB: we *must* pass "pats_tys" not just "body_ty" to tcCheckExistentialPat
-- For example, we must reject this program:
--	data C = forall a. C (a -> Int) 
-- 	f (C g) x = g x
-- Here, result_ty will be simply Int, but expected_ty is (C -> a -> Int).

tcCheckExistentialPat ctxt pats [] pat_tys body_ty
  = return ()	-- Short cut for case when there are no existentials

tcCheckExistentialPat (LetPat _) pats ex_tvs pat_tys body_ty
	-- Don't know how to deal with pattern-bound existentials yet
  = failWithTc (existentialExplode pats)

tcCheckExistentialPat ctxt pats ex_tvs pat_tys body_ty
  = addErrCtxtM (sigPatCtxt (collectPatsBinders pats) ex_tvs pat_tys)	$
    checkSigTyVarsWrt (tcTyVarsOfTypes (body_ty:pat_tys)) ex_tvs

data PatState = PS {
	pat_ctxt :: PatCtxt,
	pat_reft :: GadtRefinement	-- Binds rigid TcTyVars to their refinements
  }

data PatCtxt 
  = LamPat 
  | LetPat (Name -> Maybe TcRhoType)	-- Used for let(rec) bindings

patSigCtxt :: PatState -> UserTypeCtxt
patSigCtxt (PS { pat_ctxt = LetPat _ }) = BindPatSigCtxt
patSigCtxt other			= LamPatSigCtxt
\end{code}



%************************************************************************
%*									*
		Binders
%*									*
%************************************************************************

\begin{code}
tcPatBndr :: PatState -> Name -> BoxySigmaType -> TcM TcId
tcPatBndr (PS { pat_ctxt = LamPat }) bndr_name pat_ty
  = do	{ pat_ty' <- unBox pat_ty
		-- We have an undecorated binder, so we do rule ABS1,
		-- by unboxing the boxy type, forcing any un-filled-in
		-- boxes to become monotypes
		-- NB that pat_ty' can still be a polytype:
		-- 	data T = MkT (forall a. a->a)
		-- 	f t = case t of { MkT g -> ... }
		-- Here, the 'g' must get type (forall a. a->a) from the
		-- MkT context
	; return (mkLocalId bndr_name pat_ty') }

tcPatBndr (PS { pat_ctxt = LetPat lookup_sig }) bndr_name pat_ty
  | Just mono_ty <- lookup_sig bndr_name
  = do	{ mono_name <- newLocalName bndr_name
	; boxyUnify mono_ty pat_ty
	; return (mkLocalId mono_name mono_ty) }

  | otherwise
  = do	{ pat_ty' <- unBox pat_ty
	; mono_name <- newLocalName bndr_name
	; return (mkLocalId mono_name pat_ty') }


-------------------
bindInstsOfPatId :: TcId -> TcM a -> TcM (a, LHsBinds TcId)
bindInstsOfPatId id thing_inside
  | not (isOverloadedTy (idType id))
  = do { res <- thing_inside; return (res, emptyLHsBinds) }
  | otherwise
  = do	{ (res, lie) <- getLIE thing_inside
	; binds <- bindInstsOfLocalFuns lie [id]
	; return (res, binds) }
\end{code}


%************************************************************************
%*									*
		The main worker functions
%*									*
%************************************************************************

Note [Nesting]
~~~~~~~~~~~~~~
tcPat takes a "thing inside" over which the patter scopes.  This is partly
so that tcPat can extend the environment for the thing_inside, but also 
so that constraints arising in the thing_inside can be discharged by the
pattern.

This does not work so well for the ErrCtxt carried by the monad: we don't
want the error-context for the pattern to scope over the RHS. 
Hence the getErrCtxt/setErrCtxt stuff in tc_lpats.

\begin{code}
--------------------
tc_lpats :: PatState
	 -> [LPat Name] 
	 -> [BoxySigmaType]	
	 -> (PatState -> TcM a)
	 -> TcM ([LPat TcId], [TcTyVar], a)

tc_lpats pstate pats pat_tys thing_inside
  = do	{ err_ctxt <- getErrCtxt
	; let loop pstate [] [] 
		= do { res <- thing_inside pstate
		     ; return ([], [], res) }

	      loop pstate (p:ps) (ty:tys)
		= do { (p', p_tvs, (ps', ps_tvs, res)) 
				<- tc_lpat pstate p ty $ \ pstate' ->
				   setErrCtxt err_ctxt $
				   loop pstate' ps tys
		-- setErrCtxt: restore context before doing the next pattern
		-- See note [Nesting] above
				
		     ; return (p':ps', p_tvs ++ ps_tvs, res) }

	      loop _ _ _ = pprPanic "tc_lpats" (ppr pats $$ ppr pat_tys)

	; loop pstate pats pat_tys }

--------------------
tc_lpat :: PatState
	 -> LPat Name 
	 -> BoxySigmaType
	 -> (PatState -> TcM a)
	 -> TcM (LPat TcId, [TcTyVar], a)
tc_lpat pstate (L span pat) pat_ty thing_inside
  = setSrcSpan span		  $
    maybeAddErrCtxt (patCtxt pat) $
    do	{ let pat_ty' = refineType (pat_reft pstate) pat_ty
		-- Make sure the result type reflects the current refinement
	; (pat', tvs, res) <- tc_pat pstate pat pat_ty' thing_inside
	; return (L span pat', tvs, res) }


--------------------
tc_pat	:: PatState
	-> Pat Name -> BoxySigmaType	-- Fully refined result type
	-> (PatState -> TcM a)	-- Thing inside
	-> TcM (Pat TcId, 	-- Translated pattern
		[TcTyVar], 	-- Existential binders
		a)		-- Result of thing inside

tc_pat pstate (VarPat name) pat_ty thing_inside
  = do	{ id <- tcPatBndr pstate name pat_ty
	; (res, binds) <- bindInstsOfPatId id $
			  tcExtendIdEnv1 name id $
			  (traceTc (text "binding" <+> ppr name <+> ppr (idType id))
			   >> thing_inside pstate)
	; let pat' | isEmptyLHsBinds binds = VarPat id
		   | otherwise		   = VarPatOut id binds
	; return (pat', [], res) }

tc_pat pstate (ParPat pat) pat_ty thing_inside
  = do	{ (pat', tvs, res) <- tc_lpat pstate pat pat_ty thing_inside
	; return (ParPat pat', tvs, res) }

-- There's a wrinkle with irrefuatable patterns, namely that we
-- must not propagate type refinement from them.  For example
--	data T a where { T1 :: Int -> T Int; ... }
--	f :: T a -> Int -> a
--	f ~(T1 i) y = y
-- It's obviously not sound to refine a to Int in the right
-- hand side, because the arugment might not match T1 at all!
--
-- Nor should a lazy pattern bind any existential type variables
-- because they won't be in scope when we do the desugaring
tc_pat pstate lpat@(LazyPat pat) pat_ty thing_inside
  = do	{ (pat', pat_tvs, res) <- tc_lpat pstate pat pat_ty $ \ _ ->
				  thing_inside pstate
					-- Ignore refined pstate',
					-- revert to pstate
	; if (null pat_tvs) then return ()
	  else lazyPatErr lpat pat_tvs
	; return (LazyPat pat', [], res) }

tc_pat pstate (WildPat _) pat_ty thing_inside
  = do	{ pat_ty' <- unBox pat_ty	-- Make sure it's filled in with monotypes
	; res <- thing_inside pstate
	; return (WildPat pat_ty', [], res) }

tc_pat pstate (AsPat (L nm_loc name) pat) pat_ty thing_inside
  = do	{ bndr_id <- setSrcSpan nm_loc (tcPatBndr pstate name pat_ty)
	; (pat', tvs, res) <- tcExtendIdEnv1 name bndr_id $
			      tc_lpat pstate pat (idType bndr_id) thing_inside
	    -- NB: if we do inference on:
	    --		\ (y@(x::forall a. a->a)) = e
	    -- we'll fail.  The as-pattern infers a monotype for 'y', which then
	    -- fails to unify with the polymorphic type for 'x'.  This could 
	    -- perhaps be fixed, but only with a bit more work.
	    --
	    -- If you fix it, don't forget the bindInstsOfPatIds!
	; return (AsPat (L nm_loc bndr_id) pat', tvs, res) }

-- Type signatures in patterns
-- See Note [Pattern coercions] below
tc_pat pstate (SigPatIn pat sig_ty) pat_ty thing_inside
  = do	{ (inner_ty, tv_binds) <- tcPatSig (patSigCtxt pstate) sig_ty pat_ty
	; (pat', tvs, res) <- tcExtendTyVarEnv2 tv_binds $
			      tc_lpat pstate pat inner_ty thing_inside
	; return (SigPatOut pat' inner_ty, tvs, res) }

tc_pat pstate pat@(TypePat ty) pat_ty thing_inside
  = failWithTc (badTypePat pat)

------------------------
-- Lists, tuples, arrays
tc_pat pstate (ListPat pats _) pat_ty thing_inside
  = do	{ elt_ty <- boxySplitListTy pat_ty
	; let elt_tys = takeList pats (repeat elt_ty) 
	; (pats', pats_tvs, res) <- tc_lpats pstate pats elt_tys thing_inside
 	; return (ListPat pats' elt_ty, pats_tvs, res) }

tc_pat pstate (PArrPat pats _) pat_ty thing_inside
  = do	{ [elt_ty] <- boxySplitTyConApp parrTyCon pat_ty
 	; let elt_tys = takeList pats (repeat elt_ty) 
	; (pats', pats_tvs, res) <- tc_lpats pstate pats elt_tys thing_inside 
	; ifM (null pats) (zapToMonotype pat_ty)	-- c.f. ExplicitPArr in TcExpr
	; return (PArrPat pats' elt_ty, pats_tvs, res) }

tc_pat pstate (TuplePat pats boxity) pat_ty thing_inside
  = do	{ arg_tys <- boxySplitTyConApp (tupleTyCon boxity (length pats)) pat_ty
	; (pats', pats_tvs, res) <- tc_lpats pstate pats arg_tys thing_inside

	-- Under flag control turn a pattern (x,y,z) into ~(x,y,z)
	-- so that we can experiment with lazy tuple-matching.
	-- This is a pretty odd place to make the switch, but
	-- it was easy to do.
	; let unmangled_result = TuplePat pats' boxity
	      possibly_mangled_result
	        | opt_IrrefutableTuples && isBoxed boxity = LazyPat (noLoc unmangled_result)
	        | otherwise			          = unmangled_result

 	; ASSERT( length arg_tys == length pats )	-- Syntactically enforced
	  return (possibly_mangled_result, pats_tvs, res) }

------------------------
-- Data constructors
tc_pat pstate pat_in@(ConPatIn (L con_span con_name) arg_pats) pat_ty thing_inside
  = do	{ data_con <- tcLookupDataCon con_name
	; let tycon = dataConTyCon data_con
	; tcConPat pstate con_span data_con tycon pat_ty arg_pats thing_inside }

------------------------
-- Literal patterns
tc_pat pstate (LitPat simple_lit) pat_ty thing_inside
  = do	{ boxyUnify (hsLitType simple_lit) pat_ty
	; res <- thing_inside pstate
	; returnM (LitPat simple_lit, [], res) }

------------------------
-- Overloaded patterns: n, and n+k
tc_pat pstate pat@(NPat over_lit mb_neg eq _) pat_ty thing_inside
  = do	{ let orig = LiteralOrigin over_lit
	; lit'    <- tcOverloadedLit orig over_lit pat_ty
	; eq'     <- tcSyntaxOp orig eq (mkFunTys [pat_ty, pat_ty] boolTy)
	; mb_neg' <- case mb_neg of
			Nothing  -> return Nothing	-- Positive literal
			Just neg -> 	-- Negative literal
					-- The 'negate' is re-mappable syntax
 			    do { neg' <- tcSyntaxOp orig neg (mkFunTy pat_ty pat_ty)
			       ; return (Just neg') }
	; res <- thing_inside pstate
	; returnM (NPat lit' mb_neg' eq' pat_ty, [], res) }

tc_pat pstate pat@(NPlusKPat (L nm_loc name) lit ge minus) pat_ty thing_inside
  = do	{ bndr_id <- setSrcSpan nm_loc (tcPatBndr pstate name pat_ty)
 	; let pat_ty' = idType bndr_id
	      orig    = LiteralOrigin lit
	; lit' <- tcOverloadedLit orig lit pat_ty'

	-- The '>=' and '-' parts are re-mappable syntax
	; ge'    <- tcSyntaxOp orig ge    (mkFunTys [pat_ty', pat_ty'] boolTy)
	; minus' <- tcSyntaxOp orig minus (mkFunTys [pat_ty', pat_ty'] pat_ty')

	-- The Report says that n+k patterns must be in Integral
	-- We may not want this when using re-mappable syntax, though (ToDo?)
	; icls <- tcLookupClass integralClassName
	; dicts <- newDicts orig [mkClassPred icls [pat_ty']]	
	; extendLIEs dicts
    
	; res <- tcExtendIdEnv1 name bndr_id (thing_inside pstate)
	; returnM (NPlusKPat (L nm_loc bndr_id) lit' ge' minus', [], res) }
\end{code}


%************************************************************************
%*									*
	Most of the work for constructors is here
	(the rest is in the ConPatIn case of tc_pat)
%*									*
%************************************************************************

\begin{code}
tcConPat :: PatState -> SrcSpan -> DataCon -> TyCon 
	 -> BoxySigmaType	-- Type of the pattern
	 -> HsConDetails Name (LPat Name) -> (PatState -> TcM a)
	 -> TcM (Pat TcId, [TcTyVar], a)
tcConPat pstate con_span data_con tycon pat_ty arg_pats thing_inside
  | isVanillaDataCon data_con
  = do	{ ty_args <- boxySplitTyConApp tycon pat_ty
	; let (tvs, _, arg_tys, _, _) = dataConSig data_con
	      arg_tvs  = exactTyVarsOfTypes arg_tys
		-- See Note [Silly type synonyms in smart-app] in TcExpr
		-- for why we must use exactTyVarsOfTypes
	      inst_prs = zipEqual "tcConPat" tvs ty_args
	      subst    = mkTopTvSubst inst_prs
	      arg_tys' = substTys subst arg_tys
	      unconstrained_ty_args = [ty_arg | (tv,ty_arg) <- inst_prs,
						not (tv `elemVarSet` arg_tvs)]
	; mapM unBox unconstrained_ty_args	-- Zap these to monotypes
	; tcInstStupidTheta data_con ty_args
	; traceTc (text "tcConPat" <+> vcat [ppr data_con, ppr ty_args, ppr arg_tys'])
	; (arg_pats', tvs, res) <- tcConArgs pstate data_con arg_pats arg_tys' thing_inside
	; return (ConPatOut (L con_span data_con) [] [] emptyLHsBinds 
			    arg_pats' (mkTyConApp tycon ty_args),
		  tvs, res) }

  | otherwise	-- GADT case
  = do	{ ty_args <- boxySplitTyConApp tycon pat_ty
	; span <- getSrcSpanM	-- The whole pattern

	-- Instantiate the constructor type variables and result type
	; let (tvs, theta, arg_tys, _, res_tys) = dataConSig data_con
	      arg_tvs = exactTyVarsOfTypes arg_tys
		-- See Note [Silly type synonyms in smart-app] in TcExpr
		-- for why we must use exactTyVarsOfTypes
	      skol_info = PatSkol data_con span
	      arg_flags = [ tv `elemVarSet` arg_tvs | tv <- tvs ]
	; tvs' <- tcInstSkolTyVars skol_info tvs
	; let res_tys' = substTys (zipTopTvSubst tvs (mkTyVarTys tvs')) res_tys

	-- Do type refinement!
	; traceTc (text "tcGadtPat" <+> vcat [ppr data_con, ppr tvs', ppr res_tys', 
					      text "ty-args:" <+> ppr ty_args ])
	; refineAlt pstate data_con tvs' arg_flags res_tys' ty_args 
			$ \ pstate' tv_tys' -> do

	-- ToDo: arg_tys should be boxy, but I don't think theta' should be,
	--	 or the tv_tys' in the call to tcInstStupidTheta
	{ let tenv'    = zipTopTvSubst tvs tv_tys'
	      theta'   = substTheta tenv' theta
	      arg_tys' = substTys   tenv' arg_tys	-- Boxy types

	; ((arg_pats', inner_tvs, res), lie_req) <- getLIE $
		do { tcInstStupidTheta data_con tv_tys'
			-- The stupid-theta mentions the newly-bound tyvars, so
			-- it must live inside the getLIE, so that the
			-- tcSimplifyCheck will apply the type refinement to it
		   ; tcConArgs pstate' data_con arg_pats arg_tys' thing_inside }

	; dicts <- newDicts (SigOrigin skol_info) theta'
	; dict_binds <- tcSimplifyCheck doc tvs' dicts lie_req

	; return (ConPatOut (L con_span data_con)
			    tvs' (map instToId dicts) dict_binds
			    arg_pats' (mkTyConApp tycon ty_args),
		  tvs' ++ inner_tvs, res) 
	} }
  where
    doc = ptext SLIT("existential context for") <+> quotes (ppr data_con)

tcConArgs :: PatState -> DataCon 
	   -> HsConDetails Name (LPat Name) -> [TcSigmaType]
	   -> (PatState -> TcM a)
	   -> TcM (HsConDetails TcId (LPat Id), [TcTyVar], a)

tcConArgs pstate data_con (PrefixCon arg_pats) arg_tys thing_inside
  = do	{ checkTc (con_arity == no_of_args)	-- Check correct arity
		  (arityErr "Constructor" data_con con_arity no_of_args)
	; (arg_pats', tvs, res) <- tc_lpats pstate arg_pats arg_tys thing_inside
	; return (PrefixCon arg_pats', tvs, res) }
  where
    con_arity  = dataConSourceArity data_con
    no_of_args = length arg_pats

tcConArgs pstate data_con (InfixCon p1 p2) arg_tys thing_inside
  = do	{ checkTc (con_arity == 2)	-- Check correct arity
	 	  (arityErr "Constructor" data_con con_arity 2)
	; ([p1',p2'], tvs, res) <- tc_lpats pstate [p1,p2] arg_tys thing_inside
	; return (InfixCon p1' p2', tvs, res) }
  where
    con_arity  = dataConSourceArity data_con

tcConArgs pstate data_con (RecCon rpats) arg_tys thing_inside
  = do	{ (rpats', tvs, res) <- tc_fields pstate rpats thing_inside
	; return (RecCon rpats', tvs, res) }
  where
    tc_fields :: PatState -> [(Located Name, LPat Name)]
	      -> (PatState -> TcM a)
	      -> TcM ([(Located TcId, LPat TcId)], [TcTyVar], a)
    tc_fields pstate [] thing_inside
      = do { res <- thing_inside pstate
	   ; return ([], [], res) }

    tc_fields pstate (rpat : rpats) thing_inside
      =	do { (rpat', tvs1, (rpats', tvs2, res)) 
		<- tc_field pstate rpat  $ \ pstate' ->
		   tc_fields pstate' rpats thing_inside
	   ; return (rpat':rpats', tvs1 ++ tvs2, res) }

    tc_field pstate (field_lbl, pat) thing_inside
      = do { (sel_id, pat_ty) <- wrapLocFstM find_field_ty field_lbl
	   ; (pat', tvs, res) <- tc_lpat pstate pat pat_ty thing_inside
	   ; return ((sel_id, pat'), tvs, res) }

    find_field_ty field_lbl
	= case [ty | (f,ty) <- field_tys, f == field_lbl] of

		-- No matching field; chances are this field label comes from some
		-- other record type (or maybe none).  As well as reporting an
		-- error we still want to typecheck the pattern, principally to
		-- make sure that all the variables it binds are put into the
		-- environment, else the type checker crashes later:
		--	f (R { foo = (a,b) }) = a+b
		-- If foo isn't one of R's fields, we don't want to crash when
		-- typechecking the "a+b".
	   [] -> do { addErrTc (badFieldCon data_con field_lbl)
		    ; bogus_ty <- newFlexiTyVarTy liftedTypeKind
		    ; return (error "Bogus selector Id", bogus_ty) }

		-- The normal case, when the field comes from the right constructor
	   (pat_ty : extras) -> 
		ASSERT( null extras )
		do { sel_id <- tcLookupId field_lbl
		   ; return (sel_id, pat_ty) }

    field_tys = zip (dataConFieldLabels data_con) arg_tys
	-- Don't use zipEqual! If the constructor isn't really a record, then
	-- dataConFieldLabels will be empty (and each field in the pattern
	-- will generate an error below).
\end{code}


%************************************************************************
%*									*
		Type refinement
%*									*
%************************************************************************

\begin{code}
refineAlt :: PatState 
	  -> DataCon		-- For tracing only
	  -> [TcTyVar]		-- Type variables from pattern
	  -> [Bool]		-- Flags indicating which type variables occur
				--	in the type of at least one argument
	  -> [TcType]		-- Result types from the pattern
	  -> [BoxySigmaType] 	-- Result types from the scrutinee (context)
	  -> (PatState -> [BoxySigmaType] -> TcM a)
			-- Possibly-refined existentials
	  -> TcM a
refineAlt pstate con pat_tvs arg_flags pat_res_tys ctxt_res_tys thing_inside
  | not (all isRigidTy ctxt_res_tys)
	-- The context is not a rigid type, so we do no type refinement here.  
  = do	{ let arg_tvs = mkVarSet [ tv | (tv, True) <- pat_tvs `zip` arg_flags]
	      subst = boxyMatchTypes arg_tvs pat_res_tys ctxt_res_tys
	      
	      res_tvs = tcTyVarsOfTypes pat_res_tys
		-- The tvs are (already) all fresh skolems. We need a 
		-- fresh skolem for each type variable (to bind in the pattern)
		-- even if it's refined away by the type refinement
	      find_inst tv 
		| not (tv `elemVarSet` res_tvs)        = return (mkTyVarTy tv)
		| Just boxy_ty <- lookupTyVar subst tv = return boxy_ty
		| otherwise			       = do { tv <- newBoxyTyVar openTypeKind
							    ; return (mkTyVarTy tv) }
	; pat_tys' <- mapM find_inst pat_tvs

	-- Do the thing inside
	; res <- thing_inside pstate pat_tys'

	-- Unbox the types that have been filled in by the thing_inside
	-- I.e. the ones whose type variables are mentioned in at least one arg
	; let strip ty in_arg_tv | in_arg_tv = stripBoxyType ty
				 | otherwise = return ty
	; pat_tys'' <- zipWithM strip pat_tys' arg_flags
	; let subst = zipOpenTvSubst pat_tvs pat_tys''
	; boxyUnifyList (substTys subst pat_res_tys) ctxt_res_tys

	; return res }		-- All boxes now filled

  | otherwise	-- The context is rigid, so we can do type refinement
  = case gadtRefineTys (pat_reft pstate) con pat_tvs pat_res_tys ctxt_res_tys of
	Failed msg -> failWithTc (inaccessibleAlt msg)
	Succeeded (new_subst, all_bound_here) 
	  | all_bound_here 	-- All the new bindings are for pat_tvs, so no need
				-- to refine the environment or pstate
	  -> do  { traceTc trace_msg
		 ; thing_inside pstate pat_tvs' }
	  | otherwise 		-- New bindings affect the context, so refine
				-- the environment and pstate
	  -> refineEnvironment (pat_reft pstate') $
	     do { traceTc trace_msg
		; thing_inside pstate' pat_tvs' }
	  where
  	     pat_tvs' = map (substTyVar new_subst) pat_tvs
	     pstate'  = pstate { pat_reft = new_subst }
	     trace_msg = text "refineTypes:match" <+> ppr con <+> ppr new_subst

refineType :: GadtRefinement -> BoxyRhoType -> BoxyRhoType
-- Refine the type if it is rigid
refineType reft ty
  | isRefineableTy ty = substTy reft ty
  | otherwise	      = ty
\end{code}


%************************************************************************
%*									*
		Overloaded literals
%*									*
%************************************************************************

In tcOverloadedLit we convert directly to an Int or Integer if we
know that's what we want.  This may save some time, by not
temporarily generating overloaded literals, but it won't catch all
cases (the rest are caught in lookupInst).

\begin{code}
tcOverloadedLit :: InstOrigin
		 -> HsOverLit Name
		 -> BoxyRhoType
		 -> TcM (HsOverLit TcId)
tcOverloadedLit orig lit@(HsIntegral i fi) res_ty
  | not (fi `isHsVar` fromIntegerName)	-- Do not generate a LitInst for rebindable syntax.  
	-- Reason: If we do, tcSimplify will call lookupInst, which
	--	   will call tcSyntaxName, which does unification, 
	--	   which tcSimplify doesn't like
	-- ToDo: noLoc sadness
  = do	{ integer_ty <- tcMetaTy integerTyConName
	; fi' <- tcSyntaxOp orig fi (mkFunTy integer_ty res_ty)
	; return (HsIntegral i (HsApp (noLoc fi') (nlHsLit (HsInteger i integer_ty)))) }

  | Just expr <- shortCutIntLit i res_ty 
  = return (HsIntegral i expr)

  | otherwise
  = do 	{ expr <- newLitInst orig lit res_ty
	; return (HsIntegral i expr) }

tcOverloadedLit orig lit@(HsFractional r fr) res_ty
  | not (fr `isHsVar` fromRationalName)	-- c.f. HsIntegral case
  = do	{ rat_ty <- tcMetaTy rationalTyConName
	; fr' <- tcSyntaxOp orig fr (mkFunTy rat_ty res_ty)
	 	-- Overloaded literals must have liftedTypeKind, because
	 	-- we're instantiating an overloaded function here,
	 	-- whereas res_ty might be openTypeKind. This was a bug in 6.2.2
		-- However this'll be picked up by tcSyntaxOp if necessary
	; return (HsFractional r (HsApp (noLoc fr') (nlHsLit (HsRat r rat_ty)))) }

  | Just expr <- shortCutFracLit r res_ty 
  = return (HsFractional r expr)

  | otherwise
  = do 	{ expr <- newLitInst orig lit res_ty
	; return (HsFractional r expr) }

newLitInst :: InstOrigin -> HsOverLit Name -> BoxyRhoType -> TcM (HsExpr TcId)
newLitInst orig lit res_ty	-- Make a LitInst
  = do 	{ loc <- getInstLoc orig
	; res_tau <- zapToMonotype res_ty
	; new_uniq <- newUnique
	; let
 		lit_nm   = mkSystemVarName new_uniq FSLIT("lit")
		lit_inst = LitInst lit_nm lit res_tau loc
	; extendLIE lit_inst
	; return (HsVar (instToId lit_inst)) }
\end{code}


%************************************************************************
%*									*
		Note [Pattern coercions]
%*									*
%************************************************************************

In principle, these program would be reasonable:
	
	f :: (forall a. a->a) -> Int
	f (x :: Int->Int) = x 3

	g :: (forall a. [a]) -> Bool
	g [] = True

In both cases, the function type signature restricts what arguments can be passed
in a call (to polymorphic ones).  The pattern type signature then instantiates this
type.  For example, in the first case,  (forall a. a->a) <= Int -> Int, and we
generate the translated term
	f = \x' :: (forall a. a->a).  let x = x' Int in x 3

From a type-system point of view, this is perfectly fine, but it's *very* seldom useful.
And it requires a significant amount of code to implement, becuase we need to decorate
the translated pattern with coercion functions (generated from the subsumption check 
by tcSub).  

So for now I'm just insisting on type *equality* in patterns.  No subsumption. 

Old notes about desugaring, at a time when pattern coercions were handled:

A SigPat is a type coercion and must be handled one at at time.  We can't
combine them unless the type of the pattern inside is identical, and we don't
bother to check for that.  For example:

	data T = T1 Int | T2 Bool
	f :: (forall a. a -> a) -> T -> t
	f (g::Int->Int)   (T1 i) = T1 (g i)
	f (g::Bool->Bool) (T2 b) = T2 (g b)

We desugar this as follows:

	f = \ g::(forall a. a->a) t::T ->
	    let gi = g Int
	    in case t of { T1 i -> T1 (gi i)
			   other ->
	    let	gb = g Bool
	    in case t of { T2 b -> T2 (gb b)
			   other -> fail }}

Note that we do not treat the first column of patterns as a
column of variables, because the coerced variables (gi, gb)
would be of different types.  So we get rather grotty code.
But I don't think this is a common case, and if it was we could
doubtless improve it.

Meanwhile, the strategy is:
	* treat each SigPat coercion (always non-identity coercions)
		as a separate block
	* deal with the stuff inside, and then wrap a binding round
		the result to bind the new variable (gi, gb, etc)


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
patCtxt :: Pat Name -> Maybe Message	-- Not all patterns are worth pushing a context
patCtxt (VarPat _)  = Nothing
patCtxt (ParPat _)  = Nothing
patCtxt (AsPat _ _) = Nothing
patCtxt pat 	    = Just (hang (ptext SLIT("In the pattern:")) 
			       4 (ppr pat))

-----------------------------------------------

existentialExplode pats
  = hang (vcat [text "My brain just exploded.",
	        text "I can't handle pattern bindings for existentially-quantified constructors.",
		text "In the binding group for"])
	4 (vcat (map ppr pats))

sigPatCtxt bound_ids bound_tvs tys tidy_env 
  = 	-- tys is (body_ty : pat_tys)  
    mapM zonkTcType tys		`thenM` \ tys' ->
    let
	(env1,  tidy_tys) = tidyOpenTypes tidy_env (map idType show_ids)
	(_env2, tidy_body_ty : tidy_pat_tys) = tidyOpenTypes env1 tys'
    in
    returnM (env1,
		 sep [ptext SLIT("When checking an existential match that binds"),
		      nest 4 (vcat (zipWith ppr_id show_ids tidy_tys)),
		      ptext SLIT("The pattern(s) have type(s):") <+> vcat (map ppr tidy_pat_tys),
		      ptext SLIT("The body has type:") <+> ppr tidy_body_ty
		])
  where
    show_ids = filter is_interesting bound_ids
    is_interesting id = any (`elemVarSet` idFreeTyVars id) bound_tvs

    ppr_id id ty = ppr id <+> dcolon <+> ppr ty
	-- Don't zonk the types so we get the separate, un-unified versions

badFieldCon :: DataCon -> Name -> SDoc
badFieldCon con field
  = hsep [ptext SLIT("Constructor") <+> quotes (ppr con),
	  ptext SLIT("does not have field"), quotes (ppr field)]

polyPatSig :: TcType -> SDoc
polyPatSig sig_ty
  = hang (ptext SLIT("Illegal polymorphic type signature in pattern:"))
	 4 (ppr sig_ty)

badTypePat pat = ptext SLIT("Illegal type pattern") <+> ppr pat

lazyPatErr pat tvs
  = failWithTc $
    hang (ptext SLIT("A lazy (~) pattern connot bind existential type variables"))
       2 (vcat (map pprSkolTvBinding tvs))

inaccessibleAlt msg
  = hang (ptext SLIT("Inaccessible case alternative:")) 2 msg
\end{code}
