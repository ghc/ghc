%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcPat]{Typechecking patterns}

\begin{code}
module TcPat ( tcPat, tcPats, PatCtxt(..), badFieldCon, polyPatSig, refineTyVars ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcSyntaxOp )
import HsSyn		( Pat(..), LPat, HsConDetails(..), 
			  LHsBinds, emptyLHsBinds, isEmptyLHsBinds )
import TcHsSyn		( TcId, hsLitType )
import TcRnMonad
import Inst		( InstOrigin(..), tcOverloadedLit, 
			  newDicts, instToId, tcInstStupidTheta
			)
import Id		( Id, idType, mkLocalId )
import Var		( tyVarName )
import Name		( Name )
import TcSimplify	( tcSimplifyCheck, bindInstsOfLocalFuns )
import TcEnv		( newLocalName, tcExtendIdEnv1, tcExtendTyVarEnv2,
			  tcLookupClass, tcLookupDataCon, tcLookupId )
import TcMType 		( newTyFlexiVarTy, arityErr, tcSkolTyVars, readMetaTyVar )
import TcType		( TcType, TcTyVar, TcSigmaType, TcTauType, zipTopTvSubst,
			  SkolemInfo(PatSkol), isMetaTyVar, pprTcTyVar, 
			  TvSubst, mkOpenTvSubst, substTyVar, substTy, MetaDetails(..),
			  mkTyVarTys, mkClassPred, mkTyConApp, isOverloadedTy,
			  mkFunTy, mkFunTys )
import VarEnv		( mkVarEnv )	-- ugly
import Kind		( argTypeKind, liftedTypeKind )
import TcUnify		( tcSubPat, Expected(..), zapExpectedType, 
			  zapExpectedTo, zapToListTy, zapToTyConApp )  
import TcHsType		( UserTypeCtxt(..), TcSigInfo( sig_tau ), TcSigFun, tcHsPatSigType )
import TysWiredIn	( boolTy, parrTyCon, tupleTyCon )
import Unify		( MaybeErr(..), gadtRefineTys, BindFlag(..) )
import Type		( substTys, substTheta )
import StaticFlags	( opt_IrrefutableTuples )
import TyCon		( TyCon )
import DataCon		( DataCon, dataConTyCon, isVanillaDataCon, dataConInstOrigArgTys,
			  dataConFieldLabels, dataConSourceArity, dataConSig )
import PrelNames	( integralClassName )
import BasicTypes	( isBoxed )
import SrcLoc		( Located(..), SrcSpan, noLoc, unLoc )
import Maybes		( catMaybes )
import ErrUtils		( Message )
import Outputable
import FastString
\end{code}


%************************************************************************
%*									*
		External interface
%*									*
%************************************************************************

Note [Nesting]

tcPat takes a "thing inside" over which the patter scopes.  This is partly
so that tcPat can extend the environment for the thing_inside, but also 
so that constraints arising in the thing_inside can be discharged by the
pattern.

This does not work so well for the ErrCtxt carried by the monad: we don't
want the error-context for the pattern to scope over the RHS. 
Hence the getErrCtxt/setErrCtxt stuff in tcPat.

\begin{code}
tcPat	:: PatCtxt
	-> LPat Name -> Expected TcSigmaType
	-> TcM a		-- Thing inside
	-> TcM (LPat TcId, 	-- Translated pattern
		[TcTyVar], 	-- Existential binders
		a)		-- Result of thing inside

tcPat ctxt pat exp_ty thing_inside
  = do	{ err_ctxt <- getErrCtxt
	; maybeAddErrCtxt (patCtxt (unLoc pat))	$
	    tc_lpat ctxt pat exp_ty $
	      setErrCtxt err_ctxt thing_inside }
	-- Restore error context before doing thing_inside
	-- See note [Nesting] above

--------------------
tcPats	:: PatCtxt
	-> [LPat Name] 
	-> [Expected TcSigmaType]	-- Excess types discarded
	-> TcM a
	-> TcM ([LPat TcId], [TcTyVar], a)

tcPats ctxt [] _ thing_inside
  = do	{ res <- thing_inside
	; return ([], [], res) }

tcPats ctxt (p:ps) (ty:tys) thing_inside
  = do 	{ (p', p_tvs, (ps', ps_tvs, res)) 
		<- tcPat ctxt p ty $
		   tcPats ctxt ps tys thing_inside
	; return (p':ps', p_tvs ++ ps_tvs, res) }

--------------------
tcCheckPats :: PatCtxt
	    -> [LPat Name] -> [TcSigmaType]
	    -> TcM a 
	    -> TcM ([LPat TcId], [TcTyVar], a)
tcCheckPats ctxt pats tys thing_inside 	-- A trivial wrapper
  = tcPats ctxt pats (map Check tys) thing_inside
\end{code}


%************************************************************************
%*									*
		Binders
%*									*
%************************************************************************

\begin{code}
data PatCtxt = LamPat 		-- Used for lambda, case, do-notation etc
	     | LetPat TcSigFun	-- Used for let(rec) bindings

-------------------
tcPatBndr :: PatCtxt -> Name -> Expected TcSigmaType -> TcM TcId
tcPatBndr LamPat bndr_name pat_ty
  = do	{ pat_ty' <- zapExpectedType pat_ty argTypeKind
		-- If pat_ty is Expected, this returns the appropriate
		-- SigmaType.  In Infer mode, we create a fresh type variable.
		-- Note the SigmaType: we can get
		-- 	data T = MkT (forall a. a->a)
		-- 	f t = case t of { MkT g -> ... }
		-- Here, the 'g' must get type (forall a. a->a) from the
		-- MkT context
	; return (mkLocalId bndr_name pat_ty') }

tcPatBndr (LetPat lookup_sig) bndr_name pat_ty
  | Just sig <- lookup_sig bndr_name
  = do	{ let mono_ty = sig_tau sig
	; mono_name <- newLocalName bndr_name
	; tcSubPat mono_ty pat_ty
	; return (mkLocalId mono_name mono_ty) }

  | otherwise
  = do	{ mono_name <- newLocalName bndr_name
	; pat_ty' <- zapExpectedType pat_ty argTypeKind
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
		tc_pat: the main worker function
%*									*
%************************************************************************

\begin{code}
tc_lpat	:: PatCtxt
	-> LPat Name -> Expected TcSigmaType
	-> TcM a		-- Thing inside
	-> TcM (LPat TcId, 	-- Translated pattern
		[TcTyVar], 	-- Existential binders
		a)		-- Result of thing inside

tc_lpat ctxt (L span pat) pat_ty thing_inside
  = setSrcSpan span $ 
	-- It's OK to keep setting the SrcSpan; 
	-- it just overwrites the previous value
    do	{ (pat', tvs, res) <- tc_pat ctxt pat pat_ty thing_inside
	; return (L span pat', tvs, res) }

---------------------
tc_pat ctxt (VarPat name) pat_ty thing_inside
  = do	{ id <- tcPatBndr ctxt name pat_ty
	; (res, binds) <- bindInstsOfPatId id $
			  tcExtendIdEnv1 name id $
			  (traceTc (text "binding" <+> ppr name <+> ppr (idType id))
			   >> thing_inside)
	; let pat' | isEmptyLHsBinds binds = VarPat id
		   | otherwise		   = VarPatOut id binds
	; return (pat', [], res) }

tc_pat ctxt (ParPat pat) pat_ty thing_inside
  = do	{ (pat', tvs, res) <- tc_lpat ctxt pat pat_ty thing_inside
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
tc_pat ctxt lpat@(LazyPat pat) pat_ty thing_inside
  = do	{ reft <- getTypeRefinement
	; (pat', pat_tvs, res) <- tc_lpat ctxt pat pat_ty $
				  setTypeRefinement reft thing_inside
	; if (null pat_tvs) then return ()
	  else lazyPatErr lpat pat_tvs
	; return (LazyPat pat', [], res) }

tc_pat ctxt (WildPat _) pat_ty thing_inside
  = do	{ pat_ty' <- zapExpectedType pat_ty argTypeKind
	-- Note argTypeKind, so that
	--	f _ = 3
	-- is rejected when f applied to an unboxed tuple
	-- However, this means that 
	--	(case g x of _ -> ...)
	-- is rejected g returns an unboxed tuple, which is perhpas
	-- annoying.  I suppose we could pass the context into tc_pat...
	; res <- thing_inside
	; return (WildPat pat_ty', [], res) }

tc_pat ctxt (AsPat (L nm_loc name) pat) pat_ty thing_inside
  = do	{ bndr_id <- setSrcSpan nm_loc (tcPatBndr ctxt name pat_ty)
	; (pat', tvs, res) <- tcExtendIdEnv1 name bndr_id $
			      tc_lpat ctxt pat (Check (idType bndr_id)) thing_inside
	    -- NB: if we do inference on:
	    --		\ (y@(x::forall a. a->a)) = e
	    -- we'll fail.  The as-pattern infers a monotype for 'y', which then
	    -- fails to unify with the polymorphic type for 'x'.  This could 
	    -- perhaps be fixed, but only with a bit more work.
	    --
	    -- If you fix it, don't forget the bindInstsOfPatIds!
	; return (AsPat (L nm_loc bndr_id) pat', tvs, res) }

tc_pat ctxt (SigPatIn pat sig) pat_ty thing_inside
  = do	{ 	-- See Note [Pattern coercions] below
	  (sig_tvs, sig_ty) <- tcHsPatSigType PatSigCtxt sig
 	; tcSubPat sig_ty pat_ty
	; subst <- refineTyVars sig_tvs	-- See note [Type matching]
	; let tv_binds = [(tyVarName tv, substTyVar subst tv) | tv <- sig_tvs]
	      sig_ty'  = substTy subst sig_ty
	; (pat', tvs, res) 
	      <- tcExtendTyVarEnv2 tv_binds $
		 tc_lpat ctxt pat (Check sig_ty') thing_inside

	; return (SigPatOut pat' sig_ty, tvs, res) }

tc_pat ctxt pat@(TypePat ty) pat_ty thing_inside
  = failWithTc (badTypePat pat)

------------------------
-- Lists, tuples, arrays
tc_pat ctxt (ListPat pats _) pat_ty thing_inside
  = do	{ elem_ty <- zapToListTy pat_ty
	; (pats', pats_tvs, res) <- tcCheckPats ctxt pats (repeat elem_ty) thing_inside
 	; return (ListPat pats' elem_ty, pats_tvs, res) }

tc_pat ctxt (PArrPat pats _) pat_ty thing_inside
  = do	{ [elem_ty] <- zapToTyConApp parrTyCon pat_ty
	; (pats', pats_tvs, res) <- tcCheckPats ctxt pats (repeat elem_ty) thing_inside
 	; return (PArrPat pats' elem_ty, pats_tvs, res) }

tc_pat ctxt (TuplePat pats boxity) pat_ty thing_inside
  = do	{ let arity = length pats
	      tycon = tupleTyCon boxity arity
	; arg_tys <- zapToTyConApp tycon pat_ty
	; (pats', pats_tvs, res) <- tcCheckPats ctxt pats arg_tys thing_inside

	-- Under flag control turn a pattern (x,y,z) into ~(x,y,z)
	-- so that we can experiment with lazy tuple-matching.
	-- This is a pretty odd place to make the switch, but
	-- it was easy to do.
	; let unmangled_result = TuplePat pats' boxity
	      possibly_mangled_result
	        | opt_IrrefutableTuples && isBoxed boxity = LazyPat (noLoc unmangled_result)
	        | otherwise			          = unmangled_result

 	; ASSERT( length arg_tys == arity )	-- Syntactically enforced
	  return (possibly_mangled_result, pats_tvs, res) }

------------------------
-- Data constructors
tc_pat ctxt pat_in@(ConPatIn (L con_span con_name) arg_pats) pat_ty thing_inside
  = do	{ data_con <- tcLookupDataCon con_name
	; let tycon = dataConTyCon data_con
	; ty_args <- zapToTyConApp tycon pat_ty
	; (pat', tvs, res) <- tcConPat ctxt con_span data_con tycon ty_args arg_pats thing_inside
	; return (pat', tvs, res) }

------------------------
-- Literal patterns
tc_pat ctxt (LitPat simple_lit) pat_ty thing_inside
  = do	{ 	-- All other simple lits
	  zapExpectedTo pat_ty (hsLitType simple_lit)
	; res <- thing_inside
	; returnM (LitPat simple_lit, [], res) }

------------------------
-- Overloaded patterns: n, and n+k
tc_pat ctxt pat@(NPat over_lit mb_neg eq _) pat_ty thing_inside
  = do	{ pat_ty' <- zapExpectedType pat_ty liftedTypeKind
	; let orig = LiteralOrigin over_lit
	; lit'    <- tcOverloadedLit orig over_lit pat_ty'
	; eq'     <- tcSyntaxOp orig eq (mkFunTys [pat_ty', pat_ty'] boolTy)
	; mb_neg' <- case mb_neg of
			Nothing  -> return Nothing	-- Positive literal
			Just neg -> 	-- Negative literal
					-- The 'negate' is re-mappable syntax
 			    do { neg' <- tcSyntaxOp orig neg (mkFunTy pat_ty' pat_ty')
			       ; return (Just neg') }
	; res <- thing_inside
	; returnM (NPat lit' mb_neg' eq' pat_ty', [], res) }

tc_pat ctxt pat@(NPlusKPat (L nm_loc name) lit ge minus) pat_ty thing_inside
  = do	{ bndr_id <- setSrcSpan nm_loc (tcPatBndr ctxt name pat_ty)
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
    
	; res <- tcExtendIdEnv1 name bndr_id thing_inside
	; returnM (NPlusKPat (L nm_loc bndr_id) lit' ge' minus', [], res) }
\end{code}


%************************************************************************
%*									*
	Most of the work for constructors is here
	(the rest is in the ConPatIn case of tc_pat)
%*									*
%************************************************************************

\begin{code}
tcConPat :: PatCtxt -> SrcSpan -> DataCon -> TyCon -> [TcTauType] 
	 -> HsConDetails Name (LPat Name) -> TcM a
	 -> TcM (Pat TcId, [TcTyVar], a)
tcConPat ctxt span data_con tycon ty_args arg_pats thing_inside
  | isVanillaDataCon data_con
  = do	{ let arg_tys = dataConInstOrigArgTys data_con ty_args
	; tcInstStupidTheta data_con ty_args
	; traceTc (text "tcConPat" <+> vcat [ppr data_con, ppr ty_args, ppr arg_tys])
	; (arg_pats', tvs, res) <- tcConArgs ctxt data_con arg_pats arg_tys thing_inside
	; return (ConPatOut (L span data_con) [] [] emptyLHsBinds 
			    arg_pats' (mkTyConApp tycon ty_args),
		  tvs, res) }

  | otherwise	-- GADT case
  = do	{ let (tvs, theta, arg_tys, _, res_tys) = dataConSig data_con
	; span <- getSrcSpanM
	; let rigid_info = PatSkol data_con span
	; tvs' <- tcSkolTyVars rigid_info tvs
	; let tv_tys'  = mkTyVarTys tvs'
	      tenv     = zipTopTvSubst tvs tv_tys'
	      theta'   = substTheta tenv theta
	      arg_tys' = substTys tenv arg_tys
	      res_tys' = substTys tenv res_tys
	; dicts <- newDicts (SigOrigin rigid_info) theta'

	-- Do type refinement!
	; traceTc (text "tcGadtPat" <+> vcat [ppr data_con, ppr tvs', ppr arg_tys', ppr res_tys', 
					      text "ty-args:" <+> ppr ty_args ])
	; refineAlt ctxt data_con tvs' ty_args res_tys' $ do	

	{ ((arg_pats', inner_tvs, res), lie_req) <- getLIE $
		do { tcInstStupidTheta data_con tv_tys'
			-- The stupid-theta mentions the newly-bound tyvars, so
			-- it must live inside the getLIE, so that the
			--  tcSimplifyCheck will apply the type refinement to it
		   ; tcConArgs ctxt data_con arg_pats arg_tys' thing_inside }

	; dict_binds <- tcSimplifyCheck doc tvs' dicts lie_req

	; return (ConPatOut (L span data_con)
			    tvs' (map instToId dicts) dict_binds
			    arg_pats' (mkTyConApp tycon ty_args),
		  tvs' ++ inner_tvs, res) } }
  where
    doc = ptext SLIT("existential context for") <+> quotes (ppr data_con)

tcConArgs :: PatCtxt -> DataCon 
	   -> HsConDetails Name (LPat Name) -> [TcSigmaType]
	   -> TcM a
	   -> TcM (HsConDetails TcId (LPat Id), [TcTyVar], a)

tcConArgs ctxt data_con (PrefixCon arg_pats) arg_tys thing_inside
  = do	{ checkTc (con_arity == no_of_args)	-- Check correct arity
		  (arityErr "Constructor" data_con con_arity no_of_args)
	; (arg_pats', tvs, res) <- tcCheckPats ctxt arg_pats arg_tys thing_inside
	; return (PrefixCon arg_pats', tvs, res) }
  where
    con_arity  = dataConSourceArity data_con
    no_of_args = length arg_pats

tcConArgs ctxt data_con (InfixCon p1 p2) arg_tys thing_inside
  = do	{ checkTc (con_arity == 2)	-- Check correct arity
	 	  (arityErr "Constructor" data_con con_arity 2)
	; ([p1',p2'], tvs, res) <- tcCheckPats ctxt [p1,p2] arg_tys thing_inside
	; return (InfixCon p1' p2', tvs, res) }
  where
    con_arity  = dataConSourceArity data_con

tcConArgs ctxt data_con (RecCon rpats) arg_tys thing_inside
  = do	{ (rpats', tvs, res) <- tc_fields rpats thing_inside
	; return (RecCon rpats', tvs, res) }
  where
    tc_fields :: [(Located Name, LPat Name)] -> TcM a
	      -> TcM ([(Located TcId, LPat TcId)], [TcTyVar], a)
    tc_fields [] thing_inside
      = do { res <- thing_inside
	   ; return ([], [], res) }

    tc_fields (rpat : rpats) thing_inside
      =	do { (rpat', tvs1, (rpats', tvs2, res)) 
		<- tc_field rpat (tc_fields rpats thing_inside)
	   ; return (rpat':rpats', tvs1 ++ tvs2, res) }

    tc_field (field_lbl, pat) thing_inside
      = do { (sel_id, pat_ty) <- wrapLocFstM find_field_ty field_lbl
	   ; (pat', tvs, res) <- tcPat ctxt pat (Check pat_ty) thing_inside
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
		    ; bogus_ty <- newTyFlexiVarTy liftedTypeKind
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
refineAlt :: PatCtxt -> DataCon
	    -> [TcTyVar] 	-- Freshly bound type variables
	    -> [TcType] 	-- Types from the scrutinee (context)
	    -> [TcType]		-- Types from the pattern
	    -> TcM a -> TcM a
refineAlt ctxt con ex_tvs ctxt_tys pat_tys thing_inside 
  = do	{ old_subst <- getTypeRefinement
	; case gadtRefineTys bind_fn old_subst pat_tys ctxt_tys of
		Failed msg -> failWithTc (inaccessibleAlt msg)
		Succeeded new_subst -> do {
	  traceTc (text "refineTypes:match" <+> ppr con <+> ppr new_subst)
	; setTypeRefinement new_subst thing_inside } }

  where
    bind_fn tv | isMetaTyVar tv = WildCard	-- Wobbly types behave as wild cards
	       | otherwise	= BindMe
\end{code}

Note [Type matching]
~~~~~~~~~~~~~~~~~~~~
This little function @refineTyVars@ is a little tricky.  Suppose we have a pattern type
signature
	f = \(x :: Term a) -> body
Then 'a' should be bound to a wobbly type.  But if we have
	f :: Term b -> some-type
	f = \(x :: Term a) -> body
then 'a' should be bound to the rigid type 'b'.  So we want to
	* instantiate the type sig with fresh meta tyvars (e.g. \alpha)
	* unify with the type coming from the context
	* read out whatever information has been gleaned
		from that unificaiton (e.g. unifying \alpha with 'b')
	* and replace \alpha by 'b' in the type, when typechecking the
		pattern inside the type sig (x in this case)
It amounts to combining rigid info from the context and from the sig.

Exactly the same thing happens for 'smart function application'.

\begin{code}
refineTyVars :: [TcTyVar] 	-- Newly instantiated meta-tyvars of the function
	     -> TcM TvSubst	-- Substitution mapping any of the meta-tyvars that
				-- has been unifies to what it was instantiated to
-- Just one level of de-wobblification though.  What a hack! 
refineTyVars tvs
  = do	{ mb_prs <- mapM mk_pr tvs
	; return (mkOpenTvSubst (mkVarEnv (catMaybes mb_prs))) }
  where
    mk_pr tv = do { details <- readMetaTyVar tv
		  ; case details of
			Indirect ty -> return (Just (tv,ty))
			other	    -> return Nothing 
		  }
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
patCtxt pat 	    = Just (hang (ptext SLIT("When checking the pattern:")) 
			       4 (ppr pat))

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
       2 (vcat (map pprTcTyVar tvs))

inaccessibleAlt msg
  = hang (ptext SLIT("Inaccessible case alternative:")) 2 msg
\end{code}
