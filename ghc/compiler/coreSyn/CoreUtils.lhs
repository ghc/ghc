%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreUtils]{Utility functions on @Core@ syntax}

\begin{code}
module CoreUtils (
	IdSubst, SubstCoreExpr(..),

	coreExprType, coreAltsType, exprFreeVars, exprSomeFreeVars,

	exprIsBottom, exprIsDupable, exprIsTrivial, exprIsWHNF, exprIsCheap,
	FormSummary(..), mkFormSummary, whnfOrBottom,
	cheapEqExpr,

	substExpr, substId, substIds,
	idSpecVars, idFreeVars,

	squashableDictishCcExpr
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} CoreUnfold	( noUnfolding, hasUnfolding )

import CoreSyn
import PprCore		()	-- Instances only
import Var		( IdOrTyVar, isId, isTyVar )
import VarSet
import VarEnv
import Name		( isLocallyDefined )
import Const		( Con(..), isWHNFCon, conIsTrivial, conIsCheap )
import Id		( Id, idType, setIdType, idUnique, idAppIsBottom,
			  getIdArity, idFreeTyVars,
			  getIdSpecialisation, setIdSpecialisation,
			  getInlinePragma, setInlinePragma,
			  getIdUnfolding, setIdUnfolding
			)
import IdInfo		( arityLowerBound, InlinePragInfo(..) )
import SpecEnv		( emptySpecEnv, specEnvToList, isEmptySpecEnv )
import CostCentre	( isDictCC, CostCentre )
import Const		( Con, conType )
import Type		( Type, TyVarSubst, mkFunTy, mkForAllTy,
			  splitFunTy_maybe, applyTys, tyVarsOfType, tyVarsOfTypes,
			  fullSubstTy, substTyVar )
import Unique		( buildIdKey, augmentIdKey )
import Util		( zipWithEqual, mapAccumL )
import Outputable
import TysPrim		( alphaTy )	-- Debgging only
\end{code}


%************************************************************************
%*									*
\subsection{Substitutions}
%*									*
%************************************************************************

\begin{code}
type IdSubst = IdEnv SubstCoreExpr		-- Maps Ids to SubstCoreExpr

data SubstCoreExpr
  = Done    CoreExpr			-- No more substitution needed
  | SubstMe CoreExpr TyVarSubst IdSubst	-- A suspended substitution
\end{code}

%************************************************************************
%*									*
\subsection{Find the type of a Core atom/expression}
%*									*
%************************************************************************

\begin{code}
coreExprType :: CoreExpr -> Type

coreExprType (Var var)		    = idType var
coreExprType (Let _ body)	    = coreExprType body
coreExprType (Case _ _ alts)        = coreAltsType alts
coreExprType (Note (Coerce ty _) e) = ty
coreExprType (Note other_note e)    = coreExprType e
coreExprType e@(Con con args)       = applyTypeToArgs e (conType con) args

coreExprType (Lam binder expr)
  | isId binder    = idType binder `mkFunTy` coreExprType expr
  | isTyVar binder = mkForAllTy binder (coreExprType expr)

coreExprType e@(App _ _)
  = case collectArgs e of
	(fun, args) -> applyTypeToArgs e (coreExprType fun) args

coreExprType other = pprTrace "coreExprType" (ppr other) alphaTy

coreAltsType :: [CoreAlt] -> Type
coreAltsType ((_,_,rhs) : _) = coreExprType rhs
\end{code}

\begin{code}
-- The "e" argument is just for debugging

applyTypeToArgs e op_ty [] = op_ty

applyTypeToArgs e op_ty (Type ty : args)
  =	-- Accumulate type arguments so we can instantiate all at once
    applyTypeToArgs e (applyTys op_ty tys) rest_args
  where
    (tys, rest_args)        = go [ty] args
    go tys (Type ty : args) = go (ty:tys) args
    go tys rest_args	    = (reverse tys, rest_args)

applyTypeToArgs e op_ty (other_arg : args)
  = case (splitFunTy_maybe op_ty) of
	Just (_, res_ty) -> applyTypeToArgs e res_ty args
	Nothing -> pprPanic "applyTypeToArgs" (ppr e)
\end{code}


%************************************************************************
%*									*
\subsection{Figuring out things about expressions}
%*									*
%************************************************************************

\begin{code}
data FormSummary
  = VarForm		-- Expression is a variable (or scc var, etc)
  | ValueForm		-- Expression is a value: i.e. a value-lambda,constructor, or literal
  | BottomForm		-- Expression is guaranteed to be bottom. We're more gung
			-- ho about inlining such things, because it can't waste work
  | OtherForm		-- Anything else

instance Outputable FormSummary where
   ppr VarForm    = ptext SLIT("Var")
   ppr ValueForm  = ptext SLIT("Value")
   ppr BottomForm = ptext SLIT("Bot")
   ppr OtherForm  = ptext SLIT("Other")

whnfOrBottom :: FormSummary -> Bool
whnfOrBottom VarForm    = True
whnfOrBottom ValueForm  = True
whnfOrBottom BottomForm = True
whnfOrBottom OtherForm  = False
\end{code}

\begin{code}
mkFormSummary :: CoreExpr -> FormSummary
mkFormSummary expr
  = go (0::Int) expr	-- The "n" is the number of *value* arguments so far
  where
    go n (Con con _) | isWHNFCon con = ValueForm
		     | otherwise     = OtherForm

    go n (Note _ e)         = go n e

    go n (Let (NonRec b r) e) | exprIsTrivial r = go n e	-- let f = f' alpha in (f,g) 
								-- should be treated as a value
    go n (Let _ e)    = OtherForm
    go n (Case _ _ _) = OtherForm

    go 0 (Lam x e) | isId x    = ValueForm	-- NB: \x.bottom /= bottom!
    		   | otherwise = go 0 e
    go n (Lam x e) | isId x    = go (n-1) e	-- Applied lambda
		   | otherwise = go n e

    go n (App fun (Type _)) = go n fun		-- Ignore type args
    go n (App fun arg)      = go (n+1) fun

    go n (Var f) | idAppIsBottom f n = BottomForm
    go 0 (Var f)		     = VarForm
    go n (Var f) | n < arityLowerBound (getIdArity f) = ValueForm
		 | otherwise			      = OtherForm
\end{code}

@exprIsTrivial@	is true of expressions we are unconditionally 
		happy to duplicate; simple variables and constants,
		and type applications.

@exprIsDupable@	is true of expressions that can be duplicated at a modest
		cost in space, but without duplicating any work.


@exprIsBottom@	is true of expressions that are guaranteed to diverge


\begin{code}
exprIsTrivial (Type _)	     = True
exprIsTrivial (Var v) 	     = True
exprIsTrivial (App e arg)    = isTypeArg arg && exprIsTrivial e
exprIsTrivial (Note _ e)     = exprIsTrivial e
exprIsTrivial (Con con args) = conIsTrivial con && all isTypeArg args
exprIsTrivial (Lam b body)   | isTyVar b = exprIsTrivial body
exprIsTrivial other	     = False
\end{code}


\begin{code}
exprIsDupable (Type _)	     = True
exprIsDupable (Con con args) = conIsCheap con && 
			       all exprIsDupable args &&
			       valArgCount args <= dupAppSize

exprIsDupable (Note _ e)     = exprIsDupable e
exprIsDupable expr	     = case collectArgs expr of  
				  (Var v, args) -> n_val_args == 0 ||
						   (n_val_args < fun_arity &&
						    all exprIsDupable args &&
						    n_val_args <= dupAppSize)
						where
						   n_val_args = valArgCount args
						   fun_arity = arityLowerBound (getIdArity v)
									
				  _	        -> False

dupAppSize :: Int
dupAppSize = 4		-- Size of application we are prepared to duplicate
\end{code}

@exprIsCheap@ looks at a Core expression and returns \tr{True} if
it is obviously in weak head normal form, or is cheap to get to WHNF.
[Note that that's not the same as exprIsDupable; an expression might be
big, and hence not dupable, but still cheap.]
By ``cheap'' we mean a computation we're willing to push inside a lambda 
in order to bring a couple of lambdas together.  That might mean it gets
evaluated more than once, instead of being shared.  The main examples of things
which aren't WHNF but are ``cheap'' are:

  * 	case e of
	  pi -> ei

	where e, and all the ei are cheap; and

  *	let x = e
	in b

	where e and b are cheap; and

  *	op x1 ... xn

	where op is a cheap primitive operator

\begin{code}
exprIsCheap :: CoreExpr -> Bool
exprIsCheap (Type _)        	= True
exprIsCheap (Var _)         	= True
exprIsCheap (Con con args)  	= conIsCheap con && all exprIsCheap args
exprIsCheap (Note _ e)      	= exprIsCheap e
exprIsCheap (Lam x e)       	= if isId x then True else exprIsCheap e
exprIsCheap (Let bind body) 	= all exprIsCheap (rhssOfBind bind) && exprIsCheap body
exprIsCheap (Case scrut _ alts) = exprIsCheap scrut && 
				  all (\(_,_,rhs) -> exprIsCheap rhs) alts

exprIsCheap other_expr   -- look for manifest partial application
  = case collectArgs other_expr of

      (Var f, args) |  idAppIsBottom f (length args)
		    -> True	-- Application of a function which
				-- always gives bottom; we treat this as
				-- a WHNF, because it certainly doesn't
				-- need to be shared!

      (Var f, args) ->
		let
		    num_val_args = valArgCount args
		in
		num_val_args == 0 ||	-- Just a type application of
					-- a variable (f t1 t2 t3)
					-- counts as WHNF
		num_val_args < arityLowerBound (getIdArity f)

      _ -> False
\end{code}


\begin{code}
exprIsBottom :: CoreExpr -> Bool	-- True => definitely bottom
exprIsBottom e = go 0 e
	       where
		-- n is the number of args
		 go n (Note _ e)   = go n e
		 go n (Let _ e)    = go n e
		 go n (Case e _ _) = go 0 e	-- Just check the scrut
		 go n (App e _)    = go (n+1) e
		 go n (Var v)      = idAppIsBottom v n
		 go n (Con _ _)    = False
		 go n (Lam _ _)	   = False
\end{code}

exprIsWHNF reports True for head normal forms.  Note that does not necessarily
mean *normal* forms; constructors might have non-trivial argument expressions, for
example.  We use a let binding for WHNFs, rather than a case binding, even if it's
used strictly.  We try to expose WHNFs by floating lets out of the RHS of lets.

We treat applications of buildId and augmentId as honorary WHNFs, because we
want them to get exposed

\begin{code}
exprIsWHNF :: CoreExpr -> Bool	-- True => Variable, value-lambda, constructor, PAP
exprIsWHNF (Type ty)	      = True	-- Types are honorary WHNFs; we don't mind
					-- copying them
exprIsWHNF (Var v)    	      = True
exprIsWHNF (Lam b e)  	      = isId b || exprIsWHNF e
exprIsWHNF (Note _ e) 	      = exprIsWHNF e
exprIsWHNF (Let _ e)          = False
exprIsWHNF (Case _ _ _)       = False
exprIsWHNF (Con con _)        = isWHNFCon con 
exprIsWHNF e@(App _ _)        = case collectArgs e of  
				  (Var v, args) -> n_val_args == 0 || 
						   fun_arity > n_val_args ||
						   v_uniq == buildIdKey ||
						   v_uniq == augmentIdKey
						where
						   n_val_args = valArgCount args
						   fun_arity  = arityLowerBound (getIdArity v)
						   v_uniq     = idUnique v

				  _	        -> False
\end{code}

I don't like this function but I'n not confidnt enough to change it.

\begin{code}
squashableDictishCcExpr :: CostCentre -> Expr b -> Bool
squashableDictishCcExpr cc expr
  | isDictCC cc = False		-- that was easy...
  | otherwise   = squashable expr
  where
    squashable (Var _)      = True
    squashable (Con  _ _)   = True -- I think so... WDP 94/09
    squashable (App f a)
      | isTypeArg a	    = squashable f
    squashable other	    = False
\end{code}


@cheapEqExpr@ is a cheap equality test which bales out fast!
	True  => definitely equal
	False => may or may not be equal

\begin{code}
cheapEqExpr :: Expr b -> Expr b -> Bool

cheapEqExpr (Var v1) (Var v2) = v1==v2
cheapEqExpr (Con con1 args1) (Con con2 args2)
  = con1 == con2 && 
    and (zipWithEqual "cheapEqExpr" cheapEqExpr args1 args2)

cheapEqExpr (App f1 a1) (App f2 a2)
  = f1 `cheapEqExpr` f2 && a1 `cheapEqExpr` a2

cheapEqExpr (Type t1) (Type t2) = t1 == t2

cheapEqExpr _ _ = False
\end{code}


%************************************************************************
%*									*
\section{Finding the free variables of an expression}
%*									*
%************************************************************************

This function simply finds the free variables of an expression.
So far as type variables are concerned, it only finds tyvars that are

	* free in type arguments, 
	* free in the type of a binder,

but not those that are free in the type of variable occurrence.

\begin{code}
exprFreeVars :: CoreExpr -> IdOrTyVarSet	-- Find all locally-defined free Ids or tyvars
exprFreeVars = exprSomeFreeVars isLocallyDefined

exprSomeFreeVars :: InterestingVarFun 	-- Says which Vars are interesting
		-> CoreExpr
		-> IdOrTyVarSet
exprSomeFreeVars fv_cand e = expr_fvs e fv_cand emptyVarSet

type InterestingVarFun = IdOrTyVar -> Bool	-- True <=> interesting
\end{code}


\begin{code}
type FV = InterestingVarFun 
	  -> IdOrTyVarSet	-- In scope
	  -> IdOrTyVarSet	-- Free vars

union :: FV -> FV -> FV
union fv1 fv2 fv_cand in_scope = fv1 fv_cand in_scope `unionVarSet` fv2 fv_cand in_scope

noVars :: FV
noVars fv_cand in_scope = emptyVarSet

oneVar :: IdOrTyVar -> FV
oneVar var fv_cand in_scope
  | keep_it fv_cand in_scope var = unitVarSet var
  | otherwise			 = emptyVarSet

someVars :: IdOrTyVarSet -> FV
someVars vars fv_cand in_scope
  = filterVarSet (keep_it fv_cand in_scope) vars

keep_it fv_cand in_scope var
  | var `elemVarSet` in_scope = False
  | fv_cand var		      = True
  | otherwise		      = False


addBndr :: CoreBndr -> FV -> FV
addBndr bndr fv fv_cand in_scope
  | isId bndr = inside_fvs `unionVarSet` someVars (idFreeVars bndr) fv_cand in_scope
  | otherwise = inside_fvs
  where
    inside_fvs = fv fv_cand (in_scope `extendVarSet` bndr) 

addBndrs :: [CoreBndr] -> FV -> FV
addBndrs bndrs fv = foldr addBndr fv bndrs
\end{code}


\begin{code}
expr_fvs :: CoreExpr -> FV

expr_fvs (Type ty) 	 = someVars (tyVarsOfType ty)
expr_fvs (Var var) 	 = oneVar var
expr_fvs (Con con args)  = foldr (union . expr_fvs) noVars args
expr_fvs (Note _ expr)   = expr_fvs expr
expr_fvs (App fun arg)   = expr_fvs fun `union` expr_fvs arg
expr_fvs (Lam bndr body) = addBndr bndr (expr_fvs body)

expr_fvs (Case scrut bndr alts)
  = expr_fvs scrut `union` addBndr bndr (foldr (union. alt_fvs) noVars alts)
  where
    alt_fvs (con, bndrs, rhs) = addBndrs bndrs (expr_fvs rhs)

expr_fvs (Let (NonRec bndr rhs) body)
  = expr_fvs rhs `union` addBndr bndr (expr_fvs body)

expr_fvs (Let (Rec pairs) body)
  = addBndrs bndrs (foldr (union . expr_fvs) (expr_fvs body) rhss)
  where
    (bndrs,rhss) = unzip pairs
\end{code}


Given an Id, idSpecVars returns all its specialisations.
We extract these from its SpecEnv.
This is used by the occurrence analyser and free-var finder;
we regard an Id's specialisations as free in the Id's definition.

\begin{code}
idSpecVars :: Id -> IdOrTyVarSet
idSpecVars id 
  = foldr (unionVarSet . spec_item_fvs)
	  emptyVarSet 
	  (specEnvToList (getIdSpecialisation id))
  where
    spec_item_fvs (tyvars, tys, rhs) = foldl delVarSet
					     (tyVarsOfTypes tys `unionVarSet` exprFreeVars rhs)
					     tyvars

idFreeVars :: Id -> IdOrTyVarSet
idFreeVars id = idSpecVars id `unionVarSet` idFreeTyVars id
\end{code}


%************************************************************************
%*									*
\section{Substitution}
%*									*
%************************************************************************

This expression substituter deals correctly with name capture, much
like Type.substTy.

BUT NOTE that substExpr silently discards the
	unfolding, and
	spec env
IdInfo attached to any binders in the expression.  It's quite
tricky to do them 'right' in the case of mutually recursive bindings,
and so far has proved unnecessary.

\begin{code}
substExpr :: TyVarSubst -> IdSubst	-- Substitution
	  -> IdOrTyVarSet		-- Superset of in-scope
	  -> CoreExpr
	  -> CoreExpr

substExpr te ve in_scope expr = subst_expr (te, ve, in_scope) expr

subst_expr env@(te, ve, in_scope) expr
  = go expr
  where
    go (Var v) = case lookupVarEnv ve v of
			Just (Done e')
				-> e'

			Just (SubstMe e' te' ve')
				-> subst_expr (te', ve', in_scope) e'

			Nothing -> case lookupVarSet in_scope v of
					Just v' -> Var v'
					Nothing -> Var v
			-- NB: we look up in the in_scope set because the variable
			-- there may have more info. In particular, when substExpr
			-- is called from the simplifier, the type inside the *occurrences*
			-- of a variable may not be right; we should replace it with the
			-- binder, from the in_scope set.

    go (Type ty)      = Type (go_ty ty)
    go (Con con args) = Con con (map go args)
    go (App fun arg)  = App (go fun) (go arg)
    go (Note note e)  = Note (go_note note) (go e)

    go (Lam bndr body) = Lam bndr' (subst_expr env' body)
		       where
			 (env', bndr') = go_bndr env bndr

    go (Let (NonRec bndr rhs) body) = Let (NonRec bndr' (go rhs)) (subst_expr env' body)
				    where
				      (env', bndr') = go_bndr env bndr

    go (Let (Rec pairs) body) = Let (Rec pairs') (subst_expr env' body)
			      where
				(ve', in_scope', _, bndrs') 
				   = substIds clone_fn te ve in_scope undefined (map fst pairs)
				env'    = (te, ve', in_scope')
				pairs'	= bndrs' `zip` rhss'
				rhss'	= map (subst_expr env' . snd) pairs

    go (Case scrut bndr alts) = Case (go scrut) bndr' (map (go_alt env') alts)
			      where
				(env', bndr') = go_bndr env bndr

    go_alt env (con, bndrs, rhs) = (con, bndrs', subst_expr env' rhs)
				 where
				   (env', bndrs') = mapAccumL go_bndr env bndrs

    go_note (Coerce ty1 ty2) = Coerce (go_ty ty1) (go_ty ty2)
    go_note note	     = note

    go_ty ty = fullSubstTy te in_scope ty

    go_bndr (te, ve, in_scope) bndr
	| isTyVar bndr
	= case substTyVar te in_scope bndr of
		(te', in_scope', bndr') -> ((te', ve, in_scope'), bndr')

	| otherwise
	= case substId clone_fn te ve in_scope undefined bndr of
		(ve', in_scope', _, bndr') -> ((te, ve', in_scope'), bndr')


    clone_fn in_scope _ bndr
		| bndr `elemVarSet` in_scope = Just (uniqAway in_scope bndr, undefined)
		| otherwise		     = Nothing
				
\end{code}

Substituting in binders is a rather tricky part of the whole compiler.

\begin{code}
substIds :: (IdOrTyVarSet -> us -> Id -> Maybe (us, Id))	-- Cloner
	 -> TyVarSubst -> IdSubst -> IdOrTyVarSet	-- Usual stuff
	 -> us						-- Unique supply
	 -> [Id]
	 -> (IdSubst, IdOrTyVarSet, 			-- New id_subst, in_scope
	     us, 					-- New unique supply
	     [Id])

substIds clone_fn ty_subst id_subst in_scope us []
  = (id_subst, in_scope, us, [])

substIds clone_fn ty_subst id_subst in_scope us (id:ids)
  = case (substId clone_fn ty_subst id_subst in_scope us id) of {
	(id_subst', in_scope', us', id') -> 

    case (substIds clone_fn ty_subst id_subst' in_scope' us' ids) of {
	(id_subst'', in_scope'', us'', ids') -> 

    (id_subst'', in_scope'', us'', id':ids')
    }}


substId :: (IdOrTyVarSet -> us -> Id -> Maybe (us, Id))	-- Cloner
	-> TyVarSubst -> IdSubst -> IdOrTyVarSet	-- Usual stuff
	-> us						-- Unique supply
	-> Id
	-> (IdSubst, IdOrTyVarSet, 			-- New id_subst, in_scope
	    us, 					-- New unique supply
	    Id)

-- Returns an Id with empty unfolding and spec-env. 
-- It's up to the caller to sort these out.

substId clone_fn 
	ty_subst id_subst in_scope
	us id
  | old_id_will_do
		-- No need to clone, but we *must* zap any current substitution
		-- for the variable.  For example:
		--	(\x.e) with id_subst = [x |-> e']
		-- Here we must simply zap the substitution for x
  = (delVarEnv id_subst id, extendVarSet in_scope id, us, id)

  | otherwise
  = (extendVarEnv id_subst id (Done (Var new_id)), 
     extendVarSet in_scope new_id,
     new_us,
     new_id)
  where
    id_ty	   = idType id
    old_id_will_do = old1 && old2 && old3 && {-old4 && -}not cloned 

       -- id1 has its type zapped
    (id1,old1) |  isEmptyVarEnv ty_subst
	       || isEmptyVarSet (tyVarsOfType id_ty) = (id, True)
 	       | otherwise 			     = (setIdType id ty', False)

    ty' = fullSubstTy ty_subst in_scope id_ty

	-- id2 has its SpecEnv zapped
	-- It's filled in later by Simplify.simplPrags
    (id2,old2) | isEmptySpecEnv spec_env = (id1, True)
	       | otherwise  	         = (setIdSpecialisation id1 emptySpecEnv, False)
    spec_env  = getIdSpecialisation id

	-- id3 has its Unfolding zapped
	-- This is very important; occasionally a let-bound binder is used
	-- as a binder in some lambda, in which case its unfolding is utterly
	-- bogus.  Also the unfolding uses old binders so if we left it we'd
	-- have to substitute it. Much better simply to give the Id a new
	-- unfolding each time, which is what the simplifier does.
    (id3,old3) | hasUnfolding (getIdUnfolding id) = (id2 `setIdUnfolding` noUnfolding, False)
	       | otherwise			  = (id2, True)

	-- new_id is cloned if necessary
    (new_us, new_id, cloned) = case clone_fn in_scope us id3 of
				  Nothing         -> (us,  id3, False)
				  Just (us', id') -> (us', id', True)

        -- new_id_bndr has its Inline info neutered.  We must forget about whether it
        -- was marked safe-to-inline, because that isn't necessarily true in
        -- the simplified expression.  We do this for the *binder* which will
	-- be used at the binding site, but we *dont* do it for new_id, which
	-- is put into the in_scope env.  Why not?  Because the in_scope env
	-- carries down the occurrence information to usage sites! 
	--
	-- Net result: post-simplification, occurrences may have over-optimistic
	-- occurrence info, but binders won't.
{-    (new_id_bndr, old4)
	= case getInlinePragma id of
		ICanSafelyBeINLINEd _ _ -> (setInlinePragma new_id NoInlinePragInfo, False)
		other		        -> (new_id, True)
-}
\end{code}





