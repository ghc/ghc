%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[SimplEnv]{Environment stuff for the simplifier}

\begin{code}
module SimplEnv (
	nullSimplEnv, 
	getEnvs, setTyEnv, setIdEnv, notInScope, setSubstEnvs, zapSubstEnvs,

	bindTyVar, bindTyVars, simplTy,

	lookupIdSubst, lookupOutIdEnv, 

	bindIdToAtom, bindIdToExpr,

	markDangerousOccs,
	lookupUnfolding, isEvaluated,
	extendEnvGivenBinding, extendEnvGivenNewRhs,
	extendEnvGivenUnfolding,

	lookForConstructor,

	getSwitchChecker, switchIsSet, getSimplIntSwitch, 
	switchOffInlining, setCaseScrutinee,

	setEnclosingCC, getEnclosingCC,

	-- Types
	SwitchChecker,
	SimplEnv, 
	UnfoldConApp,
	SubstInfo(..),

	InId,  InBinder,  InBinding,  InType,
	OutId, OutBinder, OutBinding, OutType,

	InExpr,  InAlts,  InDefault,  InArg,
	OutExpr, OutAlts, OutDefault, OutArg
    ) where

#include "HsVersions.h"

import BinderInfo	( orBinderInfo, andBinderInfo, noBinderInfo, isOneOcc,
			  okToInline, isOneFunOcc,
			  BinderInfo
			)
import CmdLineOpts	( switchIsOn, intSwitchSet, opt_UnfoldingCreationThreshold,
			  SimplifierSwitch(..), SwitchResult(..)
			)
import CoreSyn
import CoreUnfold	( mkFormSummary, couldBeSmallEnoughToInline, whnfOrBottom,
			  Unfolding(..), FormSummary(..),
			  calcUnfoldingGuidance	)
import CoreUtils	( coreExprCc )
import CostCentre	( CostCentre, isCurrentCostCentre, useCurrentCostCentre, 
			  costsAreSubsumed, noCostCentreAttached, subsumedCosts,
			  currentOrSubsumedCosts
			)
import FiniteMap	-- lots of things
import Id		( getInlinePragma,
			  nullIdEnv, growIdEnvList, lookupIdEnv, delOneFromIdEnv,
			  addOneToIdEnv, modifyIdEnv, modifyIdEnv_Directly,
			  IdEnv, IdSet, Id )
import Literal		( Literal )
import Maybes		( expectJust )
import OccurAnal	( occurAnalyseExpr )
import PprCore		-- various instances
import Type		( instantiateTy, Type )
import TyVar		( TyVarEnv, emptyTyVarEnv, plusTyVarEnv, addToTyVarEnv, growTyVarEnvList,
			  TyVarSet, emptyTyVarSet,
			  TyVar
			)
import Unique		( Unique{-instance Outputable-}, Uniquable(..) )
import UniqFM		( addToUFM, addToUFM_C, ufmToList, mapUFM )
import Util		( Eager, returnEager, zipEqual, thenCmp, cmpList )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InId      = Id			-- Not yet cloned
type InBinder  = (InId, BinderInfo)
type InType    = Type			-- Ditto
type InBinding = SimplifiableCoreBinding
type InExpr    = SimplifiableCoreExpr
type InAlts    = SimplifiableCoreCaseAlts
type InDefault = SimplifiableCoreCaseDefault
type InArg     = SimplifiableCoreArg

type OutId	= Id			-- Cloned
type OutBinder	= Id
type OutType	= Type			-- Cloned
type OutBinding	= CoreBinding
type OutExpr	= CoreExpr
type OutAlts	= CoreCaseAlts
type OutDefault	= CoreCaseDefault
type OutArg	= CoreArg

type SwitchChecker = SimplifierSwitch -> SwitchResult
\end{code}

%************************************************************************
%*									*
\subsubsection{The @SimplEnv@ type}
%*									*
%************************************************************************


INVARIANT: we assume {\em no shadowing}.  (ToDo: How can we ASSERT
this? WDP 94/06) This allows us to neglect keeping everything paired
with its static environment.

The environment contains bindings for all
	{\em in-scope,}
	{\em locally-defined}
things.

For such things, any unfolding is found in the environment, not in the
Id.  Unfoldings in the Id itself are used only for imported things
(otherwise we get trouble because we have to simplify the unfoldings
inside the Ids, etc.).

\begin{code}
data SimplEnv
  = SimplEnv
	SwitchChecker
	CostCentre		-- The enclosing cost-centre (when profiling)
	SimplTypeEnv		-- Maps old type variables to new clones
	SimplValEnv		-- Maps locally-bound Ids to new clones
	ConAppMap		-- Maps constructor applications back to OutIds

type SimplTypeEnv = (TyVarSet,		-- In-scope tyvars (in result)
		     TyVarEnv Type)	-- Type substitution
	-- If t is in the in-scope set, it certainly won't be
	-- in the domain of the substitution, and vice versa

type SimplValEnv = (IdEnv StuffAboutId,	-- Domain includes *all* in-scope 
					-- Ids (in result), range gives info about them
		    IdEnv SubstInfo)	-- Id substitution
	-- The first envt tells what Ids are in scope; it
	-- corresponds to the TyVarSet in SimplTypeEnv

	-- The substitution usually maps an Id to its clone,
	-- but if the orig defn is a let-binding, and
	-- the RHS of the let simplifies to an atom,
	-- we just add the binding to the substitution and elide the let.
	-- 
	-- Ids in the domain of the substitution are *not* in scope;
	-- they *must* be substituted for the given OutArg

data SubstInfo 
  = SubstVar OutId		-- The Id maps to an already-substituted atom
  | SubstLit Literal		-- ...ditto literal
  | SubstExpr 			-- Id maps to an as-yet-unsimplified expression
	(TyVarEnv Type) 	-- ...hence we need to capture the substitution
	(IdEnv SubstInfo)	--    environments too
	SimplifiableCoreExpr
	
type StuffAboutId = (OutId, 		-- Always has the same unique as the
					-- Id that maps to it; but may have better
					-- IdInfo, and a correctly-substituted type,
					-- than the occurrences of the Id.  So use
					-- this to replace occurrences

		     BinderInfo,	-- How it occurs
					-- We keep this info so we can modify it when 
					-- something changes. 

		     Unfolding)		-- Info about what it is bound to
\end{code}


\begin{code}
nullSimplEnv :: SwitchChecker -> SimplEnv

nullSimplEnv sw_chkr
  = SimplEnv sw_chkr subsumedCosts
	     (emptyTyVarSet, emptyTyVarEnv)
	     (nullIdEnv, nullIdEnv)
	     nullConApps

	-- The top level "enclosing CC" is "SUBSUMED".  But the enclosing CC
	-- for the rhs of top level defs is "OST_CENTRE".  Consider
	--	f = \x -> e
	--	g = \y -> let v = f y in scc "x" (v ...)
	-- Here we want to inline "f", since its CC is SUBSUMED, but we don't
	-- want to inline "v" since its CC is dynamically determined.


getEnvs :: SimplEnv -> (SimplTypeEnv, SimplValEnv)
getEnvs (SimplEnv _ _ ty_env id_env _) = (ty_env, id_env)

setTyEnv :: SimplEnv -> SimplTypeEnv -> SimplEnv
setTyEnv (SimplEnv chkr encl_cc _ in_id_env con_apps) ty_env
  = SimplEnv chkr encl_cc ty_env in_id_env con_apps

setIdEnv :: SimplEnv -> SimplValEnv -> SimplEnv
setIdEnv (SimplEnv chkr encl_cc ty_env _ con_apps) id_env
  = SimplEnv chkr encl_cc ty_env id_env con_apps

setSubstEnvs :: SimplEnv -> TyVarEnv Type -> IdEnv SubstInfo -> SimplEnv
setSubstEnvs (SimplEnv chkr encl_cc (in_scope_tyvars, _) (in_scope_ids, _) con_apps)
	     ty_subst id_subst
  = SimplEnv chkr encl_cc (in_scope_tyvars, ty_subst) (in_scope_ids, id_subst) con_apps

zapSubstEnvs :: SimplEnv -> SimplEnv
zapSubstEnvs (SimplEnv chkr encl_cc (in_scope_tyvars, _) (in_scope_ids, _) con_apps)
  = SimplEnv chkr encl_cc (in_scope_tyvars, emptyTyVarEnv) (in_scope_ids, nullIdEnv) con_apps
\end{code}


%************************************************************************
%*									*
\subsubsection{Command-line switches}
%*									*
%************************************************************************

\begin{code}
getSwitchChecker :: SimplEnv -> SwitchChecker
getSwitchChecker (SimplEnv chkr _ _ _ _) = chkr

switchIsSet :: SimplEnv -> SimplifierSwitch -> Bool
switchIsSet (SimplEnv chkr _ _ _ _) switch
  = switchIsOn chkr switch

getSimplIntSwitch :: SwitchChecker -> (Int-> SimplifierSwitch) -> Int
getSimplIntSwitch chkr switch
  = expectJust "getSimplIntSwitch" (intSwitchSet chkr switch)

	-- Crude, but simple
setCaseScrutinee :: SimplEnv -> SimplEnv
setCaseScrutinee (SimplEnv chkr encl_cc ty_env id_env con_apps)
  = SimplEnv chkr' encl_cc ty_env id_env con_apps
  where
    chkr' SimplCaseScrutinee = SwBool True
    chkr' other		     = chkr other
\end{code}

@switchOffInlining@ is used to prepare the environment for simplifying
the RHS of an Id that's marked with an INLINE pragma.  It is going to
be inlined wherever they are used, and then all the inlining will take
effect.  Meanwhile, there isn't much point in doing anything to the
as-yet-un-INLINEd rhs.  Furthremore, it's very important to switch off
inlining!  because
	(a) not doing so will inline a worker straight back into its wrapper!

and 	(b) Consider the following example 
	     	let f = \pq -> BIG
	     	in
	     	let g = \y -> f y y
		    {-# INLINE g #-}
	     	in ...g...g...g...g...g...

	Now, if that's the ONLY occurrence of f, it will be inlined inside g,
	and thence copied multiple times when g is inlined.

	Andy disagrees! Example:
		all xs = foldr (&&) True xs
		any p = all . map p  {-# INLINE any #-}
	
	Problem: any won't get deforested, and so if it's exported and
	the importer doesn't use the inlining, (eg passes it as an arg)
	then we won't get deforestation at all.
	We havn't solved this problem yet!

We prepare the envt by simply modifying the id_env, which has
all the unfolding info. At one point we did it by modifying the chkr so
that it said "EssentialUnfoldingsOnly", but that prevented legitmate, and important,
simplifications happening in the body of the RHS.

\begin{code}
switchOffInlining :: SimplEnv -> SimplEnv
switchOffInlining (SimplEnv chkr encl_cc ty_env (in_scope_ids, id_subst) con_apps)
  = SimplEnv chkr encl_cc ty_env (mapUFM forget in_scope_ids, id_subst) nullConApps
  where
    forget (id, binder_info, rhs_info) = (id, noBinderInfo, NoUnfolding)
\end{code}


%************************************************************************
%*									*
\subsubsection{The ``enclosing cost-centre''}
%*									*
%************************************************************************

\begin{code}
setEnclosingCC :: SimplEnv -> CostCentre -> SimplEnv

setEnclosingCC env@(SimplEnv chkr _ ty_env id_env con_apps) encl_cc
  = SimplEnv chkr encl_cc ty_env id_env con_apps

getEnclosingCC :: SimplEnv -> CostCentre
getEnclosingCC (SimplEnv chkr encl_cc ty_env id_env con_apps) = encl_cc
\end{code}

%************************************************************************
%*									*
\subsubsection{The @TypeEnv@ part}
%*									*
%************************************************************************

These two "bind" functions extend the tyvar substitution.
They don't affect what tyvars are in scope.

\begin{code}
bindTyVar :: SimplEnv -> TyVar -> Type -> SimplEnv
bindTyVar (SimplEnv chkr encl_cc (tyvars, ty_subst) id_env con_apps) tyvar ty
  = SimplEnv chkr encl_cc (tyvars, new_ty_subst) id_env con_apps
  where
    new_ty_subst = addToTyVarEnv ty_subst tyvar ty

bindTyVars :: SimplEnv -> TyVarEnv Type -> SimplEnv
bindTyVars (SimplEnv chkr encl_cc (tyvars, ty_subst) id_env con_apps) extra_subst
  = SimplEnv chkr encl_cc (tyvars, new_ty_subst) id_env con_apps
  where
    new_ty_subst = ty_subst `plusTyVarEnv` extra_subst
\end{code}

\begin{code}
simplTy (SimplEnv _ _ (_, ty_subst) _ _) ty = returnEager (instantiateTy ty_subst ty)
\end{code}

%************************************************************************
%*									*
\subsubsection{The ``Id env'' part}
%*									*
%************************************************************************

notInScope forgets that the specified binder is in scope.
It is used when we decide to bind a let(rec) bound thing to
an atom, *after* the Id has been added to the in-scope mapping by simplBinder. 

\begin{code}
notInScope :: SimplEnv -> OutBinder -> SimplEnv
notInScope (SimplEnv chkr encl_cc ty_env (in_scope_ids, id_subst) con_apps) id
  = SimplEnv chkr encl_cc ty_env (new_in_scope_ids, id_subst) con_apps
  where
    new_in_scope_ids = delOneFromIdEnv in_scope_ids id
\end{code}

These "bind" functions extend the Id substitution.

\begin{code}
bindIdToAtom :: SimplEnv
	     -> InBinder
             -> OutArg 	-- Val args only, please
	     -> SimplEnv

bindIdToAtom (SimplEnv chkr encl_cc ty_env (in_scope_ids, id_subst) con_apps)
	     (in_id,occ_info) atom
  = SimplEnv chkr encl_cc ty_env id_env' con_apps
  where
    id_env' = case atom of
		LitArg lit -> (in_scope_ids, addOneToIdEnv id_subst in_id (SubstLit lit))
		VarArg id  -> (modifyOccInfo in_scope_ids (uniqueOf id) occ_info,
			       addOneToIdEnv id_subst in_id (SubstVar id))

bindIdToExpr :: SimplEnv
	     -> InBinder
             -> SimplifiableCoreExpr
	     -> SimplEnv

bindIdToExpr (SimplEnv chkr encl_cc ty_env@(_, ty_subst) (in_scope_ids, id_subst) con_apps)
	     (in_id,occ_info) expr
  = ASSERT( isOneFunOcc occ_info )	-- Binder occurs just once, safely, so no
					-- need to adjust occurrence info for RHS, 
					-- unlike bindIdToAtom
    SimplEnv chkr encl_cc ty_env (in_scope_ids, id_subst') con_apps
  where
    id_subst' = addOneToIdEnv id_subst in_id (SubstExpr ty_subst id_subst expr)
\end{code}


%************************************************************************
%*									*
\subsubsection{The @OutIdEnv@}
%*									*
%************************************************************************

\begin{code}
lookupIdSubst :: SimplEnv -> InId -> Maybe SubstInfo
lookupIdSubst (SimplEnv _ _ _ (_, id_subst) _) id = lookupIdEnv id_subst id

lookupOutIdEnv :: SimplEnv -> OutId -> Maybe (OutId, BinderInfo, Unfolding)
lookupOutIdEnv (SimplEnv _ _ _ (in_scope_ids, _) _) id = lookupIdEnv in_scope_ids id

lookupUnfolding :: SimplEnv -> OutId -> Unfolding
lookupUnfolding env id
  = case lookupOutIdEnv env id of
	Just (_,_,info) -> info
	Nothing		-> NoUnfolding

modifyOutEnvItem :: (OutId, BinderInfo, Unfolding)
	         -> (OutId, BinderInfo, Unfolding) 
	         -> (OutId, BinderInfo, Unfolding)
modifyOutEnvItem (id, occ, info1) (_, _, info2)
  = case (info1, info2) of
		(OtherLit ls1, OtherLit ls2) -> (id,occ, OtherLit (ls1++ls2))
		(OtherCon cs1, OtherCon cs2) -> (id,occ, OtherCon (cs1++cs2))
		(_,            NoUnfolding)  -> (id,occ, info1)
		other	       		     -> (id,occ, info2)
\end{code}


\begin{code}
isEvaluated :: Unfolding -> Bool
isEvaluated (OtherLit _) = True
isEvaluated (OtherCon _) = True
isEvaluated (CoreUnfolding ValueForm _ expr) = True
isEvaluated other = False
\end{code}



\begin{code}
mkSimplUnfoldingGuidance chkr out_id rhs
  = calcUnfoldingGuidance (getInlinePragma out_id) opt_UnfoldingCreationThreshold rhs

extendEnvGivenUnfolding :: SimplEnv -> OutId -> BinderInfo -> Unfolding -> SimplEnv
extendEnvGivenUnfolding env@(SimplEnv chkr encl_cc ty_env (in_scope_ids, id_subst) con_apps)
	              out_id occ_info rhs_info
  = SimplEnv chkr encl_cc ty_env (new_in_scope_ids, id_subst) con_apps
  where
    new_in_scope_ids = addToUFM_C modifyOutEnvItem in_scope_ids out_id 
				  (out_id, occ_info, rhs_info)
\end{code}


\begin{code}
modifyOccInfo in_scope_ids uniq new_occ
  = modifyIdEnv_Directly modify_fn in_scope_ids uniq
  where
    modify_fn (id,occ,rhs) = (id, orBinderInfo occ new_occ, rhs)

markDangerousOccs (SimplEnv chkr encl_cc ty_env (in_scope_ids, id_subst) con_apps) atoms
  = SimplEnv chkr encl_cc ty_env (new_in_scope_ids, id_subst) con_apps
  where
    new_in_scope_ids = foldl (modifyIdEnv modify_fn) in_scope_ids [v | VarArg v <- atoms]
    modify_fn (id,occ,rhs) = (id, noBinderInfo, rhs)
\end{code}


%************************************************************************
%*									*
\subsubsection{The @ConAppMap@ type}
%*									*
%************************************************************************

The @ConAppMap@ maps applications of constructors (to value atoms)
back to an association list that says "if the constructor was applied
to one of these lists-of-Types, then this OutId is your man (in a
non-gender-specific sense)".  I.e., this is a reversed mapping for
(part of) the main OutIdEnv

\begin{code}
type ConAppMap = FiniteMap UnfoldConApp [([Type], OutId)]

data UnfoldConApp
  = UCA		OutId			-- data constructor
		[OutArg]		-- *value* arguments; see use below
\end{code}

\begin{code}
nullConApps = emptyFM

extendConApps con_apps id (Con con args)
  = addToFM_C (\old new -> new++old) con_apps (UCA con val_args) [(ty_args,id)]
  where
    val_args = filter isValArg args		-- Literals and Ids
    ty_args  = [ty | TyArg ty <- args]		-- Just types

extendConApps con_apps id other_rhs = con_apps
\end{code}

\begin{code}
lookForConstructor env@(SimplEnv _ _ _ _ con_apps) (Con con args)
  | switchIsSet env SimplReuseCon
  = case lookupFM con_apps (UCA con val_args) of
	Nothing     -> Nothing

	Just assocs -> case [id | (tys, id) <- assocs, 
				  and (zipWith (==) tys ty_args)]
		       of
			  []     -> Nothing
			  (id:_) -> Just id
  where
    val_args = filter isValArg args		-- Literals and Ids
    ty_args  = [ty | TyArg ty <- args]		-- Just types

lookForConstructor env other = Nothing
\end{code}

NB: In @lookForConstructor@ we used (before Apr 94) to have a special case
for nullary constructors, but now we only do constructor re-use in
let-bindings the special case isn't necessary any more.

\begin{verbatim}	
  = 	-- Don't re-use nullary constructors; it's a waste.  Consider
	-- let
	-- 	  a = leInt#! p q
	-- in
	-- case a of
	--    True  -> ...
	--    False -> False
	--
	-- Here the False in the second case will get replace by "a", hardly
	-- a good idea
    Nothing
\end{verbatim}


The main thing about @UnfoldConApp@ is that it has @Ord@ defined on
it, so we can use it for a @FiniteMap@ key.

\begin{code}
instance Eq  UnfoldConApp where
    a == b = case (a `compare` b) of { EQ -> True;   _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False;  _ -> True  }

instance Ord UnfoldConApp where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = cmp_app a b

cmp_app (UCA c1 as1) (UCA c2 as2)
  = compare c1 c2 `thenCmp` cmpList cmp_arg as1 as2
  where
    -- ToDo: make an "instance Ord CoreArg"???

    cmp_arg (VarArg   x) (VarArg   y) = x `compare` y
    cmp_arg (LitArg   x) (LitArg   y) = x `compare` y
    cmp_arg (TyArg    x) (TyArg    y) = panic "SimplEnv.cmp_app:TyArgs"
    cmp_arg x y
      | tag x _LT_ tag y = LT
      | otherwise	 = GT
      where
	tag (VarArg   _) = ILIT(1)
	tag (LitArg   _) = ILIT(2)
	tag (TyArg    _) = panic# "SimplEnv.cmp_app:TyArg"
\end{code}


@extendUnfoldEnvGivenRhs@ records in the UnfoldEnv info about the RHS
of a new binding.  There is a horrid case we have to take care about,
due to Andr\'e Santos:
@
    type Array_type b   = Array Int b;
    type Descr_type     = (Int,Int);

    tabulate      :: (Int -> x) -> Descr_type -> Array_type x;
    tabulate      f (l,u)             = listArray (l,u) [f i | i <- [l..u]];

    f_iaamain a_xs=
	let {
	    f_aareorder::(Array_type Int) -> (Array_type t1) -> Array_type t1;
	    f_aareorder a_index a_ar=
		let {
		    f_aareorder' a_i= a_ar ! (a_index ! a_i)
		 } in  tabulate f_aareorder' (bounds a_ar);
	    r_index=tabulate ((+) 1) (1,1);
	    arr    = listArray (1,1) a_xs;
	    arg    = f_aareorder r_index arr
	 } in  elems arg
@
Now, when the RHS of arg gets simplified, we inline f_aareorder to get
@
	arg  = let f_aareorder' a_i = arr ! (r_index ! a_i)
	       in tabulate f_aareorder' (bounds arr)
@
Note that r_index is not inlined, because it was bound to a_index which
occurs inside a lambda.

Alas, if elems is inlined, so that (elems arg) becomes (case arg of ...),
then arg is inlined. IF WE USE THE NEW VERSION OF arg, and re-occurrence
analyse it, we won't spot the inside-lambda property of r_index, so r_index
will get inlined inside the lambda.  AARGH.

Solution: when we occurrence-analyse the new RHS we have to go back
and modify the info recorded in the UnfoldEnv for the free vars
of the RHS.  In the example we'd go back and record that r_index is now used
inside a lambda.

\begin{code}
extendEnvGivenNewRhs :: SimplEnv -> OutId -> OutExpr -> SimplEnv
extendEnvGivenNewRhs env out_id rhs
  = extendEnvGivenBinding env noBinderInfo out_id rhs

extendEnvGivenBinding :: SimplEnv -> BinderInfo -> OutId -> OutExpr -> SimplEnv
extendEnvGivenBinding env@(SimplEnv chkr encl_cc ty_env (in_scope_ids, id_subst) con_apps)
	              occ_info out_id rhs
  = SimplEnv chkr encl_cc ty_env (new_in_scope_ids, id_subst) new_con_apps 
  where
    new_in_scope_ids | okToInline (whnfOrBottom form) 
				  (couldBeSmallEnoughToInline out_id guidance) 
				  occ_info 
		     = env_with_unfolding
		     | otherwise
		     = in_scope_ids
	-- Don't bother to munge the OutIdEnv unless there is some possibility
	-- that the thing might be inlined.  We check this by calling okToInline suitably.

    new_con_apps = _scc_ "eegnr.conapps" 
		   extendConApps con_apps out_id rhs

	-- Modify the occ info for rhs's interesting free variables.
	-- That's to take account of:
	--		let a = \x -> BIG in
	--		let b = \f -> f a
	--		in ...b...b...b...
	-- Here "a" occurs exactly once. "b" simplifies to a small value.
	-- So "b" will be inlined at each call site, and there's a good chance
	-- that "a" will too.  So we'd better modify "a"s occurrence info to
	-- record the fact that it can now occur many times by virtue that "b" can.
    env_with_unfolding = _scc_ "eegnr.modify_occ" 
			 foldl zap env1 (ufmToList fv_occ_info)
    zap env (uniq,_)   = modifyOccInfo env uniq occ_info


	-- Add an unfolding and rhs_info for the new Id.
	-- If the out_id is already in the OutIdEnv (which should be the
	-- case because it was put there by simplBinder)
	-- then just replace the unfolding, leaving occurrence info alone.
    env1 		      =	_scc_ "eegnr.modify_out" 
				addToUFM_C modifyOutEnvItem in_scope_ids out_id 
					   (out_id, occ_info, rhs_info)

	-- Occurrence-analyse the RHS
	-- The "interesting" free variables we want occurrence info for are those
	-- in the OutIdEnv that have only a single occurrence right now.
    (fv_occ_info, template) = _scc_ "eegnr.occ-anal" 
			      occurAnalyseExpr is_interesting rhs_w_cc

    is_interesting v        = _scc_ "eegnr.mkidset" 
			      case lookupIdEnv in_scope_ids v of
				Just (_, occ, _) -> isOneOcc occ
				other	         -> False

	-- Compute unfolding details
    rhs_info = CoreUnfolding form guidance template
    form     = _scc_ "eegnr.form_sum" 
	       mkFormSummary rhs
    guidance = _scc_ "eegnr.guidance" 
	       mkSimplUnfoldingGuidance chkr out_id rhs

	-- Attach a cost centre to the RHS if necessary
    rhs_w_cc  | currentOrSubsumedCosts encl_cc
	      || not (noCostCentreAttached (coreExprCc rhs))
	      = rhs
	      | otherwise
	      = SCC encl_cc rhs
\end{code}
