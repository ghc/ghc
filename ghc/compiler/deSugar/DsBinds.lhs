%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[DsBinds]{Pattern-matching bindings (Binds and MonoBinds)}

Handles @Binds@; those at the top level require different handling, in
that the @Rec@/@NonRec@/etc structure is thrown away (whereas at lower
levels it is preserved with @let@/@letrec@s).

\begin{code}
#include "HsVersions.h"

module DsBinds (
	dsBinds, dsInstBinds
    ) where

IMPORT_Trace		-- ToDo: rm (debugging only)

import AbsSyn		-- the stuff being desugared
import PlainCore	-- the output of desugaring;
			-- importing this module also gets all the
			-- CoreSyn utility functions
import DsMonad		-- the monadery used in the desugarer

import AbsUniType
import CmdLineOpts	( GlobalSwitch(..), SwitchResult, switchIsOn )
import CostCentre	( mkAllDictsCC, preludeDictsCostCentre )
import Inst		( getInstUniType )
import DsExpr		( dsExpr )
import DsGRHSs		( dsGuarded )
import DsUtils
import Id		( getIdUniType, mkInstId, Inst, Id, DictVar(..) )
import Match		( matchWrapper )
import Maybes		( Maybe(..),assocMaybe )
import Outputable
import Pretty
import Util
import ListSetOps	( minusList, intersectLists )
\end{code}


%************************************************************************
%*									*
\subsection[toplevel-and-regular-DsBinds]{Regular and top-level @dsBinds@}
%*									*
%************************************************************************

Like @dsBinds@, @dsBind@ returns a @[PlainCoreBinding]@, but it may be
that some of the binders are of unboxed type.  This is sorted out when
the caller wraps the bindings round an expression.

\begin{code}
dsBinds :: TypecheckedBinds -> DsM [PlainCoreBinding]
\end{code}

All ``real'' bindings are expressed in terms of the
@AbsBinds@ construct, which is a massively-complicated ``shorthand'',
and its desugaring is the subject of section~9.1 in the static
semantics paper.

(ToDo)	For:
\begin{verbatim}
AbsBinds [a1, ... ,aj]	-- type variables
	 [d1, ... ,dk]	-- dict variables
	 [(l1,g1), ..., (lm,gm)]	-- overloaded equivs [Id pairs] (later...)
	 [db1=..., ..., dbn=...]	-- dict binds
	 [vb1=..., ..., vbm=...]	-- val binds; note: vb_i = l_i
\end{verbatim}
we want to make, in the general case (non-Fozzie translation):
\begin{verbatim}
   -- tupler-upper:
   tup a1...aj d1...dk =
      let <dict-binds>	   in
      let(rec) <val-binds> in (vb1,...,vbm)    -- NB: == ... in (l1,...,lm)

   -- a bunch of selectors:
   g1 a1...aj d1...dk = case (_tup a1...aj d1...dk) of (x1,x2,...,xm) -> x1
   ...
   gm a1...aj d1...dk = case (_tup a1...aj d1...dk) of (x1,x2,...,xm) -> xm
\end{verbatim}
But there are lots of special cases.


%==============================================
\subsubsection{Structure cases}
%==============================================

\begin{code}
dsBinds (BindWith _ _)		= panic "dsBinds:BindWith"
dsBinds EmptyBinds		= returnDs []
dsBinds (SingleBind bind)	= dsBind [] [] id [] bind

dsBinds (ThenBinds  binds_1 binds_2)
  = andDs (++) (dsBinds binds_1) (dsBinds binds_2)
\end{code}


%==============================================
\subsubsection{AbsBind case: no overloading}
%==============================================

Special case: no overloading.  
\begin{verbatim}
	x1 = e1
	x2 = e2
\end{verbatim}
We abstract each wrt the type variables, giving
\begin{verbatim}
	x1' = /\tyvars -> e1[x1' tyvars/x1, x2' tyvars/x2]
	x2' = /\tyvars -> e2[x1' tyvars/x1, x2' tyvars/x2]
\end{verbatim}
There are some complications.  

(i) The @val_binds@ might mention variable not in @local_global_prs@.
In this case we need to make up new polymorphic versions of them.

(ii) Exactly the same applies to any @inst_binds@ which may be
present.  However, here we expect that mostly they will be simple constant
definitions, which don't mention the type variables at all, so making them
polymorphic is really overkill.  @dsInstBinds@ deals with this case.

\begin{code}
dsBinds (AbsBinds tyvars [] local_global_prs inst_binds val_binds)
  = mapDs mk_poly_private_binder private_binders
					`thenDs` \ poly_private_binders ->
    let
	full_local_global_prs = (private_binders `zip` poly_private_binders) 
				++ local_global_prs
    in
    listDs [ mkSatTyApp global tyvar_tys `thenDs` \ app ->
	     returnDs (local, app)
	   | (local,global) <- full_local_global_prs
	   ]				 `thenDs` \ env ->

--    pprTrace "AbsBinds1:" (ppr PprDebug env) $

    extendEnvDs env (

    dsInstBinds tyvars inst_binds	`thenDs` \ (inst_bind_pairs, inst_env) ->
    extendEnvDs inst_env			 (

    dsBind tyvars [] (lookupId full_local_global_prs) inst_bind_pairs val_binds
    ))
  where
	-- "private_binders" is the list of binders in val_binds
	-- which don't appear in the local_global_prs list
	-- These only really show up in stuff produced from compiling
	-- class and instance declarations.
	-- We need to add suitable polymorphic versions of them to the
	-- local_global_prs.
    private_binders = binders `minusList` [local | (local,_) <- local_global_prs]
    binders	    = collectTypedBinders val_binds
    mk_poly_private_binder id = newSysLocalDs (snd (quantifyTy tyvars (getIdUniType id)))

    tyvar_tys = map mkTyVarTy tyvars
\end{code}


%==============================================
\subsubsection{AbsBind case: overloading}
%==============================================

If there is overloading we go for the general case.

We want the global identifiers to be abstracted wrt all types and
dictionaries; and the local identifiers wrt the non-overloaded types.
That is, we try to avoid global scoping of type abstraction. Example

	f :: Eq a => a -> [(a,b)] -> b
	f = ...f...

Here, f is fully polymorphic in b.  So we generate

	f ab d = let	...dict defns...
		 in
		 letrec f' b = ...(f' b)...
		 in f' b

*Notice* that we don't clone type variables, and *do* make use of 
shadowing.  It is possible to do cloning, but it makes the code quite
a bit more complicated, and the simplifier will clone it all anyway.

Why bother with this gloss?  Because it makes it more likely that
the defn of f' can get floated out, notably if f gets specialised
to a particular type for a.

\begin{code}
dsBinds (AbsBinds all_tyvars dicts local_global_prs dict_binds val_binds)
  = 	-- If there is any non-overloaded polymorphism, make new locals with
	-- appropriate polymorphism
    (if null non_overloaded_tyvars 
     then
	-- No non-overloaded polymorphism, so stay with current envt
	returnDs (id, [], [])
     else
	-- Some local, non-overloaded polymorphism
	cloneTyVarsDs non_overloaded_tyvars	`thenDs` \ local_tyvars ->

	mapDs mk_binder binders			`thenDs` \ new_binders ->
	let
	    old_new_pairs   = binders `zip` new_binders
        in

	listDs	[ mkSatTyApp new non_ov_tyvar_tys `thenDs` \ app ->
		  returnDs (old, app)
		| (old,new) <- old_new_pairs
		]					`thenDs` \ extra_env ->
	let
	  local_binds = [CoNonRec old app | (old,app) <- extra_env, old `is_elem` locals]
	  is_elem     = isIn "dsBinds"
	in
	returnDs (lookupId old_new_pairs, extra_env, local_binds)
    )
		`thenDs` \ (binder_subst_fn, local_env, local_binds) ->
	
--    pprTrace "AbsBinds:all:" (ppAbove (ppr PprDebug local_binds) (ppr PprDebug local_env)) $

    extendEnvDs local_env (
 
      dsInstBinds non_overloaded_tyvars dict_binds	`thenDs` \ (inst_bind_pairs, inst_env) ->

      extendEnvDs inst_env		 (

	dsBind non_overloaded_tyvars [] binder_subst_fn inst_bind_pairs val_binds	
    ))							`thenDs` \ core_binds ->

    let
	tuple_rhs = mkCoLetsAny core_binds  (
		    mkCoLetsAny local_binds (
		    mkTupleExpr locals   ))
    in
    mkTupleBind all_tyvars dicts local_global_prs tuple_rhs  `thenDs` \ core_bind_prs ->

    returnDs [ CoNonRec binder rhs | (binder,rhs) <- core_bind_prs ]
  where
    locals = [local | (local,global) <- local_global_prs]
    non_ov_tyvar_tys = map mkTyVarTy non_overloaded_tyvars

    overloaded_tyvars     = extractTyVarsFromTys (map getIdUniType dicts)
    non_overloaded_tyvars = all_tyvars `minusList` overloaded_tyvars

    binders      = collectTypedBinders val_binds
    mk_binder id = newSysLocalDs (snd (quantifyTy non_overloaded_tyvars (getIdUniType id)))
\end{code}

@mkSatTyApp id tys@ constructs an expression whose value is (id tys).
However, sometimes id takes more type args than are in tys, and the
specialiser hates that, so we have to eta expand, to
(/\ a b -> id tys a b)

\begin{code}
mkSatTyApp :: Id 		-- Id to apply to the types
	   -> [UniType]		-- Types to apply it to
	   -> DsM PlainCoreExpr

mkSatTyApp id [] = returnDs (CoVar id)

mkSatTyApp id tys
  | null tyvar_templates 
  = returnDs (mkCoTyApps (CoVar id) tys)	-- Common case

  | otherwise
  = newTyVarsDs (drop (length tys) tyvar_templates)	`thenDs` \ tyvars ->
--  pprTrace "mkSatTyApp:" (ppCat [ppr PprDebug id, ppr PprDebug tyvar_templates, ppr PprDebug tyvars, ppr PprDebug theta, ppr PprDebug tau_ty, ppr PprDebug tys]) $
    returnDs (mkCoTyLam tyvars (mkCoTyApps (mkCoTyApps (CoVar id) tys) 
					   (map mkTyVarTy tyvars)))
  where
    (tyvar_templates, theta, tau_ty) = splitType (getIdUniType id)
\end{code}

There are several places where we encounter ``inst binds,'' 
@(Inst, TypecheckedExpr)@ pairs.  Many of these are ``trivial'' binds
(a var to a var or literal), which we want to substitute away; so we
return both some desugared bindings {\em and} a substitution
environment for the subbed-away ones.

These dictionary bindings are non-recursive, and ordered, so that
later ones may mention earlier ones, but not vice versa.

\begin{code}
dsInstBinds :: [TyVar]				-- Abstract wrt these
	    -> [(Inst, TypecheckedExpr)]	-- From AbsBinds
	    -> DsM ([(Id,PlainCoreExpr)], 	-- Non-trivial bindings
		    [(Id,PlainCoreExpr)])	-- Trivial ones to be substituted away

do_nothing = ([], []) -- out here to avoid dsInstBinds CAF (sigh)
prel_dicts_cc = preludeDictsCostCentre False{-not dupd-} -- ditto

dsInstBinds tyvars []
  = returnDs do_nothing

dsInstBinds tyvars ((inst, expr@(Var _)) : bs)
  = dsExpr expr				`thenDs` ( \ rhs ->
    let	-- Need to apply dsExpr to the variable in case it 
	-- has a substitution in the current environment
	subst_item = (mkInstId inst, rhs)
    in
    extendEnvDs [subst_item] (
	dsInstBinds tyvars bs 	
    )					`thenDs` (\ (binds, subst_env) ->
    returnDs (binds, subst_item : subst_env)
    ))

dsInstBinds tyvars ((inst, expr@(Lit _)) : bs)
  = dsExpr expr				`thenDs` ( \ core_lit ->
    let
	subst_item = (mkInstId inst, core_lit)
    in
    extendEnvDs [subst_item]	 (
	dsInstBinds tyvars bs
    )				 	`thenDs` (\ (binds, subst_env) ->
    returnDs (binds, subst_item : subst_env)
    ))

dsInstBinds tyvars ((inst, expr) : bs)
  | null abs_tyvars
  = dsExpr expr			`thenDs` \ core_expr ->
    ds_dict_cc core_expr	`thenDs` \ dict_expr ->
    dsInstBinds tyvars bs	`thenDs` \ (core_rest, subst_env) ->
    returnDs ((mkInstId inst, dict_expr) : core_rest, subst_env)
	
  | otherwise
  =	-- Obscure case.  
	-- The inst mentions the type vars wrt which we are abstracting,
	-- so we have to invent a new polymorphic version, and substitute
	-- appropriately.
	-- This can occur in, for example: 
	--	leftPoll :: [FeedBack a] -> FeedBack a
	--	leftPoll xs = take poll xs
	-- Here there is an instance of take at the type of elts of xs,
	-- as well as the type of poll.  

    dsExpr expr			`thenDs` \ core_expr ->
    ds_dict_cc core_expr	`thenDs` \ dict_expr ->
    newSysLocalDs poly_inst_ty	`thenDs` \ poly_inst_id ->
    let
	subst_item = (mkInstId inst, mkCoTyApps (CoVar poly_inst_id) abs_tys)
    in
    extendEnvDs [subst_item] (
	dsInstBinds tyvars bs	
    )				`thenDs` \ (core_rest, subst_env) ->
    returnDs ((poly_inst_id, mkCoTyLam abs_tyvars dict_expr) : core_rest, 
	      subst_item : subst_env)
  where
    inst_ty    = getInstUniType inst
    abs_tyvars = extractTyVarsFromTy inst_ty `intersectLists` tyvars
    abs_tys    = map mkTyVarTy abs_tyvars
    (_, poly_inst_ty) = quantifyTy abs_tyvars inst_ty

    ------------------------
    -- Wrap a desugared expression in `_scc_ "DICT" <expr>' if
    -- appropriate.  Uses "inst"'s type.

    ds_dict_cc expr
      = -- if profiling, wrap the dict in "_scc_ DICT <dict>":
	getSwitchCheckerDs	`thenDs` \ sw_chkr ->
	let
	    doing_profiling   = sw_chkr SccProfilingOn
	    compiling_prelude = sw_chkr CompilingPrelude
	in
	if not doing_profiling
	|| not (isDictTy inst_ty) then -- that's easy: do nothing
	    returnDs expr
	else if compiling_prelude then
	    returnDs (CoSCC prel_dicts_cc expr)
	else
	    getModuleAndGroupDs 	`thenDs` \ (mod_name, grp_name) ->
	    -- ToDo: do -dicts-all flag (mark dict things
	    -- with individual CCs)
	    let
		dict_cc = mkAllDictsCC mod_name grp_name False{-not dupd-}
	    in
	    returnDs (CoSCC dict_cc expr)
\end{code}

%************************************************************************
%*									*
\subsection[dsBind]{Desugaring a @Bind@}
%*									*
%************************************************************************

Like @dsBinds@, @dsBind@ returns a @[PlainCoreBinding]@, but it may be that
some of the binders are of unboxed type.  

For an explanation of the first three args, see @dsMonoBinds@.

\begin{code}
dsBind	:: [TyVar] -> [DictVar]		-- Abstract wrt these
	-> (Id -> Id)			-- Binder substitution
	-> [(Id,PlainCoreExpr)]		-- Inst bindings already dealt with
	-> TypecheckedBind 
	-> DsM [PlainCoreBinding]

dsBind tyvars dicts binder_subst inst_bind_pairs EmptyBind 
  = returnDs [CoNonRec binder rhs | (binder,rhs) <- inst_bind_pairs]

dsBind tyvars dicts binder_subst inst_bind_pairs (NonRecBind monobinds)
  = dsMonoBinds False tyvars dicts binder_subst monobinds   `thenDs` ( \ val_bind_pairs ->
    returnDs [CoNonRec binder rhs | (binder,rhs) <- inst_bind_pairs ++ val_bind_pairs] )

dsBind tyvars dicts binder_subst inst_bind_pairs (RecBind monobinds)
  = dsMonoBinds True tyvars dicts binder_subst monobinds   `thenDs` ( \ val_bind_pairs ->
    returnDs [CoRec (inst_bind_pairs ++ val_bind_pairs)] )
\end{code}


%************************************************************************
%*									*
\subsection[dsMonoBinds]{Desugaring a @MonoBinds@}
%*									*
%************************************************************************

@dsMonoBinds@ transforms @TypecheckedMonoBinds@ into @PlainCoreBinds@.
In addition to desugaring pattern matching, @dsMonoBinds@ takes
a list of type variables and dicts, and adds abstractions for these
to the front of every binding.	That requires that the 
binders be altered too (their type has changed, 
so @dsMonoBinds@ also takes a function which maps binders into binders.
This mapping gives the binder the correct new type.

Remember, there's also a substitution in the monad which maps occurrences
of these binders into applications of the new binder to suitable type variables
and dictionaries.

\begin{code}
dsMonoBinds :: Bool			-- True <=> recursive binding group
	    -> [TyVar] -> [DictVar]	-- Abstract wrt these
	    -> (Id -> Id)		-- Binder substitution
	    -> TypecheckedMonoBinds
	    -> DsM [(Id,PlainCoreExpr)]
\end{code}



%==============================================
\subsubsection{Structure cases}
%==============================================

\begin{code}
dsMonoBinds is_rec tyvars dicts binder_subst EmptyMonoBinds = returnDs []

dsMonoBinds is_rec tyvars dicts binder_subst (AndMonoBinds  binds_1 binds_2)
  = andDs (++) (dsMonoBinds is_rec tyvars dicts binder_subst binds_1)
	       (dsMonoBinds is_rec tyvars dicts binder_subst binds_2)
\end{code}


%==============================================
\subsubsection{Simple base cases: function and variable bindings}
%==============================================

For the simplest bindings, we just heave them in the substitution env:

\begin{code}
{-	THESE TWO ARE PLAIN WRONG.
	The extendEnvDs only scopes over the nested call!
	Let the simplifier do this.

dsMonoBinds is_rec tyvars dicts binder_subst (VarMonoBind was_var (Var new_var))
  | not (is_rec || isExported was_var)
  = extendEnvDs [(was_var, CoVar new_var)] (
    returnDs [] )

dsMonoBinds is_rec tyvars dicts binder_subst (VarMonoBind was_var expr@(Lit _))
  | not (isExported was_var)
  = dsExpr expr			`thenDs` ( \ core_lit ->
    extendEnvDs [(was_var, core_lit)]	 (
    returnDs [] ))
-}

dsMonoBinds is_rec tyvars dicts binder_subst (VarMonoBind var expr)
  = dsExpr expr		`thenDs` ( \ core_expr ->
    returnDs [(binder_subst var, mkCoTyLam tyvars (mkCoLam dicts core_expr))] )
\end{code}

\begin{code}
dsMonoBinds is_rec tyvars dicts binder_subst (FunMonoBind fun matches locn)
  = putSrcLocDs locn				 (
    let
	new_fun = binder_subst fun
    in
    matchWrapper (FunMatch fun) matches (error_msg new_fun) `thenDs` \ (args, body) ->
    returnDs [(new_fun,
	       mkCoTyLam tyvars (mkCoLam dicts (mkCoLam args body)))]
    )
  where
    error_msg fun = "%F" -- "incomplete pattern(s) to match in function \""
		++ (escErrorMsg (ppShow 80 (ppr PprForUser fun))) ++ "\""

dsMonoBinds is_rec tyvars dicts binder_subst (PatMonoBind (VarPat v) grhss_and_binds locn)
  = putSrcLocDs locn			    (
    dsGuarded grhss_and_binds locn `thenDs` \ body_expr ->
    returnDs [(binder_subst v, mkCoTyLam tyvars (mkCoLam dicts body_expr))]
    )
\end{code}

%==============================================
\subsubsection{The general base case}
%==============================================

Now the general case of a pattern binding.  The monomorphism restriction
should ensure that if there is a non-simple pattern binding in the
group, then there is no overloading involved, so the dictionaries should
be empty.  (Simple pattern bindings were handled above.)
First, the paranoia check.

\begin{code}
dsMonoBinds is_rec tyvars (_:_) binder_subst (PatMonoBind pat grhss_and_binds locn)
  = panic "Non-empty dict list in for pattern binding"
\end{code}

We handle three cases for the binding
	pat = rhs

\begin{description}
\item[pat has no binders.]  
Then all this is dead code and we return an empty binding.

\item[pat has exactly one binder, v.]  
Then we can transform to:
\begin{verbatim}
	v' = /\ tyvars -> case rhs of { pat -> v }
\end{verbatim}
where \tr{v'} is gotten by looking up \tr{v} in the \tr{binder_subst}.

\item[pat has more than one binder.]
Then we transform to:
\begin{verbatim}
	t  = /\ tyvars -> case rhs of { pat -> (v1, ..., vn) }

	vi = /\ tyvars -> case (t tyvars) of { (v1, ..., vn) -> vi }
\end{verbatim}
\end{description}
  
\begin{code}
dsMonoBinds is_rec tyvars [] binder_subst (PatMonoBind pat grhss_and_binds locn)
  = putSrcLocDs locn (

    dsGuarded grhss_and_binds locn `thenDs` \ body_expr ->

{- KILLED by Sansom. 95/05
	-- make *sure* there are no primitive types in the pattern
    if any_con_w_prim_arg pat then
	error ( "ERROR: Pattern-bindings cannot involve unboxed/primitive types!\n\t"
	     ++ (ppShow 80 (ppr PprForUser pat)) ++ "\n"
	     ++ "(We apologise for not reporting this more `cleanly')\n" )

	-- Check whether the pattern already is a simple tuple; if so,
	-- we can just use the rhs directly
    else
-}
    mkSelectorBinds tyvars pat 	      
	[(binder, binder_subst binder) | binder <- pat_binders]
	body_expr
    )
  where
    pat_binders = collectTypedPatBinders pat
	-- NB For a simple tuple pattern, these binders 
	-- will appear in the right order!

{- UNUSED, post-Sansom:
    any_con_w_prim_arg :: TypecheckedPat -> Bool

    any_con_w_prim_arg (WildPat ty)	= isPrimType ty
    any_con_w_prim_arg (VarPat v)	= isPrimType (getIdUniType v)
    any_con_w_prim_arg (LazyPat pat)	= any_con_w_prim_arg pat
    any_con_w_prim_arg (AsPat _ pat)	= any_con_w_prim_arg pat
    any_con_w_prim_arg p@(ConPat _ _ args)  = any any_con_w_prim_arg args
    any_con_w_prim_arg (ConOpPat a1 _ a2 _) = any any_con_w_prim_arg [a1,a2]
    any_con_w_prim_arg (ListPat _ args)	    = any any_con_w_prim_arg args
    any_con_w_prim_arg (TuplePat  args)	    = any any_con_w_prim_arg args
    any_con_w_prim_arg (LitPat _ ty)	    = isPrimType ty
    any_con_w_prim_arg (NPat	  _ _ _)	= False -- be more paranoid?
    any_con_w_prim_arg (NPlusKPat _ _ _ _ _ _)	= False -- ditto

#ifdef DPH
    -- Should be more efficient to find type of pid than pats 
    any_con_w_prim_arg (ProcessorPat pats _ pat) 
       = error "any_con_w_prim_arg:ProcessorPat (DPH)"
#endif {- Data Parallel Haskell -}
-}

{-	OLD ... removed 6 Feb 95

    -- we allow it if the constructor has *only one*
    -- argument and that is unboxed, as in
    --
    --	let (I# i#) = ... in ...
    --
    prim_args args
      = let
	    no_of_prim_args
	      = length [ a | a <- args, isPrimType (typeOfPat a) ]
        in
	if no_of_prim_args == 0 then
	    False
	else if no_of_prim_args == 1 && length args == 1 then
	    False -- special case we let through
	else
	    True

-}
\end{code}

Wild-card patterns could be made acceptable here, but it involves some
extra work to benefit only rather unusual constructs like
\begin{verbatim}
	let (_,a,b) = ... in ...
\end{verbatim}
Better to extend the whole thing for any irrefutable constructor, at least.


