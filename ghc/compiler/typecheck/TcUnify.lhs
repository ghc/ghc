%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Unify]{Unifier}

The unifier is now squarely in the typechecker monad (because of the
updatable substitution).

\begin{code}
module TcUnify ( unifyTauTy, unifyTauTyList, unifyTauTyLists, 
	         unifyFunTy, unifyListTy, unifyTupleTy, unifyUnboxedTupleTy,
	 	 unifyKind, unifyKinds, unifyTypeKind
 ) where

#include "HsVersions.h"

-- friends: 
import TcMonad
import Type	( Type(..), tyVarsOfType, funTyCon,
		  mkFunTy, splitFunTy_maybe, splitTyConApp_maybe,
		  Kind, boxedTypeKind, typeCon, anyBoxCon, anyBoxKind,
		  splitAppTy_maybe,
	   	  tidyOpenType, tidyOpenTypes, tidyTyVar
		)
import TyCon	( TyCon, isTupleTyCon, isUnboxedTupleTyCon, 
		  tyConArity )
import Name	( hasBetterProv )
import Var	( TyVar, tyVarKind, varName, isSigTyVar )
import VarEnv	
import VarSet	( varSetElems )
import TcType	( TcType, TcTauType, TcTyVar, TcKind, 
		  newTyVarTy, newOpenTypeKind, newTyVarTy_OpenKind,
		  tcGetTyVar, tcPutTyVar, zonkTcType, tcTypeKind
		)
-- others:
import BasicTypes ( Arity )
import TysWiredIn ( listTyCon, mkListTy, mkTupleTy, mkUnboxedTupleTy )
import PprType	()		-- Instances
import Util
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{The Kind variants}
%*									*
%************************************************************************

\begin{code}
unifyKind :: TcKind		    -- Expected
	  -> TcKind		    -- Actual
	  -> TcM s ()
unifyKind k1 k2 
  = tcAddErrCtxtM (unifyCtxt "kind" k1 k2) $
    uTys k1 k1 k2 k2

unifyKinds :: [TcKind] -> [TcKind] -> TcM s ()
unifyKinds []       []       = returnTc ()
unifyKinds (k1:ks1) (k2:ks2) = unifyKind k1 k2 	`thenTc_`
			       unifyKinds ks1 ks2
unifyKinds _ _ = panic "unifyKinds: length mis-match"
\end{code}


%************************************************************************
%*									*
\subsection[Unify-exported]{Exported unification functions}
%*									*
%************************************************************************

The exported functions are all defined as versions of some
non-exported generic functions.

Unify two @TauType@s.  Dead straightforward.

\begin{code}
unifyTauTy :: TcTauType -> TcTauType -> TcM s ()
unifyTauTy ty1 ty2 	-- ty1 expected, ty2 inferred
  = tcAddErrCtxtM (unifyCtxt "type" ty1 ty2) $
    uTys ty1 ty1 ty2 ty2
\end{code}

@unifyTauTyList@ unifies corresponding elements of two lists of
@TauType@s.  It uses @uTys@ to do the real work.  The lists should be
of equal length.  We charge down the list explicitly so that we can
complain if their lengths differ.

\begin{code}
unifyTauTyLists :: [TcTauType] -> [TcTauType] ->  TcM s ()
unifyTauTyLists [] 	     []	        = returnTc ()
unifyTauTyLists (ty1:tys1) (ty2:tys2) = uTys ty1 ty1 ty2 ty2   `thenTc_`
					unifyTauTyLists tys1 tys2
unifyTauTyLists ty1s ty2s = panic "Unify.unifyTauTyLists: mismatched type lists!"
\end{code}

@unifyTauTyList@ takes a single list of @TauType@s and unifies them
all together.  It is used, for example, when typechecking explicit
lists, when all the elts should be of the same type.

\begin{code}
unifyTauTyList :: [TcTauType] -> TcM s ()
unifyTauTyList []		 = returnTc ()
unifyTauTyList [ty]		 = returnTc ()
unifyTauTyList (ty1:tys@(ty2:_)) = unifyTauTy ty1 ty2	`thenTc_`
				   unifyTauTyList tys
\end{code}

%************************************************************************
%*									*
\subsection[Unify-uTys]{@uTys@: getting down to business}
%*									*
%************************************************************************

@uTys@ is the heart of the unifier.  Each arg happens twice, because
we want to report errors in terms of synomyms if poss.  The first of
the pair is used in error messages only; it is always the same as the
second, except that if the first is a synonym then the second may be a
de-synonym'd version.  This way we get better error messages.

We call the first one \tr{ps_ty1}, \tr{ps_ty2} for ``possible synomym''.

\begin{code}
uTys :: TcTauType -> TcTauType	-- Error reporting ty1 and real ty1
     -> TcTauType -> TcTauType	-- Error reporting ty2 and real ty2
     -> TcM s ()

	-- Always expand synonyms (see notes at end)
uTys ps_ty1 (NoteTy _ ty1) ps_ty2 ty2 = uTys ps_ty1 ty1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (NoteTy _ ty2) = uTys ps_ty1 ty1 ps_ty2 ty2

	-- Variables; go for uVar
uTys ps_ty1 (TyVarTy tyvar1) ps_ty2 ty2 = uVar False tyvar1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (TyVarTy tyvar2) = uVar True  tyvar2 ps_ty1 ty1
					-- "True" means args swapped

	-- Functions; just check the two parts
uTys _ (FunTy fun1 arg1) _ (FunTy fun2 arg2)
  = uTys fun1 fun1 fun2 fun2	`thenTc_`    uTys arg1 arg1 arg2 arg2

	-- Type constructors must match
uTys ps_ty1 (TyConApp con1 tys1) ps_ty2 (TyConApp con2 tys2)
  = checkTcM (cons_match && length tys1 == length tys2) 
	     (unifyMisMatch ps_ty1 ps_ty2)			`thenTc_`
    unifyTauTyLists tys1 tys2
  where
	-- The AnyBox wild card matches anything
    cons_match =  con1 == con2 
	       || con1 == anyBoxCon
	       || con2 == anyBoxCon

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
uTys ps_ty1 (AppTy s1 t1) ps_ty2 ty2
  = case splitAppTy_maybe ty2 of
	Just (s2,t2) -> uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2
	Nothing      -> unifyMisMatch ps_ty1 ps_ty2

	-- Now the same, but the other way round
	-- Don't swap the types, because the error messages get worse
uTys ps_ty1 ty1 ps_ty2 (AppTy s2 t2)
  = case splitAppTy_maybe ty1 of
	Just (s1,t1) -> uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2
	Nothing      -> unifyMisMatch ps_ty1 ps_ty2

	-- Not expecting for-alls in unification
	-- ... but the error message from the unifyMisMatch more informative
	-- than a panic message!

	-- Anything else fails
uTys ps_ty1 ty1 ps_ty2 ty2  = unifyMisMatch ps_ty1 ps_ty2
\end{code}

Notes on synonyms
~~~~~~~~~~~~~~~~~
If you are tempted to make a short cut on synonyms, as in this
pseudocode...

\begin{verbatim}
uTys (SynTy con1 args1 ty1) (SynTy con2 args2 ty2)
  = if (con1 == con2) then
	-- Good news!  Same synonym constructors, so we can shortcut
	-- by unifying their arguments and ignoring their expansions.
	unifyTauTypeLists args1 args2
    else
	-- Never mind.  Just expand them and try again
	uTys ty1 ty2
\end{verbatim}

then THINK AGAIN.  Here is the whole story, as detected and reported
by Chris Okasaki \tr{<Chris_Okasaki@loch.mess.cs.cmu.edu>}:
\begin{quotation}
Here's a test program that should detect the problem:

\begin{verbatim}
	type Bogus a = Int
	x = (1 :: Bogus Char) :: Bogus Bool
\end{verbatim}

The problem with [the attempted shortcut code] is that
\begin{verbatim}
	con1 == con2
\end{verbatim}
is not a sufficient condition to be able to use the shortcut!
You also need to know that the type synonym actually USES all
its arguments.  For example, consider the following type synonym
which does not use all its arguments.
\begin{verbatim}
	type Bogus a = Int
\end{verbatim}

If you ever tried unifying, say, \tr{Bogus Char} with \tr{Bogus Bool},
the unifier would blithely try to unify \tr{Char} with \tr{Bool} and
would fail, even though the expanded forms (both \tr{Int}) should
match.

Similarly, unifying \tr{Bogus Char} with \tr{Bogus t} would
unnecessarily bind \tr{t} to \tr{Char}.

... You could explicitly test for the problem synonyms and mark them
somehow as needing expansion, perhaps also issuing a warning to the
user.
\end{quotation}


%************************************************************************
%*									*
\subsection[Unify-uVar]{@uVar@: unifying with a type variable}
%*									*
%************************************************************************

@uVar@ is called when at least one of the types being unified is a
variable.  It does {\em not} assume that the variable is a fixed point
of the substitution; rather, notice that @uVar@ (defined below) nips
back into @uTys@ if it turns out that the variable is already bound.

\begin{code}
uVar :: Bool		-- False => tyvar is the "expected"
			-- True  => ty    is the "expected" thing
     -> TcTyVar
     -> TcTauType -> TcTauType	-- printing and real versions
     -> TcM s ()

uVar swapped tv1 ps_ty2 ty2
  = tcGetTyVar tv1	`thenNF_Tc` \ maybe_ty1 ->
    case maybe_ty1 of
	Just ty1 | swapped   -> uTys ps_ty2 ty2 ty1 ty1	-- Swap back
		 | otherwise -> uTys ty1 ty1 ps_ty2 ty2	-- Same order
	other       -> uUnboundVar swapped tv1 maybe_ty1 ps_ty2 ty2

	-- Expand synonyms
uUnboundVar swapped tv1 maybe_ty1 ps_ty2 (NoteTy _ ty2)
  = uUnboundVar swapped tv1 maybe_ty1 ps_ty2 ty2


	-- The both-type-variable case
uUnboundVar swapped tv1 maybe_ty1 ps_ty2 ty2@(TyVarTy tv2)

	-- Same type variable => no-op
  | tv1 == tv2
  = returnTc ()

	-- Distinct type variables
	-- ASSERT maybe_ty1 /= Just
  | otherwise
  = tcGetTyVar tv2	`thenNF_Tc` \ maybe_ty2 ->
    case maybe_ty2 of
	Just ty2' -> uUnboundVar swapped tv1 maybe_ty1 ty2' ty2'

	Nothing -> checkKinds swapped tv1 ty2			`thenTc_`

		   if tv1 `dominates` tv2 then
			tcPutTyVar tv2 (TyVarTy tv1)		`thenNF_Tc_`
			returnTc ()
		   else
			tcPutTyVar tv1 ps_ty2			`thenNF_Tc_`
			returnTc ()
  where
    tv1 `dominates` tv2 =  isSigTyVar tv1 
				-- Don't unify a signature type variable if poss
			|| varName tv1 `hasBetterProv` varName tv2 
				-- Try to update sys-y type variables in preference to sig-y ones

	-- Second one isn't a type variable
uUnboundVar swapped tv1 maybe_ty1 ps_ty2 non_var_ty2
  | non_var_ty2 == anyBoxKind
	-- If the 
  = returnTc ()

  | otherwise
  = checkKinds swapped tv1 non_var_ty2			`thenTc_`
    occur_check non_var_ty2				`thenTc_`
    checkTcM (not (isSigTyVar tv1))
	     (failWithTcM (unifyWithSigErr tv1 ps_ty2))	`thenTc_`
    tcPutTyVar tv1 ps_ty2				`thenNF_Tc_`
    returnTc ()
  where
    occur_check ty = mapTc occur_check_tv (varSetElems (tyVarsOfType ty))	`thenTc_`
		     returnTc ()

    occur_check_tv tv2
       | tv1 == tv2		-- Same tyvar; fail
       = zonkTcType ps_ty2	`thenNF_Tc` \ zonked_ty2 ->
	 failWithTcM (unifyOccurCheck tv1 zonked_ty2)

       | otherwise		-- A different tyvar
       = tcGetTyVar tv2	`thenNF_Tc` \ maybe_ty2 ->
	 case maybe_ty2 of
		Just ty2' -> occur_check ty2'
		other	  -> returnTc ()

checkKinds swapped tv1 ty2
  = tcAddErrCtxtM (unifyKindCtxt swapped tv1 ty2)	$

	-- We have to use tcTypeKind not just typeKind to get the
	-- kind of ty2, because there might be mutable kind variables
	-- in the way.  For example, suppose that ty2 :: (a b), and
	-- the kind of 'a' is a kind variable 'k' that has (presumably)
	-- been unified with 'k1 -> k2'.
    tcTypeKind ty2		`thenNF_Tc` \ k2 ->

    if swapped then
	unifyKind k2 (tyVarKind tv1)
    else
	unifyKind (tyVarKind tv1) k2
\end{code}

%************************************************************************
%*									*
\subsection[Unify-fun]{@unifyFunTy@}
%*									*
%************************************************************************

@unifyFunTy@ is used to avoid the fruitless creation of type variables.

\begin{code}
unifyFunTy :: TcType	 			-- Fail if ty isn't a function type
	   -> TcM s (TcType, TcType)	-- otherwise return arg and result types

unifyFunTy ty@(TyVarTy tyvar)
  = tcGetTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyFunTy ty'
	other	    -> unify_fun_ty_help ty

unifyFunTy ty
  = case splitFunTy_maybe ty of
	Just arg_and_res -> returnTc arg_and_res
	Nothing 	 -> unify_fun_ty_help ty

unify_fun_ty_help ty	-- Special cases failed, so revert to ordinary unification
  = newTyVarTy_OpenKind		`thenNF_Tc` \ arg ->
    newTyVarTy_OpenKind		`thenNF_Tc` \ res ->
    unifyTauTy ty (mkFunTy arg res)	`thenTc_`
    returnTc (arg,res)
\end{code}

\begin{code}
unifyListTy :: TcType              -- expected list type
	    -> TcM s TcType      -- list element type

unifyListTy ty@(TyVarTy tyvar)
  = tcGetTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyListTy ty'
	other	    -> unify_list_ty_help ty

unifyListTy ty
  = case splitTyConApp_maybe ty of
	Just (tycon, [arg_ty]) | tycon == listTyCon -> returnTc arg_ty
	other					    -> unify_list_ty_help ty

unify_list_ty_help ty	-- Revert to ordinary unification
  = newTyVarTy boxedTypeKind		`thenNF_Tc` \ elt_ty ->
    unifyTauTy ty (mkListTy elt_ty)	`thenTc_`
    returnTc elt_ty
\end{code}

\begin{code}
unifyTupleTy :: Arity -> TcType -> TcM s [TcType]
unifyTupleTy arity ty@(TyVarTy tyvar)
  = tcGetTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyTupleTy arity ty'
	other	    -> unify_tuple_ty_help arity ty

unifyTupleTy arity ty
  = case splitTyConApp_maybe ty of
	Just (tycon, arg_tys) |  isTupleTyCon tycon 
			 && tyConArity tycon == arity
			 -> returnTc arg_tys
	other -> unify_tuple_ty_help arity ty

unify_tuple_ty_help arity ty
  = mapNF_Tc (\ _ -> newTyVarTy boxedTypeKind) [1..arity]	`thenNF_Tc` \ arg_tys ->
    unifyTauTy ty (mkTupleTy arity arg_tys)			`thenTc_`
    returnTc arg_tys
\end{code}

\begin{code}
unifyUnboxedTupleTy :: Arity -> TcType -> TcM s [TcType]
unifyUnboxedTupleTy arity ty@(TyVarTy tyvar)
  = tcGetTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyUnboxedTupleTy arity ty'
	other	 -> unify_unboxed_tuple_ty_help arity ty

unifyUnboxedTupleTy arity ty
  = case splitTyConApp_maybe ty of
	Just (tycon, arg_tys) |  isUnboxedTupleTyCon tycon 
			 && tyConArity tycon == arity
			 -> returnTc arg_tys
	other -> unify_tuple_ty_help arity ty

unify_unboxed_tuple_ty_help arity ty
  = mapNF_Tc (\ _ -> newTyVarTy_OpenKind) [1..arity]	`thenNF_Tc` \ arg_tys ->
    unifyTauTy ty (mkUnboxedTupleTy arity arg_tys)	`thenTc_`
    returnTc arg_tys
\end{code}

Make sure a kind is of the form (Type b) for some boxity b.

\begin{code}
unifyTypeKind  :: TcKind -> TcM s ()
unifyTypeKind kind@(TyVarTy kv)
  = tcGetTyVar kv 	`thenNF_Tc` \ maybe_kind ->
    case maybe_kind of
	Just kind' -> unifyTypeKind kind'
	Nothing    -> unify_type_kind_help kind

unifyTypeKind kind
  = case splitTyConApp_maybe kind of
	Just (tycon, [_]) | tycon == typeCon -> returnTc ()
	other				     -> unify_type_kind_help kind

unify_type_kind_help kind
  = newOpenTypeKind	`thenNF_Tc` \ expected_kind ->
    unifyKind expected_kind kind
\end{code}


%************************************************************************
%*									*
\subsection[Unify-context]{Errors and contexts}
%*									*
%************************************************************************

Errors
~~~~~~

\begin{code}
unifyCtxt s ty1 ty2 tidy_env	-- ty1 expected, ty2 inferred
  = zonkTcType ty1	`thenNF_Tc` \ ty1' ->
    zonkTcType ty2	`thenNF_Tc` \ ty2' ->
    returnNF_Tc (err ty1' ty2')
  where
    err ty1 ty2 = (env1, 
		   nest 4 
			(vcat [
			   text "Expected" <+> text s <> colon <+> ppr tidy_ty1,
			   text "Inferred" <+> text s <> colon <+> ppr tidy_ty2
		        ]))
		  where
		    (env1, [tidy_ty1,tidy_ty2]) = tidyOpenTypes tidy_env [ty1,ty2]

unifyKindCtxt swapped tv1 ty2 tidy_env	-- not swapped => tv1 expected, ty2 inferred
  = returnNF_Tc (env2, ptext SLIT("When matching types") <+> 
		       sep [quotes pp_expected, ptext SLIT("and"), quotes pp_actual])
  where
    (pp_expected, pp_actual) | swapped   = (pp2, pp1)
			     | otherwise = (pp1, pp2)
    (env1, tv1') = tidyTyVar tidy_env tv1
    (env2, ty2') = tidyOpenType  env1     ty2
    pp1 = ppr tv1'
    pp2 = ppr ty2'

unifyMisMatch ty1 ty2
  = zonkTcType ty1	`thenNF_Tc` \ ty1' ->
    zonkTcType ty2	`thenNF_Tc` \ ty2' ->
    let
    	(env, [tidy_ty1, tidy_ty2]) = tidyOpenTypes emptyTidyEnv [ty1',ty2']
	msg = hang (ptext SLIT("Couldn't match"))
		   4 (sep [quotes (ppr tidy_ty1), 
			   ptext SLIT("against"), 
			   quotes (ppr tidy_ty2)])
    in
    failWithTcM (env, msg)

unifyWithSigErr tyvar ty
  = (env2, hang (ptext SLIT("Cannot unify the type-signature variable") <+> quotes (ppr tidy_tyvar))
	      4 (ptext SLIT("with the type") <+> quotes (ppr tidy_ty)))
  where
    (env1, tidy_tyvar) = tidyTyVar emptyTidyEnv tyvar
    (env2, tidy_ty)    = tidyOpenType  env1     ty

unifyOccurCheck tyvar ty
  = (env2, hang (ptext SLIT("Occurs check: cannot construct the infinite type:"))
	      4 (sep [ppr tidy_tyvar, char '=', ppr tidy_ty]))
  where
    (env1, tidy_tyvar) = tidyTyVar emptyTidyEnv tyvar
    (env2, tidy_ty)    = tidyOpenType  env1     ty
\end{code}

