%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Unify]{Unifier}

The unifier is now squarely in the typechecker monad (because of the
updatable substitution).

\begin{code}
module Unify ( unifyTauTy, unifyTauTyList, unifyTauTyLists, 
	       unifyFunTy, unifyListTy, unifyTupleTy,
	       Subst, unifyTysX, unifyTyListsX
 ) where

#include "HsVersions.h"

-- friends: 
import TcMonad
import Type	( GenType(..), Type, tyVarsOfType,
		  typeKind, mkFunTy, splitFunTy_maybe, splitTyConApp_maybe )
import TyCon	( TyCon, mkFunTyCon, isTupleTyCon, tyConArity, Arity )
import TyVar	( TyVar(..), GenTyVar(..), tyVarKind, tyVarFlexi,
		  TyVarEnv, lookupTyVarEnv, emptyTyVarEnv, addToTyVarEnv,
		  tyVarSetToList
		)
import TcType	( TcType, TcMaybe(..), TcTauType, TcTyVar,
		  newTyVarTy, tcReadTyVar, tcWriteTyVar, zonkTcType
		)
-- others:
import Kind	( Kind, hasMoreBoxityInfo, mkTypeKind, mkBoxedTypeKind )
import TysWiredIn ( listTyCon, mkListTy, mkTupleTy )
import Maybes	( maybeToBool )
import PprType	()		-- Instances
import Util
import Outputable
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
unifyTauTy :: TcTauType s -> TcTauType s -> TcM s ()
unifyTauTy ty1 ty2 	-- ty1 expected, ty2 inferred
  = tcAddErrCtxtM (unifyCtxt ty1 ty2) $
    uTys ty1 ty1 ty2 ty2
\end{code}

@unifyTauTyList@ unifies corresponding elements of two lists of
@TauType@s.  It uses @uTys@ to do the real work.  The lists should be
of equal length.  We charge down the list explicitly so that we can
complain if their lengths differ.

\begin{code}
unifyTauTyLists :: [TcTauType s] -> [TcTauType s] ->  TcM s ()
unifyTauTyLists [] 	     []	        = returnTc ()
unifyTauTyLists (ty1:tys1) (ty2:tys2) = uTys ty1 ty1 ty2 ty2   `thenTc_`
					unifyTauTyLists tys1 tys2
unifyTauTyLists ty1s ty2s = panic "Unify.unifyTauTyLists: mismatched type lists!"
\end{code}

@unifyTauTyList@ takes a single list of @TauType@s and unifies them
all together.  It is used, for example, when typechecking explicit
lists, when all the elts should be of the same type.

\begin{code}
unifyTauTyList :: [TcTauType s] -> TcM s ()
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
uTys :: TcTauType s -> TcTauType s	-- Error reporting ty1 and real ty1
     -> TcTauType s -> TcTauType s	-- Error reporting ty2 and real ty2
     -> TcM s ()

	-- Always expand synonyms (see notes at end)
uTys ps_ty1 (SynTy _ ty1) ps_ty2 ty2 = uTys ps_ty1 ty1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (SynTy _ ty2) = uTys ps_ty1 ty1 ps_ty2 ty2

	-- Variables; go for uVar
uTys ps_ty1 (TyVarTy tyvar1) ps_ty2 ty2 = uVar tyvar1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (TyVarTy tyvar2) = uVar tyvar2 ps_ty1 ty1

	-- Functions; just check the two parts
uTys _ (FunTy fun1 arg1) _ (FunTy fun2 arg2)
  = uTys fun1 fun1 fun2 fun2	`thenTc_`    uTys arg1 arg1 arg2 arg2

	-- Type constructors must match
uTys ps_ty1 (TyConApp con1 tys1) ps_ty2 (TyConApp con2 tys2)
  = checkTc (con1 == con2 && length tys1 == length tys2) 
	    (unifyMisMatch ps_ty1 ps_ty2)		`thenTc_`
    unifyTauTyLists tys1 tys2

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp
uTys _ (AppTy s1 t1) _ (AppTy s2 t2)
  = uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2

uTys _ (AppTy s1 t1) _ (FunTy fun2 arg2)
  = uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2
  where
	 -- Converts  a -> b to (->) a b
    s2 = TyConApp mkFunTyCon [fun2]
    t2 = arg2

uTys _ (AppTy s1 t1) _ (TyConApp tc tys@(_:_))
  = case snocView tys of
	(ts2, t2) -> uTys s1 s1 s2 s2	`thenTc_`   uTys t1 t1 t2 t2
		  where
			-- Not efficient, but simple
		     s2 = TyConApp tc ts2

uTys ps1 s1 ps2 s2@(AppTy _ _) = uTys ps2 s2 ps1 s1
	-- Swap arguments if the App is in the second argument

	-- Not expecting for-alls in unification
#ifdef DEBUG
uTys ps_ty1 (ForAllTy _ _)	  ps_ty2 ty2 = panic "Unify.uTys:ForAllTy (1st arg)"
uTys ps_ty1 ty1 ps_ty2	      (ForAllTy _ _) = panic "Unify.uTys:ForAllTy (2nd arg)"
#endif

	-- Anything else fails
uTys ps_ty1 ty1 ps_ty2 ty2  = failWithTc (unifyMisMatch ps_ty1 ps_ty2)
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
of the substitution; rather, notice that @bindTo@ (defined below) nips
back into @uTys@ if it turns out that the variable is already bound.

There is a slight worry that one might try to @bindTo@ a (say) Poly
tyvar (as tv1) with an Open tyvar (as ty2) which is already unified to
an unboxed type.  In fact this can't happen, because the Open ones are
always the ones which are unified away.

\begin{code}
uVar :: TcTyVar s
     -> TcTauType s -> TcTauType s	-- printing and real versions
     -> TcM s ()

uVar tv1 ps_ty2 ty2
  = tcReadTyVar tv1	`thenNF_Tc` \ maybe_ty1 ->
    case maybe_ty1 of
	BoundTo ty1 -> uTys ty1 ty1 ps_ty2 ty2
	other       -> uUnboundVar tv1 maybe_ty1 ps_ty2 ty2

	-- Expand synonyms
uUnboundVar tv1 maybe_ty1 ps_ty2 (SynTy _ ty2)
  = uUnboundVar tv1 maybe_ty1 ps_ty2 ty2


	-- The both-type-variable case
uUnboundVar tv1@(TyVar uniq1 kind1 name1 box1)
	    maybe_ty1
	    ps_ty2
	    ty2@(TyVarTy tv2@(TyVar uniq2 kind2 name2 box2))

	-- Same type variable => no-op
  | uniq1 == uniq2
  = returnTc ()

	-- Distinct type variables
	-- ASSERT maybe_ty1 /= BoundTo
  | otherwise
  = tcReadTyVar tv2	`thenNF_Tc` \ maybe_ty2 ->
    case maybe_ty2 of
	BoundTo ty2' -> uUnboundVar tv1 maybe_ty1 ty2' ty2'

	UnBound |  (kind1 == kind2 && not (maybeToBool name1))	-- Same kinds and tv1 is anonymous
								-- so update tv1
		-> tcWriteTyVar tv1 ps_ty2		`thenNF_Tc_` returnTc ()
	
	        |  kind1 `hasMoreBoxityInfo` kind2		-- Update tv2 if possible
		-> tcWriteTyVar tv2 (TyVarTy tv1)	`thenNF_Tc_` returnTc ()

		| kind2 `hasMoreBoxityInfo` kind1		-- Update tv1 if possible
		-> tcWriteTyVar tv1 ps_ty2		`thenNF_Tc_` returnTc ()
	
	other	-> failWithTc (unifyKindErr tv1 ps_ty2)

	-- Second one isn't a type variable
uUnboundVar tv1@(TyVar uniq1 kind1 name1 box1) maybe_ty1 ps_ty2 non_var_ty2
  |  typeKind non_var_ty2 `hasMoreBoxityInfo` kind1
  =  occur_check non_var_ty2			`thenTc_`
     tcWriteTyVar tv1 ps_ty2			`thenNF_Tc_`
     returnTc ()

  | otherwise 
  = failWithTc (unifyKindErr tv1 ps_ty2)

  where
    occur_check ty = mapTc occur_check_tv (tyVarSetToList (tyVarsOfType ty))	`thenTc_`
		     returnTc ()

    occur_check_tv tv2@(TyVar uniq2 _ _ box2)
       | uniq1 == uniq2		-- Same tyvar; fail
       = failWithTc (unifyOccurCheck tv1 ps_ty2)

       | otherwise		-- A different tyvar
       = tcReadTyVar tv2	`thenNF_Tc` \ maybe_ty2 ->
	 case maybe_ty2 of
		BoundTo ty2' -> occur_check ty2'
		other	     -> returnTc ()
\end{code}

%************************************************************************
%*									*
\subsection[Unify-fun]{@unifyFunTy@}
%*									*
%************************************************************************

@unifyFunTy@ is used to avoid the fruitless creation of type variables.

\begin{code}
unifyFunTy :: TcType s	 			-- Fail if ty isn't a function type
	   -> TcM s (TcType s, TcType s)	-- otherwise return arg and result types

unifyFunTy ty@(TyVarTy tyvar)
  = tcReadTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty' -> unifyFunTy ty'
	other	    -> unify_fun_ty_help ty

unifyFunTy ty
  = case splitFunTy_maybe ty of
	Just arg_and_res -> returnTc arg_and_res
	Nothing 	 -> unify_fun_ty_help ty

unify_fun_ty_help ty	-- Special cases failed, so revert to ordinary unification
  = newTyVarTy mkTypeKind		`thenNF_Tc` \ arg ->
    newTyVarTy mkTypeKind		`thenNF_Tc` \ res ->
    unifyTauTy ty (mkFunTy arg res)	`thenTc_`
    returnTc (arg,res)
\end{code}

\begin{code}
unifyListTy :: TcType s              -- expected list type
	    -> TcM s (TcType s)      -- list element type

unifyListTy ty@(TyVarTy tyvar)
  = tcReadTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty' -> unifyListTy ty'
	other	    -> unify_list_ty_help ty

unifyListTy ty
  = case splitTyConApp_maybe ty of
	Just (tycon, [arg_ty]) | tycon == listTyCon -> returnTc arg_ty
	other					    -> unify_list_ty_help ty

unify_list_ty_help ty	-- Revert to ordinary unification
  = newTyVarTy mkBoxedTypeKind		`thenNF_Tc` \ elt_ty ->
    unifyTauTy ty (mkListTy elt_ty)	`thenTc_`
    returnTc elt_ty
\end{code}

\begin{code}
unifyTupleTy :: Arity -> TcType s -> TcM s [TcType s]
unifyTupleTy arity ty@(TyVarTy tyvar)
  = tcReadTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty' -> unifyTupleTy arity ty'
	other	    -> unify_tuple_ty_help arity ty

unifyTupleTy arity ty
  = case splitTyConApp_maybe ty of
	Just (tycon, arg_tys) |  isTupleTyCon tycon 
			 && tyConArity tycon == arity
			 -> returnTc arg_tys
	other -> unify_tuple_ty_help arity ty

unify_tuple_ty_help arity ty
  = mapNF_Tc (\ _ -> newTyVarTy mkBoxedTypeKind) [1..arity]	`thenNF_Tc` \ arg_tys ->
    unifyTauTy ty (mkTupleTy arity arg_tys)			`thenTc_`
    returnTc arg_tys
\end{code}

%************************************************************************
%*									*
\subsection{Unification wih a explicit substitution}
%*									*
%************************************************************************

Unify types with an explicit substitution and no monad.

\begin{code}
type Subst  = TyVarEnv (GenType Bool)	-- Not necessarily idempotent

unifyTysX :: GenType Bool
          -> GenType Bool
          -> Maybe Subst
unifyTysX ty1 ty2 = uTysX ty1 ty2 (\s -> Just s) emptyTyVarEnv

unifyTyListsX :: [GenType Bool] -> [GenType Bool] -> Maybe Subst
unifyTyListsX tys1 tys2 = uTyListsX tys1 tys2 (\s -> Just s) emptyTyVarEnv


uTysX :: GenType Bool
      -> GenType Bool
      -> (Subst -> Maybe Subst)
      -> Subst
      -> Maybe Subst

uTysX (SynTy _ ty1) ty2 k subst = uTysX ty1 ty2 k subst
uTysX ty1 (SynTy _ ty2) k subst = uTysX ty1 ty2 k subst

	-- Variables; go for uVar
uTysX (TyVarTy tyvar1) (TyVarTy tyvar2) k subst 
  | tyvar1 == tyvar2
  = k subst
uTysX (TyVarTy tyvar1) ty2 k subst 
  | tyVarFlexi tyvar1
  = uVarX tyvar1 ty2 k subst
uTysX ty1 (TyVarTy tyvar2) k subst 
  | tyVarFlexi tyvar2
  = uVarX tyvar2 ty1 k subst

	-- Functions; just check the two parts
uTysX (FunTy fun1 arg1) (FunTy fun2 arg2) k subst
  = uTysX fun1 fun2 (uTysX arg1 arg2 k) subst

	-- Type constructors must match
uTysX (TyConApp con1 tys1) (TyConApp con2 tys2) k subst
  | (con1 == con2 && length tys1 == length tys2)
  = uTyListsX tys1 tys2 k subst

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp
uTysX (AppTy s1 t1) (AppTy s2 t2) k subst
  = uTysX s1 s2 (uTysX t1 t2 k) subst

uTysX (AppTy s1 t1) (FunTy fun2 arg2) k subst
  = uTysX s1 s2 (uTysX t1 t2 k) subst
  where
	 -- Converts  a -> b to (->) a b
    s2 = TyConApp mkFunTyCon [fun2]
    t2 = arg2

uTysX (AppTy s1 t1) (TyConApp tc tys@(_:_)) k subst
  = case snocView tys of
	(ts2, t2) -> uTysX s1 s2 (uTysX t1 t2 k) subst
		  where
			-- Not efficient, but simple
		     s2 = TyConApp tc ts2

uTysX s1 s2@(AppTy _ _) k subst = uTysX s2 s1 k subst
	-- Swap arguments if the App is in the second argument

	-- Not expecting for-alls in unification
#ifdef DEBUG
uTysX (ForAllTy _ _) ty2 k subst = panic "Unify.uTysX subst:ForAllTy (1st arg)"
uTysX ty1 (ForAllTy _ _) k subst = panic "Unify.uTysX subst:ForAllTy (2nd arg)"
#endif

	-- Anything else fails
uTysX ty1 ty2 k subst = Nothing


uTyListsX []         []         k subst = k subst
uTyListsX (ty1:tys1) (ty2:tys2) k subst = uTysX ty1 ty2 (uTyListsX tys1 tys2 k) subst
uTyListsX tys1	     tys2       k subst = Nothing   -- Fail if the lists are different lengths
\end{code}

\begin{code}
-- Invariant: tv1 is a unifiable variable
uVarX tv1 ty2 k subst
  = case lookupTyVarEnv subst tv1 of
      Just ty1 ->    -- Already bound
		     uTysX ty1 ty2 k subst

      Nothing	     -- Not already bound
	       |  typeKind ty2 `hasMoreBoxityInfo` tyVarKind tv1
	       && occur_check_ok ty2
	       ->     -- No kind mismatch nor occur check
	          k (addToTyVarEnv subst tv1 ty2)

	       | otherwise -> Nothing	-- Fail if kind mis-match or occur check
  where
    occur_check_ok ty = all occur_check_ok_tv (tyVarSetToList (tyVarsOfType ty))
    occur_check_ok_tv tv | tv1 == tv = False
			 | otherwise = case lookupTyVarEnv subst tv of
				         Nothing -> True
					 Just ty -> occur_check_ok ty
\end{code}


%************************************************************************
%*									*
\subsection[Unify-context]{Errors and contexts}
%*									*
%************************************************************************

Errors
~~~~~~

\begin{code}
unifyCtxt ty1 ty2		-- ty1 expected, ty2 inferred
  = zonkTcType ty1	`thenNF_Tc` \ ty1' ->
    zonkTcType ty2	`thenNF_Tc` \ ty2' ->
    returnNF_Tc (err ty1' ty2')
  where
    err ty1' ty2' = vcat [
			   hsep [ptext SLIT("Expected:"), ppr ty1'],
			   hsep [ptext SLIT("Inferred:"), ppr ty2']
		        ]

unifyMisMatch ty1 ty2
  = hang (ptext SLIT("Couldn't match the type"))
	 4 (sep [quotes (ppr ty1), ptext SLIT("against"), quotes (ppr ty2)])

unifyKindErr tyvar ty
  = hang (ptext SLIT("Kind mis-match between"))
	 4 (sep [quotes (hsep [ppr tyvar, ptext SLIT("::"), ppr (tyVarKind tyvar)]),
		 ptext SLIT("and"), 
		 quotes (hsep [ppr ty, ptext SLIT("::"), ppr (typeKind ty)])])

unifyOccurCheck tyvar ty
  = hang (ptext SLIT("Occurs check: cannot construct the infinite type:"))
	 8 (sep [ppr tyvar, char '=', ppr ty])
\end{code}

