%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Unify]{Unifier}

The unifier is now squarely in the typechecker monad (because of the
updatable substitution).

\begin{code}
#include "HsVersions.h"

module Unify ( unifyTauTy, unifyTauTyList, unifyTauTyLists, 
	       unifyFunTy, unifyListTy, unifyTupleTy
 ) where

IMP_Ubiq()


-- friends: 
import TcMonad
import Type	( GenType(..), typeKind, mkFunTy, getFunTy_maybe, splitAppTys )
import TyCon	( TyCon, mkFunTyCon, isTupleTyCon, tyConArity )
import Class	( GenClass )
import TyVar	( GenTyVar(..), SYN_IE(TyVar), tyVarKind )
import TcType	( SYN_IE(TcType), TcMaybe(..), SYN_IE(TcTauType), SYN_IE(TcTyVar),
		  newTyVarTy, tcReadTyVar, tcWriteTyVar, zonkTcType
		)
-- others:
import Kind	( Kind, hasMoreBoxityInfo, mkTypeKind, mkBoxedTypeKind )
import TysWiredIn ( listTyCon, mkListTy, mkTupleTy )
import Usage	( duffUsage )
import PprType	( GenTyVar, GenType )	-- instances
import Pretty
import Unique	( Unique )		-- instances
import Util

#if __GLASGOW_HASKELL__ >= 202
import Outputable
#endif

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

	-- Variables; go for uVar
uTys ps_ty1 (TyVarTy tyvar1) ps_ty2 ty2 = uVar tyvar1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (TyVarTy tyvar2) = uVar tyvar2 ps_ty1 ty1

	-- Applications and functions; just check the two parts
uTys _ (FunTy fun1 arg1 _) _ (FunTy fun2 arg2 _)
  = uTys fun1 fun1 fun2 fun2	`thenTc_`    uTys arg1 arg1 arg2 arg2

uTys _ (AppTy s1 t1) _ (AppTy s2 t2)
  = uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2

	-- Special case: converts  a -> b to (->) a b
uTys _ (AppTy s1 t1) _ (FunTy fun2 arg2 _)
  = uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2
  where
    s2 = AppTy (TyConTy mkFunTyCon duffUsage) fun2
    t2 = arg2

uTys _ (FunTy fun1 arg1 _) _ (AppTy s2 t2)
  = uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2
  where
    s1 = AppTy (TyConTy mkFunTyCon duffUsage) fun1
    t1 = arg1

	-- Type constructors must match
uTys ps_ty1 (TyConTy con1 _) ps_ty2 (TyConTy con2 _)
  = checkTc (con1 == con2) (unifyMisMatch ps_ty1 ps_ty2)

	-- Dictionary types must match.  (They can only occur when
	-- unifying signature contexts in TcBinds.)
uTys ps_ty1 (DictTy c1 t1 _) ps_ty2 (DictTy c2 t2 _)
  = checkTc (c1 == c2) (unifyMisMatch ps_ty1 ps_ty2)	`thenTc_`
    uTys t1 t1 t2 t2

	-- Always expand synonyms (see notes at end)
uTys ps_ty1 (SynTy con1 args1 ty1) ps_ty2 ty2 = uTys ps_ty1 ty1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (SynTy con2 args2 ty2) = uTys ps_ty1 ty1 ps_ty2 ty2

	-- Not expecting for-alls in unification
#ifdef DEBUG
uTys ps_ty1 (ForAllTy _ _)	  ps_ty2 ty2 = panic "Unify.uTys:ForAllTy (1st arg)"
uTys ps_ty1 ty1 ps_ty2	      (ForAllTy _ _) = panic "Unify.uTys:ForAllTy (2nd arg)"
uTys ps_ty1 (ForAllUsageTy _ _ _) ps_ty2 ty2 = panic "Unify.uTys:ForAllUsageTy (1st arg)"
uTys ps_ty1 ty1 ps_ty2 (ForAllUsageTy _ _ _) = panic "Unify.uTys:ForAllUsageTy (2nd arg)"
#endif

	-- Anything else fails
uTys ps_ty1 ty1 ps_ty2 ty2  = failTc (unifyMisMatch ps_ty1 ps_ty2)
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
uUnboundVar tv1 maybe_ty1 ps_ty2 (SynTy _ _ ty2)
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
    case (maybe_ty1, maybe_ty2) of
	(_, BoundTo ty2') -> uUnboundVar tv1 maybe_ty1 ty2' ty2'

	(UnBound, _) |  kind2 `hasMoreBoxityInfo` kind1
		     -> tcWriteTyVar tv1 ps_ty2		`thenNF_Tc_` returnTc ()
	
	(_, UnBound) |  kind1 `hasMoreBoxityInfo` kind2
		     -> tcWriteTyVar tv2 (TyVarTy tv1)	`thenNF_Tc_` returnTc ()

-- Allow two type-sig variables to be bound together.
-- They may be from the same binding group, so it may be OK.
	(DontBind,DontBind) |  kind2 `hasMoreBoxityInfo` kind1
		            -> tcWriteTyVar tv1 ps_ty2		`thenNF_Tc_` returnTc ()
	
			    |  kind1 `hasMoreBoxityInfo` kind2
			    -> tcWriteTyVar tv2 (TyVarTy tv1)	`thenNF_Tc_` returnTc ()

	other	     -> failTc (unifyKindErr tv1 ps_ty2)

	-- Second one isn't a type variable
uUnboundVar tv1@(TyVar uniq1 kind1 name1 box1) maybe_ty1 ps_ty2 non_var_ty2
  = case maybe_ty1 of
	DontBind -> failTc (unifyDontBindErr tv1 ps_ty2)

	UnBound	 |  typeKind non_var_ty2 `hasMoreBoxityInfo` kind1
		 -> occur_check non_var_ty2			`thenTc_`
		    tcWriteTyVar tv1 ps_ty2			`thenNF_Tc_`
		    returnTc ()

	other	 -> failTc (unifyKindErr tv1 ps_ty2)
  where
    occur_check (TyVarTy tv2@(TyVar uniq2 _ _ box2))
       | uniq1 == uniq2		-- Same tyvar; fail
       = failTc (unifyOccurCheck tv1 ps_ty2)

       | otherwise		-- A different tyvar
       = tcReadTyVar tv2	`thenNF_Tc` \ maybe_ty2 ->
	 case maybe_ty2 of
		BoundTo ty2' -> occur_check ty2'
		other	     -> returnTc ()

    occur_check (AppTy fun arg)   = occur_check fun `thenTc_` occur_check arg
    occur_check (FunTy fun arg _) = occur_check fun `thenTc_` occur_check arg
    occur_check (TyConTy _ _)	  = returnTc ()
    occur_check (SynTy _ _ ty2)   = occur_check ty2

	-- DictTys and ForAllTys can occur when pattern matching against
	-- constructors with universally quantified fields.
    occur_check (DictTy c ty2 _)  = occur_check ty2
    occur_check (ForAllTy tv ty2) | tv == tv1 = returnTc ()
				  | otherwise = occur_check ty2
    occur_check other		  = panic "Unexpected ForAllUsage in occurCheck"
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
  = case getFunTy_maybe ty of
	Just arg_and_res -> returnTc arg_and_res
	Nothing 	 -> unify_fun_ty_help ty

unify_fun_ty_help ty	-- Special cases failed, so revert to ordinary unification
  = newTyVarTy mkTypeKind		`thenNF_Tc` \ arg ->
    newTyVarTy mkTypeKind		`thenNF_Tc` \ res ->
    unifyTauTy (mkFunTy arg res) ty	`thenTc_`
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

unifyListTy (AppTy (TyConTy tycon _) arg_ty)
  | tycon == listTyCon
  = returnTc arg_ty

unifyListTy ty = unify_list_ty_help ty

unify_list_ty_help ty	-- Revert to ordinary unification
  = newTyVarTy mkBoxedTypeKind		`thenNF_Tc` \ elt_ty ->
    unifyTauTy (mkListTy elt_ty) ty	`thenTc_`
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
  = case splitAppTys ty of
	(TyConTy tycon _, arg_tys) |  isTupleTyCon tycon 
				   && tyConArity tycon == arity
				   -> returnTc arg_tys
	other -> unify_tuple_ty_help arity ty

unify_tuple_ty_help arity ty
  = mapNF_Tc (\ _ -> newTyVarTy mkBoxedTypeKind) [1..arity]	`thenNF_Tc` \ arg_tys ->
    unifyTauTy (mkTupleTy arity arg_tys) ty			`thenTc_`
    returnTc arg_tys
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
    err ty1' ty2' sty = vcat [
			   hsep [ptext SLIT("Expected:"), ppr sty ty1'],
			   hsep [ptext SLIT("Inferred:"), ppr sty ty2']
		        ]

unifyMisMatch ty1 ty2 sty
  = hang (ptext SLIT("Couldn't match the type"))
	 4 (sep [ppr sty ty1, ptext SLIT("against"), ppr sty ty2])

expectedFunErr ty sty
  = hang (text "Function type expected, but found the type")
	 4 (ppr sty ty)

unifyKindErr tyvar ty sty
  = hang (ptext SLIT("Compiler bug: kind mis-match between"))
	 4 (sep [hsep [ppr sty tyvar, ptext SLIT("::"), ppr sty (tyVarKind tyvar)],
		   ptext SLIT("and"), 
		   hsep [ppr sty ty, ptext SLIT("::"), ppr sty (typeKind ty)]])

unifyDontBindErr tyvar ty sty
  = hang (ptext SLIT("Couldn't match the signature/existential type variable"))
	 4 (sep [ppr sty tyvar,
		   ptext SLIT("with the type"), 
		   ppr sty ty])

unifyOccurCheck tyvar ty sty
  = hang (ptext SLIT("Cannot construct the infinite type (occur check)"))
	 4 (sep [ppr sty tyvar, char '=', ppr sty ty])
\end{code}

