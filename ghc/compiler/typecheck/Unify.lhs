%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Unify]{Unifier}

The unifier is now squarely in the typechecker monad (because of the
updatable substitution).

\begin{code}
#include "HsVersions.h"

module Unify ( unifyTauTy, unifyTauTyList, unifyTauTyLists ) where

import Ubiq

-- friends: 
import TcMonad
import Type	( GenType(..), getTypeKind )
import TyCon	( TyCon(..), ConsVisible, NewOrData )
import TyVar	( GenTyVar(..), TyVar(..) )
import TcType	( TcType(..), TcMaybe(..), TcTauType(..), TcTyVar(..),
		  tcReadTyVar, tcWriteTyVar
		)
-- others:
import Kind	( Kind, isSubKindOf )
import PprType	( GenTyVar, GenType )	-- instances
import Pretty
import Unique	( Unique )		-- instances
import Util
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
unifyTauTy ty1 ty2 
  = tcAddErrCtxt (unifyCtxt ty1 ty2) $
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
unifyTauTypeLists ty1s ty2s = panic "Unify.unifyTauTypeLists: mismatched type lists!"
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
uTys _ (AppTy fun1 arg1) _ (AppTy fun2 arg2)
  = uTys fun1 fun1 fun2 fun2	`thenTc_`    uTys arg1 arg1 arg2 arg2

	-- Type constructors must match
uTys ps_ty1 (TyConTy con1 _) ps_ty2 (TyConTy con2 _)
  = checkTc (con1 == con2) (unifyMisMatch ps_ty1 ps_ty2)

	-- Always expand synonyms (see notes at end)
uTys ps_ty1 (SynTy con1 args1 ty1) ps_ty2 ty2 = uTys ps_ty1 ty1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (SynTy con2 args2 ty2) = uTys ps_ty1 ty1 ps_ty2 ty2

	-- Special case: converts  (->) a b  to  a -> b
uTys ps_ty1 (AppTy (AppTy (TyConTy FunTyCon u) fun) arg) ps_ty2 ty2
  = uTys ps_ty1 (FunTy fun arg u) ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (AppTy (AppTy (TyConTy FunTyCon u) fun) arg)
  = uTys ps_ty1 ty1 ps_ty2 (FunTy fun arg u)

	-- Anything else fails
uTys ps_ty1 ty1 ps_ty2 ty2  = failTc (unifyMisMatch ps_ty1 ps_ty2)
\end{code}

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
	UnBound  -> uUnboundVar tv1 ps_ty2 ty2

	-- Expand synonyms
uUnboundVar tv1 ps_ty2 (SynTy _ _ ty2) = uUnboundVar tv1 ps_ty2 ty2


	-- The both-type-variable case
uUnboundVar tv1@(TyVar uniq1 kind1 name1 box1)
	    ps_ty2
	    ty2@(TyVarTy tv2@(TyVar uniq2 kind2 name2 box2))

	-- Same type variable => no-op
  | uniq1 == uniq2
  = returnTc ()

	-- Distinct type variables
  | otherwise
  = tcReadTyVar tv2	`thenNF_Tc` \ maybe_ty2 ->
    case maybe_ty2 of
	BoundTo ty2' -> uUnboundVar tv1 ty2' ty2'
	UnBound   -> if kind2 `isSubKindOf` kind1 then
			tcWriteTyVar tv1 ty2		`thenNF_Tc_` returnTc ()
		     else if kind1 `isSubKindOf` kind2 then
			tcWriteTyVar tv2 (TyVarTy tv1)	`thenNF_Tc_` returnTc ()
		     else
			failTc (unifyKindErr tv1 ps_ty2)

	-- Second one isn't a type variable
uUnboundVar tv1@(TyVar uniq1 kind1 name1 box1) ps_ty2 non_var_ty2
  = occur_check non_var_ty2			`thenTc_`
    checkTc (getTypeKind non_var_ty2 `isSubKindOf` kind1)
	    (unifyKindErr tv1 ps_ty2)		`thenTc_`
    tcWriteTyVar tv1 non_var_ty2		`thenNF_Tc_`
    returnTc ()
  where
    occur_check (TyVarTy tv2@(TyVar uniq2 _ _ box2))
       | uniq1 == uniq2		-- Same tyvar; fail
       = failTc (unifyOccurCheck tv1 ps_ty2)

       | otherwise		-- A different tyvar
       = tcReadTyVar tv2	`thenNF_Tc` \ maybe_ty2 ->
	 case maybe_ty2 of
		BoundTo ty2' -> occur_check ty2'
		UnBound   -> returnTc ()

    occur_check (AppTy fun arg)   = occur_check fun `thenTc_` occur_check arg
    occur_check (FunTy fun arg _) = occur_check fun `thenTc_` occur_check arg
    occur_check (TyConTy _ _)	  = returnTc ()
    occur_check (SynTy _ _ ty2)   = occur_check ty2
    occur_check other		  = panic "Unexpected Dict or ForAll in occurCheck"
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


Errors
~~~~~~

\begin{code}
unifyCtxt ty1 ty2 sty
  = ppAboves [
	ppCat [ppStr "Expected:", ppr sty ty1],
	ppCat [ppStr "  Actual:", ppr sty ty2]
    ]

unifyMisMatch ty1 ty2 sty
  = ppHang (ppStr "Couldn't match the type")
	 4 (ppSep [ppr sty ty1, ppStr "against", ppr sty ty2])

unifyKindErr tyvar ty sty
  = ppHang (ppStr "Kind mis-match between")
	 4 (ppSep [ppr sty tyvar, ppStr "and", ppr sty ty])

unifyOccurCheck tyvar ty sty
  = ppHang (ppStr "Occur check: cannot construct the infinite type")
	 4 (ppSep [ppr sty tyvar, ppStr "=", ppr sty ty])
\end{code}

