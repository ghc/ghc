%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Unify]{Unifier}

The unifier is now squarely in the typechecker monad (because of the
updatable substitution).

\begin{code}
#include "HsVersions.h"

module Unify ( unifyTauTy, unifyTauTyList, unifyTauTyLists ) where

IMPORT_Trace		-- ToDo: rm (debugging only)
import Outputable
import Pretty

import AbsSyn
import TcMonad

import CmdLineOpts	( GlobalSwitch(..) )
import CoreLift		( isUnboxedButNotState )
import Errors		( unifyErr, UnifyErrInfo(..), UnifyErrContext  )
import Id		( Id, DataCon(..), Inst )
import Maybes		( Maybe(..) )
import Subst		( extendSubst, SubstResult(..), Subst )
#if USE_ATTACK_PRAGMAS
import Class		( Class(..), cmpClass ) -- .. for pragmas only
import TyCon		( TyCon(..), isBoxedTyCon, isVisibleSynTyCon, cmpTyCon )
			-- .. on TyCon is for pragmas only
import TyVar		-- make all visible for pragmas
import UniTyFuns	( pprUniType, pprTyCon ) 
#else
import Class		( Class )
import TyVar		( TyVar(..), TyVarTemplate )
import TyCon		( TyCon, isBoxedTyCon, isVisibleSynTyCon )
#endif
import UniType		( UniType(..), TauType(..)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import Util
\end{code}

%************************************************************************
%*									*
\subsection[Unify-spec]{Specification}
%*									*
%************************************************************************

CLAIM: the unifier works correctly even if the types to be unified are not
fixed points of the substitution.

%************************************************************************
%*									*
\subsection[Unify-exported]{Exported unification functions}
%*									*
%************************************************************************

The exported functions are all defined as versions of some
non-exported generic functions.

Unify two @TauType@s.  Dead straightforward.

\begin{code}
unifyTauTy :: TauType -> TauType -> UnifyErrContext -> TcM ()

unifyTauTy ty1 ty2 err_ctxt = uTys ty1 ty1 ty2 ty2 err_ctxt
\end{code}

@unifyTauTyLists@ unifies corresponding elements of its two list
arguments.  The lists should be of equal length.

\begin{code}
unifyTauTyLists :: [TauType] -> [TauType] -> UnifyErrContext -> TcM ()

unifyTauTyLists tys1 tys2 err_ctxt = uList tys1 tys2 err_ctxt
\end{code}

@unifyTauTyList@ takes a single list of @TauType@s and unifies them
all together.  It is used, for example, when typechecking explicit
lists, when all the elts should be of the same type.

\begin{code}
unifyTauTyList :: [TauType] -> UnifyErrContext -> TcM ()

unifyTauTyList []   _ = returnTc ()
unifyTauTyList [ty] _ = returnTc ()

unifyTauTyList (ty1:tys@(ty2:_)) err_ctxt
  = unifyTauTy ty1 ty2 err_ctxt	`thenTc_`
    unifyTauTyList tys err_ctxt
\end{code}

%************************************************************************
%*									*
\subsection[Unify-lists-of-types]{@uList@}
%*									*
%************************************************************************

@uList@ unifies corresponding elements of two lists of @TauType@s.  It
uses @uTys@ to do the real work.  We charge down the list explicitly
so that we can complain if their lengths differ.

\begin{code}
uList :: [TauType] -> [TauType]
      -> UnifyErrContext
      -> TcM ()

uList [] [] _ = returnTc ()

uList (ty1:tys1) (ty2:tys2) err_ctxt
  = uTys ty1 ty1 ty2 ty2 err_ctxt   `thenTc_`
    uList tys1 tys2 err_ctxt

uList ty1s ty2s _ = panic "Unify.uList: mismatched type lists!"
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
uTys :: TauType -> TauType	-- Error reporting ty1 and real ty1
     -> TauType -> TauType	-- Error reporting ty2 and real ty2
     -> UnifyErrContext
     -> TcM ()
\end{code}

%********************************************************
%*							*
Sanity check: should never find a UniTyVarTemplate
%*							*
%********************************************************

\begin{code}
#ifdef DEBUG

uTys ps_ty1 ty1@(UniTyVarTemplate tv1) ps_ty2 ty2 err_ctxt
  = pprPanic "Unify:uTys:unifying w/ UniTyVarTemplate(1):" (ppCat [ppr PprDebug tv1, ppr PprDebug ty2])

uTys ps_ty1 ty1 ps_ty2 ty2@(UniTyVarTemplate tv2) err_ctxt
  = pprPanic "Unify:uTys:unifying w/ UniTyVarTemplate(2):" (ppCat [ppr PprDebug ty1, ppr PprDebug tv2])

#endif {-DEBUG-}
\end{code}

%********************************************************
%*							*
Both variables:
%*							*
%********************************************************

\begin{code}
uTys ps_ty1 (UniTyVar tyvar1) ps_ty2 ty2 err_ctxt = uVar tyvar1 ps_ty2 ty2 err_ctxt
uTys ps_ty1 ty1 ps_ty2 (UniTyVar tyvar2) err_ctxt = uVar tyvar2 ps_ty1 ty1 err_ctxt
\end{code}

%********************************************************
%*							*
Both function constructors:
%*							*
%********************************************************

\begin{code}
uTys _ (UniFun fun1 arg1) _ (UniFun fun2 arg2) err_ctxt
  = uList [fun1, arg1] [fun2, arg2] err_ctxt
\end{code}

%********************************************************
%*							*
Both datatype constructors:
%*							*
%********************************************************

\begin{code}
uTys ps_ty1 ty1@(UniData con1 args1) ps_ty2 ty2@(UniData con2 args2) err_ctxt
  = if (con1 == con2) then
	-- Same constructors, just unify the arguments
	uList args1 args2 err_ctxt
    else
	-- Different constructors: disaster
	getSrcLocTc		`thenNF_Tc` \ src_loc ->
	failTc (unifyErr (UnifyMisMatch ps_ty1 ps_ty2) err_ctxt src_loc)
\end{code}

%********************************************************
%*							*
Type synonyms:
%*							*
%********************************************************

If just one or the other is a synonym, just expand it.

\begin{code}
uTys ps_ty1 (UniSyn con1 args1 ty1) ps_ty2 ty2 err_ctxt
 | isVisibleSynTyCon con1
 = uTys ps_ty1 ty1 ps_ty2 ty2 err_ctxt

uTys ps_ty1 ty1 ps_ty2 (UniSyn con2 args2 ty2) err_ctxt
 | isVisibleSynTyCon con2
 = uTys ps_ty1 ty1 ps_ty2 ty2 err_ctxt
\end{code}

If you are tempted to make a short cut on synonyms, as in this
pseudocode...

\begin{verbatim}
uTys (UniSyn con1 args1 ty1) (UniSyn con2 args2 ty2)
  = if (con1 == con2) then
	-- Good news!  Same synonym constructors, so we can shortcut
	-- by unifying their arguments and ignoring their expansions.
	uList args1 args2
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

Still, if the synonym is abstract, we can only just go ahead and try!

\begin{code}
uTys ps_ty1 (UniSyn con1 args1 ty1) ps_ty2 (UniSyn con2 args2 ty2) err_ctxt
  -- Both must be abstract (i.e., non "visible" -- not done yet)
  = if (con1 == con2) then
	-- Good news!  Same synonym constructors, so we can shortcut
	-- by unifying their arguments and ignoring their expansions.
	uList args1 args2 err_ctxt
    else
	-- Bad news; mis-matched type constructors
	getSrcLocTc		`thenNF_Tc` \ src_loc ->
	failTc (unifyErr (UnifyMisMatch ps_ty1 ps_ty2) err_ctxt src_loc)
\end{code}

%********************************************************
%*							*
Catch-all case---just fails:
%*							*
%********************************************************

Anything else fails. For example, matching a @UniFun@ against
a @UniData@.
\begin{code}
uTys ps_ty1 ty1 ps_ty2 ty2 err_ctxt
  = getSrcLocTc		`thenNF_Tc` \ src_loc ->
    failTc (unifyErr (UnifyMisMatch ps_ty1 ps_ty2) err_ctxt src_loc)
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
uVar :: TyVar
     -> UniType -> UniType	-- printing and real versions
     -> UnifyErrContext
     -> TcM ()

uVar tv1 ps_ty2 ty2 err_ctxt
  = do tv1 ty2
  where
	-- Expand synonyms
    do _ (UniSyn _ _ ty2) = do tv1 ty2

	-- Commit any open type variable
    do (OpenSysTyVar _) ty2				    = tv1 `bindTo` ps_ty2
    do _ 	        ty2@(UniTyVar tv2@(OpenSysTyVar _)) = tv2 `bindTo` ty1

	-- Eliminate Poly in favour of User
    do (PolySysTyVar _) ty2@(UniTyVar (UserTyVar _ _))      = tv1 `bindTo` ps_ty2
    do (PolySysTyVar _) ty2@(UniTyVar (PolySysTyVar _))     = tv1 `bindTo` ps_ty2
    do (UserTyVar _ _)  ty2@(UniTyVar tv2@(PolySysTyVar _)) = tv2 `bindTo` ty1
    do (UserTyVar _ _)  ty2@(UniTyVar (UserTyVar _ _))      = tv1 `bindTo` ps_ty2

    	-- Matching for boxed data types
    do (PolySysTyVar _) ty2@(UniData con _) | isBoxedTyCon con  = tv1 `bindTo` ps_ty2
    do (UserTyVar _ _)  ty2@(UniData con _) | isBoxedTyCon con  = tv1 `bindTo` ps_ty2

	-- Matching for unboxed data types:
	--   requires specialisation w.r.t. the unboxed type
    do (PolySysTyVar _) ty2@(UniData con _)  = tv1 `bindToUnboxed` ps_ty2
    do (UserTyVar _ _)  ty2@(UniData con _)  = tv1 `bindToUnboxed` ps_ty2

	-- Matching for function types
    do (PolySysTyVar _) ty2@(UniFun _ _)     = tv1 `bindTo` ps_ty2
    do (UserTyVar _ _)  ty2@(UniFun _ _)     = tv1 `bindTo` ps_ty2

	-- Default
    do _ _ = getSrcLocTc `thenNF_Tc` \ src_loc ->
             failTc (unifyErr (UnifyMisMatch ty1 ps_ty2) err_ctxt src_loc)

	----------- END OF CASES ---------------

    ty1 = UniTyVar tv1

    tyvar1 `bindTo` ty2 
	= extendSubstTc tyvar1 ty2 err_ctxt

    tyvar1 `bindToUnboxed` ty2 
	= getSwitchCheckerTc 	`thenNF_Tc` \ sw_chkr ->
	  if sw_chkr SpecialiseUnboxed && isUnboxedButNotState ty2 then
	      extendSubstTc tyvar1 ty2 err_ctxt
	  else
	      getSrcLocTc 	`thenNF_Tc` \ src_loc ->
              failTc (unifyErr (UnifyUnboxedMisMatch ty1 ps_ty2) err_ctxt src_loc)
\end{code}
