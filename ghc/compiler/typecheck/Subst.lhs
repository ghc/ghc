%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Subst]{Substitutions}

\begin{code}
#include "HsVersions.h"

module Subst (
	Subst, SubstResult(..),	-- Subst is an abstract data type

	mkEmptySubst, extendSubst,

--not exported:	applySubstToTauTy,
	applySubstToTy,
	applySubstToThetaTy, applySubstToTyVar, 

	getSubstTyVarUniques, getSubstTyVarUnique,

	pushSubstUndos, combineSubstUndos, undoSubstUndos,
	-- pruneSubst,

	-- and to make the interface self-sufficient...
	TyVar, UniType
    ) where

import AbsUniType	-- lots of stuff, plus...
import UniType		-- UniType(..) -- *********** YOW!!! ********
import Bag		( emptyBag, unionBags, snocBag, 
			  bagToList, filterBag, unitBag, Bag )
import Maybes		( Maybe(..), maybeToBool )
import Outputable
import Unique
import Util
\end{code}

%************************************************************************
%*									*
\subsection[Subst-magic-importst]{Funny imports to support magic implementation}
%*									*
%************************************************************************

Or lack thereof.

If we are compiling with Glasgow Haskell we can use mutable
arrays to implement the substitution ...

\begin{code}
#ifndef __GLASGOW_HASKELL__

import LiftMonad

#else {- __GLASGOW_HASKELL__ -}

import PreludeGlaST

type STWorld = _State _RealWorld

newWorld (S# real_world) = S# real_world

#endif {- __GLASGOW_HASKELL__ -}
\end{code}

%************************************************************************
%*									*
\subsection[Subst-common]{@Subst@: common implementation-independent bits}
%*									*
%************************************************************************

\begin{code}
data SubstResult
  = SubstOK		
  | OccursCheck	    TyVar
		    TauType
  | AlreadyBound    TauType -- The variable is already bound
			    -- to this type.  The type is *not*
			    -- necessarily a fixed pt of the 
			    -- substitution
\end{code}

Common signatures of major functions.

\begin{code}
mkEmptySubst :: Int -> Subst
\end{code}

%---------

@extendSubst@: Add a single binding to the substitution. We have to:
\begin{itemize}
\item
apply the existing bindings to the new one;
\item
check whether we are adding a trivial substitution of a type
variable to itself (if so, do nothing);
\item
perform an occurs check on the right-hand side of the new binding;
\end{itemize}
We do not apply the new binding to all the existing ones. This is 
delayed until the substitution is applied.
\begin{code}
extendSubst :: TyVar 		-- Tyvar to bind
	    -> TauType 		-- Type to bind it to; NB can be a synonym
	    -> SubstM SubstResult
\end{code}

%---------

Apply a substitution to a given type.  

	{\em The type returned is guaranteed to be 
	a fixed point of the substitution.}

Hence, we have to traverse the type determining the type mapped to
tyvars. The type mapped must be recusively traversed as the substition
is not stored idempotently.

@applySubstToTauTy@ does not expect to meet a dict or forall type.
@applySubstToTy@ may encounter these, but complains if the forall
binds a variable which is in the domain of the substitution.

\begin{code}
applySubstToTy	    :: Subst -> UniType   -> (Subst, UniType)
applySubstToTauTy   :: Subst -> TauType   -> (Subst, TauType)
applySubstToThetaTy :: Subst -> ThetaType -> (Subst, ThetaType)
applySubstToTyVar   :: Subst -> TyVar     -> (Subst, TauType)
\end{code}

These functions are only used by the type checker.  We know that 
all the for-all'd type variables are fixed points of the substitution,
so it's quite safe just to apply the substitution inside foralls.

%---------

Sorta obvious.
\begin{code}
getSubstTyVarUnique  :: Subst -> (Subst, Unique)
getSubstTyVarUniques :: Int -> Subst -> (Subst, [Unique])
\end{code}

%---------

@pushSubstUndos@ starts a new subst undo scope, saving the old scopes.
It also saves the current unique supply so that it can be restored if
the typecheck fails.

@combineSubstUndos@ is called after a successful typecheck. It
combines the current undos with the previos ones in case we fail in an
outer scope. If no previous undos exist the undos are thrown away as
we must have succeeded at the top level. The unique supply of the
successful scope is returned to the unique supply of the current
scope.

@undoSubstUndos@ is called when a typecheck failed. The any
substitution modifications are undone and the undo information
discarded. The saved unique supply of the enclosing scope is restored.
\begin{code}
pushSubstUndos, combineSubstUndos, undoSubstUndos :: Subst -> Subst
\end{code}

%************************************************************************
%*									*
\subsection[Subst-Arrays]{@Subst@ with mutable @Arrays@ !!!}
%*									*
%************************************************************************

Depends on....
\begin{code}
#ifdef __GLASGOW_HASKELL__
\end{code}

%************************************************************************
%*									*
\subsubsection{@Subst@: specification and representation}
%*									*
%************************************************************************

{\em Specification:}
* When new bindings are added to the substitution, an occurs check is performed.
* The applySubst function guarantees to return a fixed point of the substitution.

{\em Representation:}
A substitution binds type variables to tau-types, that is @UniType@s without
any @UniForall@ or @UniDict@ constructors.

It is represented as an array, indexed on Int, with a world
token, and a stack of type variables whos subst may be undone. The
array is extended (by copying) if it overflows. The supply of
Ints and the size of the array are linked so the substitution
is also responsible for allocating the supply of uniques.

The undo information is a stack of bags of the nested modifications to
the substitution. If the typecheck fails the modifications to the
substition are undone. If it succeeds the current undos are combined
with the undos in the enclosing scope so that they would be undone if
the enclsing scope typecheck fails.

The unique supply is also stacked so that it can be restored if a
typecheck fails.

NOTE: The uniqueness of the world token, and hence the substitution,
is critical as the 'worldSEQ' operation is unsafe if the token can be
duplicated!!!

\begin{code}
type SubstArray = _MutableArray _RealWorld Int (Maybe TauType)

type SubstArrayIndex = Int	-- Allocated within this module, single-threadedly

data Subst
  = MkSubst SubstArray		-- Mapping for allocated tyvars

	    [(SubstArrayIndex, Bag (SubstArrayIndex, Maybe TauType))]
				-- Stack to be undone if we fail, plus next free
				-- slot when reverting.  All the undos are for
				-- slots earlier than the corresp "next free" index.
				--
				-- The "bag" is a lie: it's really a sequence, with
				-- the most recently performed write appearing first.

	    STWorld 		-- State token

	    SubstArrayIndex	-- Next free slot
\end{code}

Here's a local monad for threading the substitution around:

\begin{code}
type SubstM a = Subst -> (Subst,a)

returnSubstM x = \s -> (s,x)
thenSubstM m k = \s -> case m s of { (s1, r) -> k r s1 }

mapSubstM f []     = returnSubstM []
mapSubstM f (x:xs) = f x		`thenSubstM` \ r ->
		     mapSubstM f xs	`thenSubstM` \ rs ->
		     returnSubstM (r:rs)

-- Breaks the ST abstraction.  But we have to do so somewhere...
doST :: STWorld -> ST _RealWorld a -> (a, STWorld)
doST w st = st w
\end{code}

%********************************************************
%*							*
\subsubsection{@Subst@: the array}
%*							*
%********************************************************

\begin{code}
writeSubst  :: SubstArrayIndex -> Maybe TauType -> SubstM ()
	-- writeSubst writes in such a way that we can undo it later

writeSubst index new_val 
	   (MkSubst arr undo_stack@((checkpoint, undos):rest_undo_stack) 
		    world next_free)
  | index < checkpoint	-- Record in undos
  = let
	(old, new_world) = doST world (
			  readArray arr index		`thenStrictlyST` \ old_val ->
			  writeArray arr index new_val	`seqStrictlyST`
			  returnStrictlyST old_val
			)
	new_undos = unitBag (index,old) `unionBags` undos
			-- The order is significant!  The right most thing
			-- gets undone last
    in
    (MkSubst arr ((checkpoint, new_undos) : rest_undo_stack) new_world next_free, ())

writeSubst index new_val (MkSubst arr undo_stack world next_free)
  -- No need to record in undos: undo_stack is empty,
  -- or index is after checkpoint
  = let
	(_, new_world) = doST world (writeArray arr index new_val)
    in
    (MkSubst arr undo_stack new_world next_free, ())

readSubst  :: SubstArrayIndex -> SubstM (Maybe TauType)
readSubst index (MkSubst arr undos world supplies)
  = let
	(result, new_world) = doST world (readArray arr index)
    in
    (MkSubst arr undos new_world supplies, result)

tyVarToIndex :: TyVar -> SubstArrayIndex
tyVarToIndex tyvar = unpkUnifiableTyVarUnique (getTheUnique tyvar)
\end{code}

%********************************************************
%*							*
\subsubsection{@Subst@: building them}
%*							*
%********************************************************

The function @mkEmptySubst@ used to be a CAF containing a mutable
array.  The imperative world had a name for this kind of thing:
``global variable'' and has observed that using these ``global variables''
leads to something they call ``side effects''.

These ``side effects'' never caused a problem for @hsc@ because empty
substitutions are only used in one place (the typechecker) and only
used once in every program run.  In \tr{ghci} however, we might use the
typechecker several times---in which case we'd like to have a
different (fresh) substitution each time.  The easy way (HACK) to
achieve this is to deCAFinate so that a fresh substitution will be
created each time the typechecker runs.

\begin{code}
aRRAY_START :: Int
aRRAY_START = 0

mkEmptySubst aRRAY_SIZE
  = let
      	world = newWorld (S# realWorld#)
	(arr, new_world) = doST world (newArray (aRRAY_START,aRRAY_SIZE) Nothing)
    in
    MkSubst arr [] new_world aRRAY_START

extendSubstArr :: Subst
	       -> Subst
extendSubstArr (MkSubst old_arr undos world next_free)
  = let
	-- these "sizes" are really end-limits (WDP 94/11)
	cur_size  = case (boundsOfArray old_arr) of { (_, x) -> x }
        new_size = (cur_size * 2) + 1

	(new_arr, new_world) = doST world (
				newArray (aRRAY_START,new_size) Nothing	`thenStrictlyST` \ new_arr ->
				let
				    copyArr pos
					| pos > cur_size = returnStrictlyST ()
					| otherwise
					  = readArray  old_arr pos	`thenStrictlyST`  \ ele ->
					    writeArray new_arr pos ele	`seqStrictlyST`
					    copyArr (pos + 1)
				in
				copyArr aRRAY_START		`seqStrictlyST`
				returnStrictlyST new_arr
			    )
    in
    MkSubst new_arr undos new_world next_free
\end{code}

\begin{code}
extendSubst tyvar tau_ty
  = readSubst index		`thenSubstM` \ maybe_ty ->

    case maybe_ty of
       Just exist_ty ->	-- Bound already
		returnSubstM (AlreadyBound exist_ty)

       Nothing       ->	-- Not already bound
	  apply_rep_to_ty tau_ty `thenSubstM` \ new_tau_ty ->
	  case expandVisibleTySyn new_tau_ty of
		UniTyVar tv | tv `eqTyVar` tyvar ->
		       	-- Trivial new binding of a type variable to itself; 
			-- return old substition
		       returnSubstM SubstOK

		other | tyvar `is_elem`  (extractTyVarsFromTy new_tau_ty) ->
			-- Occurs check finds error
			returnSubstM (OccursCheck tyvar new_tau_ty)

		      | otherwise -> 
			-- OK to bind 
			writeSubst index (Just new_tau_ty) `thenSubstM` \ _ ->
			returnSubstM SubstOK
  where
      index   = tyVarToIndex tyvar
      is_elem = isIn "extendSubst"
\end{code}

%********************************************************
%*							*
\subsubsection{@Subst@: lookup}
%*							*
%********************************************************

All of them use the underlying function, @apply_rep_to_ty@, which
ensures that an idempotent result is returned.

\begin{code}
applySubstToTy      subst ty       = apply_rep_to_ty ty subst
applySubstToTauTy   subst tau_ty   = apply_rep_to_ty tau_ty subst
applySubstToTyVar   subst tyvar    = apply_rep_to_ty (mkTyVarTy tyvar) subst
applySubstToThetaTy subst theta_ty 
  = let
      do_one (clas, ty) = apply_rep_to_ty ty  `thenSubstM` \ new_ty ->
			  returnSubstM (clas, new_ty)
    in
    mapSubstM do_one theta_ty subst
\end{code}

And now down to serious business...
\begin{code}  
apply_rep_to_ty :: UniType -> SubstM UniType

apply_rep_to_ty (UniTyVar tyvar)
  = readSubst index		`thenSubstM` \ maybe_ty ->
    case maybe_ty of

      Nothing -> -- Not found, so return a trivial type
		 returnSubstM (mkTyVarTy tyvar)

      Just ty -> -- Found, so recursively apply the subst the result to
		 -- maintain idempotence!
		 apply_rep_to_ty ty		`thenSubstM` \ new_ty ->

		 -- The mapping for this tyvar is then updated with the
		 -- result to reduce the number of subsequent lookups
		 writeSubst index (Just new_ty)	`thenSubstM` \ _ ->

		 returnSubstM new_ty
  where
    index = tyVarToIndex tyvar

apply_rep_to_ty (UniFun t1 t2)
  = apply_rep_to_ty t1		`thenSubstM` \ new_t1 ->
    apply_rep_to_ty t2		`thenSubstM` \ new_t2 ->
    returnSubstM (UniFun new_t1 new_t2)

apply_rep_to_ty (UniData con args)
  = mapSubstM apply_rep_to_ty args	`thenSubstM` \ new_args ->
    returnSubstM (UniData con new_args)

apply_rep_to_ty (UniSyn con args ty)
  = mapSubstM apply_rep_to_ty args 	`thenSubstM` \ new_args ->
    apply_rep_to_ty ty			`thenSubstM` \ new_ty ->
    returnSubstM (UniSyn con new_args new_ty)

apply_rep_to_ty (UniDict clas ty)
  = apply_rep_to_ty ty		`thenSubstM` \ new_ty ->
    returnSubstM (UniDict clas new_ty)

apply_rep_to_ty (UniForall v ty)
  = apply_rep_to_ty ty		`thenSubstM` \ new_ty ->
    returnSubstM (UniForall v new_ty)

apply_rep_to_ty ty@(UniTyVarTemplate v) = returnSubstM ty
\end{code}

%************************************************************************
%*									*
\subsubsection{Allocating @TyVarUniques@}
%*									*
%************************************************************************

The array is extended if the allocated type variables would cause an
out of bounds error.

\begin{code}
getSubstTyVarUnique subst@(MkSubst arr undo world next_free)
  | next_free <= size	-- The common case; there's a spare slot
  = (MkSubst arr undo world new_next_free, uniq)

  | otherwise		-- Need more room: Extend first, then re-try
  = getSubstTyVarUnique (extendSubstArr subst)

  where
    size       = case (boundsOfArray arr) of { (_, x) -> x  }
    uniq       = mkUnifiableTyVarUnique next_free
    new_next_free = next_free + 1
    

getSubstTyVarUniques n subst@(MkSubst arr undo world next_free)
  | new_next_free - 1 <= size	-- The common case; there's a spare slot
  = (MkSubst arr undo world new_next_free, uniqs)

  | otherwise		-- Need more room: extend, then re-try
  = getSubstTyVarUniques n (extendSubstArr subst)

  where
    size    	= case (boundsOfArray arr) of { (_, x) -> x  }
    uniqs	= [mkUnifiableTyVarUnique (next_free + i) | i <- [0..n-1]]
    new_next_free  = next_free + n
\end{code}

%************************************************************************
%*									*
\subsubsection{Undoing substitution on typechecking failure}
%*									*
%************************************************************************

\begin{code}
pushSubstUndos (MkSubst arr undos world next_free)
  = MkSubst arr ((next_free,emptyBag):undos) world next_free

combineSubstUndos (MkSubst arr [_] world next_free)
  = MkSubst arr [] world next_free  -- top level undo ignored

combineSubstUndos (MkSubst arr ((_,u1):(checkpoint,u2):undo_stack) 
			   world next_free)
  = MkSubst arr ((checkpoint, new_u1 `unionBags` u2):undo_stack) world next_free
  where
	-- Keep only undos which apply to indices before checkpoint
    new_u1 = filterBag (\ (index,val) -> index < checkpoint) u1

undoSubstUndos (MkSubst arr ((checkpoint,undo_now):undo_stack) world next_free)
  = MkSubst arr undo_stack new_world checkpoint
  where
    (_, new_world) = doST world (perform_undo (bagToList undo_now) `seqStrictlyST`
			      clear_block checkpoint
			     )

    perform_undo []		     = returnStrictlyST ()
    perform_undo ((index,val):undos) = writeArray arr index val `seqStrictlyST`
				       perform_undo undos

	-- (clear_block n) clears the array from n up to next_free
	-- This is necessary because undos beyond supp2 aren't recorded in undos
    clear_block n | n >= next_free = returnStrictlyST ()
		  | otherwise      = writeArray arr n Nothing `seqStrictlyST`
				     clear_block (n+1)
\end{code}

%************************************************************************
%*									*
\subsubsection{Pruning a substitution}
%*									*
%************************************************************************

ToDo: Implement with array !!  Ignore?  Restore unique supply?

@pruneSubst@ prunes a substitution to a given level.

This is tricky stuff.  The idea is that if we
    (a) catch the current unique supply
    (b) do some work
    (c) back-substitute over the results of the work
    (d) prune the substitution back to the level caught in (a)
then everything will be fine.  Any *subsequent* unifications to
these just-pruned ones will be added and not subsequently deleted.

NB: this code relies on the idempotence property, otherwise discarding
substitions might be dangerous.

\begin{code} 
{-
pruneSubst :: TyVarUnique -> Subst -> Subst

pruneSubst keep_marker (MkSubst subst_rep) 
  = -- BSCC("pruneSubst")
    MkSubst [(tyvar,ty) | (tyvar,ty) <- subst_rep, 
	     getTheUnique tyvar `ltUnique` keep_marker]
    -- ESCC
-}
\end{code}

%************************************************************************
%*									*
\subsection[Subst-Lists]{@Subst@ with poor list implementation}
%*									*
%************************************************************************

If don't have Glasgow Haskell we have to revert to list implementation
of arrays ...

\begin{code}
#else {- ! __GLASGOW_HASKELL__ -}
\end{code}

%************************************************************************
%*									*
\subsubsection{@Subst@: specification and representation}
%*									*
%************************************************************************

{\em Specification:}
* When new bindings are added to the substitution, an occurs check is performed.
* The applySubst function guarantees to return a fixed point of the substitution.

{\em Representation:}
A substitution binds type variables to tau-types, that is @UniType@s without
any @UniForall@ or @UniDict@ constructors.

It is represented as an association list, indexed on Uniques
with a stack of type variable unique markers indicating undo
checkpoints.  The supply of TyVarUniques is also part of the
aubstitution.

The undo information is a stack of tyvar markers. If the typecheck
fails all extensions to the association list subsequent to (and
including) the marker are undone. If it succeeds the current marker is
discarded.

The unique supply is also stacked so that it can be restored if a
typecheck fails.

\begin{code}
type SubstRep = [(Unique, TauType)]

data Subst
  = MkSubst SubstRep		-- mapping for allocated tyvars
	    [Maybe Unique]	-- stack of markers to strip off if we fail
	    [UniqueSupply]	-- stack of tyvar unique supplies

mkEmptySubst size = MkSubst [] [] []
\end{code}

\begin{code}
lookup_rep :: SubstRep -> TyVar -> Maybe TauType
lookup_rep alist tyvar
  = let
        key = getTheUnique tyvar

        lookup []	     = Nothing
        lookup ((u,ty):rest)
	  = case (cmpUnique key u) of { EQ_ -> Just ty; _ -> lookup rest }
    in
    lookup alist
\end{code}

%********************************************************
%*							*
\subsubsection{@Subst@: building them}
%*							*
%********************************************************

\begin{code}
--OLD? initSubst init = MkSubst [] [] [mkUniqueSupply init]
\end{code}

\begin{code}
extendSubst subst@(MkSubst srep undo supp) tyvar tau_ty
  = -- BSCC("extendSubst")
    apply_rep_to_ty srep tau_ty `thenLft` \ new_tau_ty ->

    case expandVisibleTySyn new_tau_ty of

	UniTyVar tv | tv `eqTyVar` tyvar ->
	     -- Trivial new binding; return old substition
	     (SubstOK, subst)

	_ -> let
		is_elem = isIn "extendSubst2"
	     in
	     if (tyvar `is_elem` (extractTyVarsFromTy new_tau_ty)) then
	   	 (OccursCheck tyvar new_tau_ty, subst)
	     else
		 case lookup_rep srep tyvar of 
		     Just exist_ty ->
		         (AlreadyBound exist_ty, subst)
		     Nothing       ->
			 let
			   new_srep = (getTheUnique tyvar, new_tau_ty) : srep
			   new_undo = case undo of
				      [] 		-> [] 
					  -- top level undo ignored

				      (Nothing : undos)	-> (Just (getTheUnique tyvar)) : undos
				      (Just _ : _ ) 	-> undo
					  -- only first undo recorded
			 in
		         (SubstOK, MkSubst new_srep new_undo supp)
    -- ESCC
\end{code}

%********************************************************
%*							*
\subsubsection{@Subst@: lookup}
%*							*
%********************************************************

All of them use the underlying function, @apply_rep_to_ty@, which
ensures that an idempotent result is returned.

\begin{code}
applySubstToTy subst@(MkSubst srep undo supp) ty
  = -- BSCC("applySubstToTy")
    apply_rep_to_ty srep ty 		`thenLft` \ new_ty ->
    (subst, new_ty)
    -- ESCC

applySubstToTauTy subst@(MkSubst srep undo supp) tauty
  = -- BSCC("applySubstToTauTy")
    apply_rep_to_ty srep tauty 		`thenLft`\ new_tauty ->
	(subst, new_tauty)
    -- ESCC

applySubstToThetaTy subst@(MkSubst srep undo supp) theta
  = -- BSCC("applySubstToThetaTy")
    let
        do_one (clas, ty) = apply_rep_to_ty srep ty  `thenLft` \ new_ty ->
			    returnLft (clas, new_ty)
    in
    mapLft do_one theta		`thenLft` \ new_theta ->
    (subst, new_theta)
    -- ESCC

applySubstToTyVar subst@(MkSubst srep undo supp) tyvar
  = -- BSCC("applySubstToTyVar")
    apply_rep_to_ty srep (mkTyVarTy tyvar) `thenLft` \ new_tauty ->
    (subst, new_tauty)
    -- ESCC
\end{code}

And now down to serious business...
\begin{code}  
apply_rep_to_ty :: SubstRep -> UniType -> LiftM UniType

apply_rep_to_ty srep (UniTyVar tyvar)
  = case lookup_rep srep tyvar of
      Nothing -> -- Not found, so return a trivial type
		 returnLft (mkTyVarTy tyvar)

      Just ty -> -- Found, so recursively apply the subst the result to
		 -- maintain idempotence!
		 apply_rep_to_ty srep ty

apply_rep_to_ty srep (UniFun t1 t2)
  = apply_rep_to_ty srep t1		`thenLft` \ new_t1 ->
    apply_rep_to_ty srep t2		`thenLft` \ new_t2 ->
    returnLft (UniFun new_t1 new_t2)

apply_rep_to_ty srep (UniData con args)
  = mapLft (apply_rep_to_ty srep) args	`thenLft` \ new_args ->
    returnLft (UniData con new_args)

apply_rep_to_ty srep (UniSyn con args ty)
  = mapLft (apply_rep_to_ty srep) args 	`thenLft` \ new_args ->
    apply_rep_to_ty srep ty		`thenLft` \ new_ty ->
    returnLft (UniSyn con new_args new_ty)

apply_rep_to_ty srep (UniDict clas ty)
  = apply_rep_to_ty srep ty		`thenLft` \ new_ty ->
    returnLft (UniDict clas new_ty)

apply_rep_to_ty srep (UniForall v ty)
  = apply_rep_to_ty srep ty		`thenLft` \ new_ty ->
    returnLft (UniForall v new_ty)

apply_rep_to_ty srep ty@(UniTyVarTemplate v) = 	returnLft ty
\end{code}

%************************************************************************
%*									*
\subsubsection{Allocating TyVarUniques}
%*									*
%************************************************************************

The array is extended if the allocated type variables would cause an
out of bounds error.

\begin{code}
getSubstTyVarUnique subst@(MkSubst srep undo (supp:supps))
  = -- BSCC("allocTyVarUniques")
    case getUnique supp of
      (new_supp, uniq) -> (MkSubst srep undo (new_supp:supps), uniq)
    -- ESCC

getSubstTyVarUniques n subst@(MkSubst srep undo (supp:supps))
  = -- BSCC("allocTyVarUniques")
    case getUniques n supp of
      (new_supp, uniqs) -> (MkSubst srep undo (new_supp:supps), uniqs)
    -- ESCC
\end{code}

%************************************************************************
%*									*
\subsubsection[Subst-undo]{Undoing substitution on typechecking failure}
%*									*
%************************************************************************

\begin{code}
pushSubstUndos subst@(MkSubst srep undos (supp:supps))
  = -- BSCC("pushSubstUndos")
    MkSubst srep (Nothing:undos) (supp:supp:supps)
    -- ESCC

combineSubstUndos subst@(MkSubst srep (u:us) (supp1:supp2:supps))
  = -- BSCC("combineSubstUndos")
      MkSubst srep us (supp1:supps)
    -- ESCC

undoSubstUndos subst@(MkSubst srep (u:us) (supp1:supp2:supps))
  = -- BSCC("undoSubstUndos")
    let 
      strip_to []	     key = []
      strip_to ((u,ty):srep) key
	= case (cmpUnique u key) of { EQ_ -> srep; _ -> strip_to srep key }
      
      perform_undo Nothing     srep = srep
      perform_undo (Just uniq) srep = strip_to srep uniq 
    in
      MkSubst (perform_undo u srep) us (supp2:supps)

	-- Note: the saved unique supply is restored from the enclosing scope

    -- ESCC
\end{code}

%************************************************************************
%*									*
\subsubsection{Pruning a substitution}
%*									*
%************************************************************************

ToDo: Implement with list !!  Ignore?  Restore unique supply?

@pruneSubst@ prunes a substitution to a given level.

This is tricky stuff.  The idea is that if we
    (a) catch the current unique supply
    (b) do some work
    (c) back-substitute over the results of the work
    (d) prune the substitution back to the level caught in (a)
then everything will be fine.  Any *subsequent* unifications to
these just-pruned ones will be added and not subsequently deleted.

NB: this code relies on the idempotence property, otherwise discarding
substitions might be dangerous.

\begin{code} 
{-
pruneSubst :: TyVarUnique -> Subst -> Subst

pruneSubst keep_marker (MkSubst subst_rep) 
  = -- BSCC("pruneSubst")
    MkSubst [(tyvar,ty) | (tyvar,ty) <- subst_rep, 
	     getTheUnique tyvar `ltUnique` keep_marker]
    -- ESCC
-}
\end{code}

\begin{code}
#endif {- ! __GLASGOW_HASKELL__ -}
\end{code}
