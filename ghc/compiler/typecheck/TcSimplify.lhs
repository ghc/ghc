%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcSimplify]{TcSimplify}

Notes:

Inference (local definitions)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the inst constrains a local type variable, then
  [ReduceMe] if it's a literal or method inst, reduce it

  [DontReduce] otherwise see whether the inst is just a constant
    if succeed, use it
    if not, add original to context
  This check gets rid of constant dictionaries without
  losing sharing.

If the inst does not constrain a local type variable then
  [Free] then throw it out as free.

Inference (top level definitions)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the inst does not constrain a local type variable, then
  [FreeIfTautological] try for tautology; 
      if so, throw it out as free
	 (discarding result of tautology check)
      if not, make original inst part of the context 
	 (eliminating superclasses as usual)

If the inst constrains a local type variable, then
   as for inference (local defns)


Checking (local defns)
~~~~~~~~
If the inst constrains a local type variable then 
  [ReduceMe] reduce (signal error on failure)

If the inst does not constrain a local type variable then
  [Free] throw it out as free.

Checking (top level)
~~~~~~~~~~~~~~~~~~~~
If the inst constrains a local type variable then
   as for checking (local defns)

If the inst does not constrain a local type variable then
   as for checking (local defns)



Checking once per module
~~~~~~~~~~~~~~~~~~~~~~~~~
For dicts of the form (C a), where C is a std class
  and "a" is a type variable,
  [DontReduce] add to context

otherwise [ReduceMe] always reduce

[NB: we may generate one Tree [Int] dict per module, so 
     sharing is not complete.]

Sort out ambiguity at the end.

Principal types
~~~~~~~~~~~~~~~
class C a where
  op :: a -> a

f x = let g y = op (y::Int) in True

Here the principal type of f is (forall a. a->a)
but we'll produce the non-principal type
    f :: forall a. C Int => a -> a


Ambiguity
~~~~~~~~~
Consider this:

	instance C (T a) Int  where ...
	instance C (T a) Bool where ...

and suppose we infer a context

	    C (T x) y

from some expression, where x and y are type varibles,
and x is ambiguous, and y is being quantified over.
Should we complain, or should we generate the type

       forall x y. C (T x) y => <type not involving x>

The idea is that at the call of the function we might
know that y is Int (say), so the "x" isn't really ambiguous.
Notice that we have to add "x" to the type variables over
which we generalise.

Something similar can happen even if C constrains only ambiguous
variables.  Suppose we infer the context 

       C [x]

where x is ambiguous.  Then we could infer the type

       forall x. C [x] => <type not involving x>

in the hope that at the call site there was an instance
decl such as

       instance Num a => C [a] where ...

and hence the default mechanism would resolve the "a".


\begin{code}
module TcSimplify (
	tcSimplify, tcSimplifyAndCheck,
	tcSimplifyTop, tcSimplifyThetas, tcSimplifyCheckThetas,
	bindInstsOfLocalFuns
    ) where

#include "HsVersions.h"

import HsSyn		( MonoBinds(..), HsExpr(..), andMonoBinds )
import TcHsSyn		( TcExpr, TcIdOcc(..), TcIdBndr, 
			  TcMonoBinds, TcDictBinds
			)

import TcMonad
import Inst		( lookupInst, lookupSimpleInst, LookupInstResult(..),
			  tyVarsOfInst, 
			  isTyVarDict, isDict, isStdClassTyVarDict, isMethodFor,
			  instToId, instBindingRequired, instCanBeGeneralised,
			  newDictFromOld,
			  instLoc, getDictClassTys,
			  pprInst, zonkInst,
			  Inst(..), LIE, pprInsts, pprInstsInFull, mkLIE, 
			  InstOrigin(..), pprOrigin
			)
import TcEnv		( TcIdOcc(..), tcGetGlobalTyVars )
import TcType		( TcType, TcTyVar, TcTyVarSet, TcMaybe, tcInstType, tcInstTheta )
import Unify		( unifyTauTy )
import Id		( mkIdSet )

import Bag		( Bag, unitBag, listToBag, foldBag, filterBag, emptyBag, bagToList, 
			  snocBag, consBag, unionBags, isEmptyBag )
import Class		( Class, ClassInstEnv, classBigSig, classInstEnv )
import PrelInfo		( isNumericClass, isCcallishClass )

import Maybes		( expectJust, firstJust, catMaybes, seqMaybe, maybeToBool )
import Type		( Type, ThetaType, TauType, mkTyVarTy, getTyVar,
			  isTyVarTy, getTyVar_maybe, instantiateThetaTy
			)
import PprType		( pprConstraint )
import TysWiredIn	( intTy, unitTy )
import TyVar		( elementOfTyVarSet, emptyTyVarSet, unionTyVarSets,
			  intersectTyVarSets, unionManyTyVarSets,
			  isEmptyTyVarSet, tyVarSetToList, 
			  zipTyVarEnv, emptyTyVarEnv
			)
import FiniteMap
import BasicTypes	( TopLevelFlag(..) )
import Unique		( Unique )
import Outputable
import Util
import List		( partition )
\end{code}


%************************************************************************
%*									*
\subsection[tcSimplify-main]{Main entry function}
%*									*
%************************************************************************

The main wrapper is @tcSimplify@.  It just calls @tcSimpl@, but with
the ``don't-squash-consts'' flag set depending on top-level ness.  For
top level defns we *do* squash constants, so that they stay local to a
single defn.  This makes things which are inlined more likely to be
exportable, because their constants are "inside".  Later passes will
float them out if poss, after inlinings are sorted out.

\begin{code}
tcSimplify
	:: SDoc 
	-> TopLevelFlag
	-> TcTyVarSet s			-- ``Local''  type variables
	-> LIE s			-- Wanted
	-> TcM s (LIE s,			-- Free
		  TcDictBinds s,		-- Bindings
		  LIE s)			-- Remaining wanteds; no dups

tcSimplify str top_lvl local_tvs wanteds
  = tcSimpl str top_lvl local_tvs Nothing wanteds
\end{code}

@tcSimplifyAndCheck@ is similar to the above, except that it checks
that there is an empty wanted-set at the end.  It may still return
some of constant insts, which have to be resolved finally at the end.

\begin{code}
tcSimplifyAndCheck
	 :: SDoc 
	 -> TcTyVarSet s		-- ``Local''  type variables; ASSERT is fixpoint
	 -> LIE s			-- Given
	 -> LIE s			-- Wanted
	 -> TcM s (LIE s,		-- Free
		   TcDictBinds s)	-- Bindings

tcSimplifyAndCheck str local_tvs givens wanteds
  = tcSimpl str top_lvl local_tvs (Just givens) wanteds	`thenTc` \ (free_insts, binds, new_wanteds) ->
    ASSERT( isEmptyBag new_wanteds )
    returnTc (free_insts, binds)
  where
    top_lvl = error "tcSimplifyAndCheck"	-- Never needed
\end{code}

\begin{code}
tcSimpl :: SDoc
	-> TopLevelFlag
	-> TcTyVarSet s			-- ``Local''  type variables
					-- ASSERT: this tyvar set is already zonked
	-> Maybe (LIE s)		-- Given; these constrain only local tyvars
					--	  Nothing => just simplify
					--	  Just g  => check that g entails wanteds
	-> LIE s			-- Wanted
	-> TcM s (LIE s,			-- Free
		  TcMonoBinds s,		-- Bindings
		  LIE s)			-- Remaining wanteds; no dups

tcSimpl str top_lvl local_tvs maybe_given_lie wanted_lie
  =	-- ASSSERT: local_tvs are already zonked
    reduceContext str try_me 
		  givens 
		  (bagToList wanted_lie)	`thenTc` \ (binds, frees, irreds) ->

	-- Check for non-generalisable insts
    let
	cant_generalise = filter (not . instCanBeGeneralised) irreds
    in
    checkTc (null cant_generalise)
	    (genCantGenErr cant_generalise)	`thenTc_`

	 -- Finished
    returnTc (mkLIE frees, binds, mkLIE irreds)
  where
    givens = case maybe_given_lie of
	   	  Just given_lie -> bagToList given_lie
		  Nothing        -> []

    checking_against_signature = maybeToBool maybe_given_lie
    is_top_level = case top_lvl of { TopLevel -> True; other -> False }

    try_me inst 
      -- Does not constrain a local tyvar
      | isEmptyTyVarSet (inst_tyvars `intersectTyVarSets` local_tvs)
      = -- if not checking_against_signature && is_top_level then
	--   FreeIfTautological		  -- Special case for inference on 
	--				  -- top-level defns
	-- else
	   
	Free

      -- When checking against a given signature we always reduce
      -- until we find a match against something given, or can't reduce
      |  checking_against_signature
      = ReduceMe CarryOn

      -- So we're infering (not checking) the type, and 
      -- the inst constrains a local type variable
      | otherwise
      = if isDict inst then 
	   DontReduce	    -- Dicts
	else
	   ReduceMe CarryOn    -- Lits and Methods

      where
        inst_tyvars     = tyVarsOfInst inst
\end{code}



%************************************************************************
%*									*
\subsection{Data types for the reduction mechanism}
%*									*
%************************************************************************

The main control over context reduction is here

\begin{code}
data WhatToDo 
 = ReduceMe		  -- Reduce this
	NoInstanceAction  -- What to do if there's no such instance

 | DontReduce		  -- Return as irreducible

 | Free			  -- Return as free

 | FreeIfTautological	  -- Return as free iff it's tautological; 
			  -- if not, return as irreducible

data NoInstanceAction
  = CarryOn		-- Produce an error message, but keep on with next inst

  | Stop		-- Produce an error message and stop reduction

  | AddToIrreds		-- Just add the inst to the irreductible ones; don't 
			-- produce an error message of any kind.
			-- It might be quite legitimate
			-- such as (Eq a)!
\end{code}



\begin{code}
type RedState s
  = (Avails s,		-- What's available
     [Inst s],		-- Insts for which try_me returned Free
     [Inst s]		-- Insts for which try_me returned DontReduce
    )

type Avails s = FiniteMap (Inst s) (Avail s)

data Avail s
  = Avail
	(TcIdOcc s)	-- The "main Id"; that is, the Id for the Inst that 
			-- caused this avail to be put into the finite map in the first place
			-- It is this Id that is bound to the RHS.

	(RHS s)	        -- The RHS: an expression whose value is that Inst.
			-- The main Id should be bound to this RHS

	[TcIdOcc s]	-- Extra Ids that must all be bound to the main Id.
			-- At the end we generate a list of bindings
			--	 { i1 = main_id; i2 = main_id; i3 = main_id; ... }

data RHS s
  = NoRhs		-- Used for irreducible dictionaries,
			-- which are going to be lambda bound, or for those that are
			-- suppplied as "given" when checking againgst a signature.
			--
			-- NoRhs is also used for Insts like (CCallable f)
			-- where no witness is required.

  | Rhs 		-- Used when there is a RHS 
	(TcExpr s)	 
	Bool		-- True => the RHS simply selects a superclass dictionary
			--	   from a subclass dictionary.
			-- False => not so.  
			-- This is useful info, because superclass selection
			-- is cheaper than building the dictionary using its dfun,
			-- and we can sometimes replace the latter with the former

  | PassiveScSel	-- Used for as-yet-unactivated RHSs.  For example suppose we have
			-- an (Ord t) dictionary; then we put an (Eq t) entry in
			-- the finite map, with an PassiveScSel.  Then if the
			-- the (Eq t) binding is ever *needed* we make it an Rhs
	(TcExpr s)
	[Inst s]	-- List of Insts that are free in the RHS.
			-- If the main Id is subsequently needed, we toss this list into
			-- the needed-inst pool so that we make sure their bindings
			-- will actually be produced.
			--
			-- Invariant: these Insts are already in the finite mapping


pprAvails avails = vcat (map pp (eltsFM avails))
  where
    pp (Avail main_id rhs ids)
      = ppr main_id <> colon <+> brackets (ppr ids) <+> pprRhs rhs

pprRhs NoRhs = text "<no rhs>"
pprRhs (Rhs rhs b) = ppr rhs
pprRhs (PassiveScSel rhs is) = text "passive" <+> ppr rhs
\end{code}


%************************************************************************
%*									*
\subsection[reduce]{@reduce@}
%*									*
%************************************************************************

The main entry point for context reduction is @reduceContext@:

\begin{code}
reduceContext :: SDoc -> (Inst s -> WhatToDo)
	      -> [Inst s]	-- Given
	      -> [Inst s]	-- Wanted
	      -> TcM s (TcDictBinds s, [Inst s], [Inst s])

reduceContext str try_me givens wanteds
  =     -- Zonking first
    mapNF_Tc zonkInst givens	`thenNF_Tc` \ givens ->
    mapNF_Tc zonkInst wanteds	`thenNF_Tc` \ wanteds ->

{-
    pprTrace "reduceContext" (vcat [
	     text "----------------------",
	     str,
	     text "given" <+> ppr givens,
	     text "wanted" <+> ppr wanteds,
	     text "----------------------"
	     ]) $
-}

        -- Build the Avail mapping from "givens"
    foldlNF_Tc addGiven emptyFM givens		`thenNF_Tc` \ avails ->

        -- Do the real work
    reduce try_me wanteds (avails, [], [])	`thenTc` \ (avails, frees, irreds) ->

	-- Extract the bindings from avails
    let
       binds = foldFM add_bind EmptyMonoBinds avails

       add_bind _ (Avail main_id rhs ids) binds
         = foldr add_synonym (add_rhs_bind rhs binds) ids
	 where
	   add_rhs_bind (Rhs rhs _) binds = binds `AndMonoBinds` VarMonoBind main_id rhs 
	   add_rhs_bind other       binds = binds

	   -- Add the trivial {x = y} bindings
	   -- The main Id can end up in the list when it's first added passively
	   -- and then activated, so we have to filter it out.  A bit of a hack.
	   add_synonym id binds
	     | id /= main_id = binds `AndMonoBinds` VarMonoBind id (HsVar main_id)
	     | otherwise     = binds
    in
{-
    pprTrace ("reduceContext1") (vcat [
	     text "----------------------",
	     str,
	     text "given" <+> ppr givens,
	     text "wanted" <+> ppr wanteds,
	     text "----", 
	     pprAvails avails,
	     text "----------------------"
	     ]) $
-}
    returnTc (binds, frees, irreds)
\end{code}

The main context-reduction function is @reduce@.  Here's its game plan.

\begin{code}
reduce :: (Inst s -> WhatToDo)
       -> [Inst s]
       -> RedState s
       -> TcM s (RedState s)
\end{code}

@reduce@ is passed
     try_me:	given an inst, this function returns
		  Reduce       reduce this
		  DontReduce   return this in "irreds"
		  Free	       return this in "frees"

     wanteds:	The list of insts to reduce
     state:	An accumulating parameter of type RedState 
		that contains the state of the algorithm

  It returns a RedState.


\begin{code}
    -- Base case: we're done!
reduce try_me [] state = returnTc state

reduce try_me (wanted:wanteds) state@(avails, frees, irreds)

    -- It's the same as an existing inst, or a superclass thereof
  | wanted `elemFM` avails
  = reduce try_me wanteds (activate avails wanted, frees, irreds)

    -- It should be reduced
  | case try_me_result of { ReduceMe _ -> True; _ -> False }
  = lookupInst wanted	      `thenNF_Tc` \ lookup_result ->

    case lookup_result of
      GenInst wanteds' rhs -> use_instance wanteds' rhs
      SimpleInst rhs       -> use_instance []       rhs

      NoInstance ->    -- No such instance! 
		       -- Decide what to do based on the no_instance_action requested
		 case no_instance_action of
		   Stop -> 		-- Fail
		            addNoInstanceErr wanted		`thenNF_Tc_`
			    failTc
	
		   CarryOn -> 		-- Carry on.
				-- Add the bad guy to the avails to suppress similar
				-- messages from other insts in wanteds
		            addNoInstanceErr wanted	`thenNF_Tc_`
			    addGiven avails wanted	`thenNF_Tc` \ avails' -> 
			    reduce try_me wanteds (avails', frees, irreds)	-- Carry on

		   AddToIrreds -> 	-- Add the offending insts to the irreds
				  add_to_irreds
				  


    -- It's free and this isn't a top-level binding, so just chuck it upstairs
  | case try_me_result of { Free -> True; _ -> False }
  =     -- First, see if the inst can be reduced to a constant in one step
    lookupInst wanted	  `thenNF_Tc` \ lookup_result ->
    case lookup_result of
       SimpleInst rhs -> use_instance [] rhs
       other	      -> add_to_frees

    -- It's free and this is a top level binding, so
    -- check whether it's a tautology or not
  | case try_me_result of { FreeIfTautological -> True; _ -> False }
  =     -- Try for tautology
    tryTc 
	  -- If tautology trial fails, add to irreds
	  (addGiven avails wanted      `thenNF_Tc` \ avails' ->
	   returnTc (avails', frees, wanted:irreds))

	  -- If tautology succeeds, just add to frees
	  (reduce try_me_taut [wanted] (avails, [], [])		`thenTc_`
	   returnTc (avails, wanted:frees, irreds))
								`thenTc` \ state' ->
    reduce try_me wanteds state'


    -- It's irreducible (or at least should not be reduced)
  | otherwise
  = ASSERT( case try_me_result of { DontReduce -> True; other -> False } )
        -- See if the inst can be reduced to a constant in one step
    lookupInst wanted	  `thenNF_Tc` \ lookup_result ->
    case lookup_result of
       SimpleInst rhs -> use_instance [] rhs
       other          -> add_to_irreds

  where
	-- The three main actions
    add_to_frees  = reduce try_me wanteds (avails, wanted:frees, irreds)

    add_to_irreds = addGiven avails wanted		`thenNF_Tc` \ avails' ->
		    reduce try_me wanteds (avails',  frees, wanted:irreds)

    use_instance wanteds' rhs = addWanted avails wanted rhs	`thenNF_Tc` \ avails' ->
		       		reduce try_me (wanteds' ++ wanteds) (avails', frees, irreds)


    try_me_result	        = try_me wanted
    ReduceMe no_instance_action = try_me_result

    -- The try-me to use when trying to identify tautologies
    -- It blunders on reducing as much as possible
    try_me_taut inst = ReduceMe Stop	-- No error recovery
\end{code}


\begin{code}
activate :: Avails s -> Inst s -> Avails s
	 -- Activate the binding for Inst, ensuring that a binding for the
	 -- wanted Inst will be generated.
	 -- (Activate its parent if necessary, recursively).
	 -- Precondition: the Inst is in Avails already

activate avails wanted
  | not (instBindingRequired wanted) 
  = avails

  | otherwise
  = case lookupFM avails wanted of

      Just (Avail main_id (PassiveScSel rhs insts) ids) ->
	       foldl activate avails' insts	 -- Activate anything it needs
	     where
	       avails' = addToFM avails wanted avail'
	       avail'  = Avail main_id (Rhs rhs True) (wanted_id : ids)	-- Activate it

      Just (Avail main_id other_rhs ids) -> -- Just add to the synonyms list
	       addToFM avails wanted (Avail main_id other_rhs (wanted_id : ids))

      Nothing -> panic "activate"
  where
      wanted_id = instToId wanted
    
addWanted avails wanted rhs_expr
  = ASSERT( not (wanted `elemFM` avails) )
    returnNF_Tc (addToFM avails wanted avail)
	-- NB: we don't add the thing's superclasses too!
	-- Why not?  Because addWanted is used when we've successfully used an
	-- instance decl to reduce something; e.g.
	--	d:Ord [a] = dfunOrd (d1:Eq [a]) (d2:Ord a)
	-- Note that we pass the superclasses to the dfun, so they will be "wanted".
	-- If we put the superclasses of "d" in avails, then we might end up
	-- expressing "d1" in terms of "d", which would be a disaster.
  where
    avail = Avail (instToId wanted) rhs []

    rhs | instBindingRequired wanted = Rhs rhs_expr False	-- Not superclass selection
	| otherwise		     = NoRhs

addGiven :: Avails s -> Inst s -> NF_TcM s (Avails s)
addGiven avails given
  =	 -- ASSERT( not (given `elemFM` avails) )
	 -- This assertion isn' necessarily true.  It's permitted
	 -- to given a redundant context in a type signature (eg (Ord a, Eq a) => ...)
	 -- and when typechecking instance decls we generate redundant "givens" too.
    addAvail avails given avail
  where
    avail = Avail (instToId given) NoRhs []

addAvail avails wanted avail
  = addSuperClasses (addToFM avails wanted avail) wanted

addSuperClasses :: Avails s -> Inst s -> NF_TcM s (Avails s)
		-- Add all the superclasses of the Inst to Avails
		-- Invariant: the Inst is already in Avails.

addSuperClasses avails dict
  | not (isDict dict)
  = returnNF_Tc avails

  | otherwise	-- It is a dictionary
  = tcInstTheta env sc_theta		`thenNF_Tc` \ sc_theta' ->
    foldlNF_Tc add_sc avails (zipEqual "addSuperClasses" sc_theta' sc_sels)
  where
    (clas, tys) = getDictClassTys dict
    
    (tyvars, sc_theta, sc_sels, _, _) = classBigSig clas
    env       = zipTyVarEnv tyvars tys

    add_sc avails ((super_clas, super_tys), sc_sel)
      = newDictFromOld dict super_clas super_tys	`thenNF_Tc` \ super_dict ->
        let
	   sc_sel_rhs = DictApp (TyApp (HsVar (RealId sc_sel)) 
				       tys)
				[instToId dict]
	in
        case lookupFM avails super_dict of

	     Just (Avail main_id (Rhs rhs False {- not sc selection -}) ids) ->
		  -- Already there, but not as a superclass selector
		  -- No need to look at its superclasses; since it's there
		  --	already they must be already in avails
		  -- However, we must remember to activate the dictionary
		  -- from which it is (now) generated
		  returnNF_Tc (activate avails' dict)
		where
	     	  avails' = addToFM avails super_dict avail
		  avail   = Avail main_id (Rhs sc_sel_rhs True) ids	-- Superclass selection
	
	     Just (Avail _ _ _) -> returnNF_Tc avails
		  -- Already there; no need to do anything

	     Nothing ->
		  -- Not there at all, so add it, and its superclasses
		  addAvail avails super_dict avail
		where
		  avail   = Avail (instToId super_dict) 
				  (PassiveScSel sc_sel_rhs [dict])
				  []
\end{code}

%************************************************************************
%*									*
\subsection[simple]{@Simple@ versions}
%*									*
%************************************************************************

Much simpler versions when there are no bindings to make!

@tcSimplifyThetas@ simplifies class-type constraints formed by
@deriving@ declarations and when specialising instances.  We are
only interested in the simplified bunch of class/type constraints.

It simplifies to constraints of the form (C a b c) where
a,b,c are type variables.  This is required for the context of
instance declarations.

\begin{code}
tcSimplifyThetas :: (Class -> ClassInstEnv)		-- How to find the ClassInstEnv
	       	 -> ThetaType				-- Wanted
	       	 -> TcM s ThetaType			-- Needed; of the form C a b c
							-- where a,b,c are type variables

tcSimplifyThetas inst_mapper wanteds
  = reduceSimple inst_mapper [] wanteds		`thenNF_Tc` \ irreds ->
    let
	-- Check that the returned dictionaries are of the form (C a b c)
	bad_guys = [ct | ct@(clas,tys) <- irreds, not (all isTyVarTy tys)]
    in
    if null bad_guys then
	returnTc irreds
    else
       mapNF_Tc addNoInstErr bad_guys		`thenNF_Tc_`
       failTc
\end{code}

@tcSimplifyCheckThetas@ just checks class-type constraints, essentially;
used with \tr{default} declarations.  We are only interested in
whether it worked or not.

\begin{code}
tcSimplifyCheckThetas :: ThetaType	-- Given
		      -> ThetaType	-- Wanted
		      -> TcM s ()

tcSimplifyCheckThetas givens wanteds
  = reduceSimple classInstEnv givens wanteds    `thenNF_Tc`	\ irreds ->
    if null irreds then
       returnTc ()
    else
       mapNF_Tc addNoInstErr irreds		`thenNF_Tc_`
       failTc

addNoInstErr (c,ts) = addErrTc (noDictInstanceErr c ts)
\end{code}


\begin{code}
type AvailsSimple = FiniteMap (Class, [TauType]) Bool
		    -- True  => irreducible 
		    -- False => given, or can be derived from a given or from an irreducible

reduceSimple :: (Class -> ClassInstEnv) 
	     -> ThetaType		-- Given
	     -> ThetaType		-- Wanted
	     -> NF_TcM s ThetaType	-- Irreducible

reduceSimple inst_mapper givens wanteds
  = reduce_simple inst_mapper givens_fm wanteds	`thenNF_Tc` \ givens_fm' ->
    returnNF_Tc [ct | (ct,True) <- fmToList givens_fm']
  where
    givens_fm     = foldl addNonIrred emptyFM givens

reduce_simple :: (Class -> ClassInstEnv) 
	      -> AvailsSimple
	      -> ThetaType
	      -> NF_TcM s AvailsSimple

reduce_simple inst_mapper givens [] 
  =	     -- Finished, so pull out the needed ones
    returnNF_Tc givens

reduce_simple inst_mapper givens (wanted@(clas,tys) : wanteds)
  | wanted `elemFM` givens
  = reduce_simple inst_mapper givens wanteds

  | otherwise
  = lookupSimpleInst (inst_mapper clas) clas tys	`thenNF_Tc` \ maybe_theta ->

    case maybe_theta of
      Nothing ->    reduce_simple inst_mapper (addIrred    givens wanted) wanteds
      Just theta -> reduce_simple inst_mapper (addNonIrred givens wanted) (theta ++ wanteds)

addIrred :: AvailsSimple -> (Class, [TauType]) -> AvailsSimple
addIrred givens ct
  = addSCs (addToFM givens ct True) ct

addNonIrred :: AvailsSimple -> (Class, [TauType]) -> AvailsSimple
addNonIrred givens ct
  = addSCs (addToFM givens ct False) ct

addSCs givens ct@(clas,tys)
 = foldl add givens sc_theta
 where
   (tyvars, sc_theta_tmpl, _, _, _) = classBigSig clas
   sc_theta = instantiateThetaTy (zipTyVarEnv tyvars tys) sc_theta_tmpl

   add givens ct = case lookupFM givens ct of
			   Nothing    -> -- Add it and its superclasses
					 addSCs (addToFM givens ct False) ct

			   Just True  -> -- Set its flag to False; superclasses already done
				         addToFM givens ct False

			   Just False -> -- Already done
				         givens
			   
\end{code}

%************************************************************************
%*									*
\subsection[binds-for-local-funs]{@bindInstsOfLocalFuns@}
%*									*
%************************************************************************

When doing a binding group, we may have @Insts@ of local functions.
For example, we might have...
\begin{verbatim}
let f x = x + 1	    -- orig local function (overloaded)
    f.1 = f Int	    -- two instances of f
    f.2 = f Float
 in
    (f.1 5, f.2 6.7)
\end{verbatim}
The point is: we must drop the bindings for @f.1@ and @f.2@ here,
where @f@ is in scope; those @Insts@ must certainly not be passed
upwards towards the top-level.	If the @Insts@ were binding-ified up
there, they would have unresolvable references to @f@.

We pass in an @init_lie@ of @Insts@ and a list of locally-bound @Ids@.
For each method @Inst@ in the @init_lie@ that mentions one of the
@Ids@, we create a binding.  We return the remaining @Insts@ (in an
@LIE@), as well as the @HsBinds@ generated.

\begin{code}
bindInstsOfLocalFuns ::	LIE s -> [TcIdBndr s] -> TcM s (LIE s, TcMonoBinds s)

bindInstsOfLocalFuns init_lie local_ids
  = reduceContext (text "bindInsts" <+> ppr local_ids)
		  try_me [] (bagToList init_lie)	`thenTc` \ (binds, frees, irreds) ->
    ASSERT( null irreds )
    returnTc (mkLIE frees, binds)
  where
    local_id_set = mkIdSet local_ids	-- There can occasionally be a lot of them
					-- so it's worth building a set, so that 
					-- lookup (in isMethodFor) is faster
    try_me inst | isMethodFor local_id_set inst = ReduceMe CarryOn
		| otherwise		        = Free
\end{code}


%************************************************************************
%*									*
\section[Disambig]{Disambiguation of overloading}
%*									*
%************************************************************************


If a dictionary constrains a type variable which is
\begin{itemize}
\item
not mentioned in the environment
\item
and not mentioned in the type of the expression
\end{itemize}
then it is ambiguous. No further information will arise to instantiate
the type variable; nor will it be generalised and turned into an extra
parameter to a function.

It is an error for this to occur, except that Haskell provided for
certain rules to be applied in the special case of numeric types.

Specifically, if
\begin{itemize}
\item
at least one of its classes is a numeric class, and
\item
all of its classes are numeric or standard
\end{itemize}
then the type variable can be defaulted to the first type in the
default-type list which is an instance of all the offending classes.

So here is the function which does the work.  It takes the ambiguous
dictionaries and either resolves them (producing bindings) or
complains.  It works by splitting the dictionary list by type
variable, and using @disambigOne@ to do the real business.


@tcSimplifyTop@ is called once per module to simplify
all the constant and ambiguous Insts.

\begin{code}
tcSimplifyTop :: LIE s -> TcM s (TcDictBinds s)
tcSimplifyTop wanteds
  = reduceContext (text "tcSimplTop") try_me [] (bagToList wanteds)	`thenTc` \ (binds1, frees, irreds) ->
    ASSERT( null frees )

    let
		-- All the non-std ones are definite errors
	(stds, non_stds) = partition isStdClassTyVarDict irreds
	

		-- Group by type variable
	std_groups = equivClasses cmp_by_tyvar stds

		-- Pick the ones which its worth trying to disambiguate
	(std_oks, std_bads) = partition worth_a_try std_groups
		-- Have a try at disambiguation 
		-- if the type variable isn't bound
		-- up with one of the non-standard classes
	worth_a_try group@(d:_) = isEmptyTyVarSet (tyVarsOfInst d `intersectTyVarSets` non_std_tyvars)
	non_std_tyvars		= unionManyTyVarSets (map tyVarsOfInst non_stds)

		-- Collect together all the bad guys
	bad_guys = non_stds ++ concat std_bads
    in

	-- Disambiguate the ones that look feasible
    mapTc disambigGroup std_oks		`thenTc` \ binds_ambig ->

	-- And complain about the ones that don't
    mapNF_Tc complain bad_guys		`thenNF_Tc_`

    returnTc (binds1 `AndMonoBinds` andMonoBinds binds_ambig)
  where
    try_me inst		 = ReduceMe AddToIrreds

    d1 `cmp_by_tyvar` d2 = get_tv d1 `compare` get_tv d2

    complain d | isEmptyTyVarSet (tyVarsOfInst d) = addNoInstanceErr d
	       | otherwise			  = addAmbigErr [d]

get_tv d   = case getDictClassTys d of
		   (clas, [ty]) -> getTyVar "tcSimplifyTop" ty
get_clas d = case getDictClassTys d of
		   (clas, [ty]) -> clas
\end{code}

@disambigOne@ assumes that its arguments dictionaries constrain all
the same type variable.

ADR Comment 20/6/94: I've changed the @CReturnable@ case to default to
@()@ instead of @Int@.  I reckon this is the Right Thing to do since
the most common use of defaulting is code like:
\begin{verbatim}
	_ccall_ foo	`seqPrimIO` bar
\end{verbatim}
Since we're not using the result of @foo@, the result if (presumably)
@void@.

\begin{code}
disambigGroup :: [Inst s]	-- All standard classes of form (C a)
	      -> TcM s (TcDictBinds s)

disambigGroup dicts
  |  any isNumericClass classes 	-- Guaranteed all standard classes
  = 	-- THE DICTS OBEY THE DEFAULTABLE CONSTRAINT
	-- SO, TRY DEFAULT TYPES IN ORDER

	-- Failure here is caused by there being no type in the
	-- default list which can satisfy all the ambiguous classes.
	-- For example, if Real a is reqd, but the only type in the
	-- default list is Int.
    tcGetDefaultTys			`thenNF_Tc` \ default_tys ->
    let
      try_default [] 	-- No defaults work, so fail
	= failTc

      try_default (default_ty : default_tys)
	= tryTc (try_default default_tys) $	-- If default_ty fails, we try
						-- default_tys instead
	  tcSimplifyCheckThetas [] thetas	`thenTc` \ _ ->
	  returnTc default_ty
        where
	  thetas = classes `zip` repeat [default_ty]
    in
	-- See if any default works, and if so bind the type variable to it
	-- If not, add an AmbigErr
    recoverTc (addAmbigErr dicts `thenNF_Tc_` returnTc EmptyMonoBinds)	$

    try_default default_tys		 	`thenTc` \ chosen_default_ty ->

	-- Bind the type variable and reduce the context, for real this time
    tcInstType emptyTyVarEnv chosen_default_ty		`thenNF_Tc` \ chosen_default_tc_ty ->	-- Tiresome!
    unifyTauTy chosen_default_tc_ty (mkTyVarTy tyvar)	`thenTc_`
    reduceContext (text "disambig" <+> ppr dicts)
		  try_me [] dicts	`thenTc` \ (binds, frees, ambigs) ->
    ASSERT( null frees && null ambigs )
    returnTc binds

  | all isCcallishClass classes
  = 	-- Default CCall stuff to (); we don't even both to check that () is an 
	-- instance of CCallable/CReturnable, because we know it is.
    unifyTauTy (mkTyVarTy tyvar) unitTy    `thenTc_`
    returnTc EmptyMonoBinds
    
  | otherwise -- No defaults
  = addAmbigErr dicts	`thenNF_Tc_`
    returnTc EmptyMonoBinds

  where
    try_me inst = ReduceMe CarryOn
    tyvar       = get_tv (head dicts)		-- Should be non-empty
    classes     = map get_clas dicts
\end{code}



Errors and contexts
~~~~~~~~~~~~~~~~~~~
ToDo: for these error messages, should we note the location as coming
from the insts, or just whatever seems to be around in the monad just
now?

\begin{code}
genCantGenErr insts	-- Can't generalise these Insts
  = sep [ptext SLIT("Cannot generalise these overloadings (in a _ccall_):"), 
	 nest 4 (pprInstsInFull insts)
	]

addAmbigErr dicts
  = tcAddSrcLoc (instLoc (head dicts)) $
    addErrTc (sep [text "Cannot resolve the ambiguous context" <+> pprInsts dicts,
	 	   nest 4 (pprInstsInFull dicts)])

addNoInstanceErr dict
  = tcAddSrcLoc (instLoc dict)		       $
    tcAddErrCtxt (pprOrigin dict)	       $
    addErrTc (noDictInstanceErr clas tys)	       
  where
    (clas, tys) = getDictClassTys dict

noDictInstanceErr clas tys
  = ptext SLIT("No instance for:") <+> quotes (pprConstraint clas tys)

reduceSigCtxt lie
  = sep [ptext SLIT("When matching against a type signature with context"),
         nest 4 (quotes (pprInsts (bagToList lie)))
    ]
\end{code}


