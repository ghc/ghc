%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
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
	tcSimplify, tcSimplifyAndCheck, tcSimplifyToDicts, 
	tcSimplifyTop, tcSimplifyThetas, tcSimplifyCheckThetas,
	bindInstsOfLocalFuns
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_MaxContextReductionDepth, opt_GlasgowExts, opt_WarnTypeDefaults )
import HsSyn		( MonoBinds(..), HsExpr(..), andMonoBinds, andMonoBindList )
import TcHsSyn		( TcExpr, TcId, 
			  TcMonoBinds, TcDictBinds
			)

import TcMonad
import Inst		( lookupInst, lookupSimpleInst, LookupInstResult(..),
			  tyVarsOfInst, 
			  isDict, isStdClassTyVarDict, isMethodFor,
			  instToId, instBindingRequired, instCanBeGeneralised,
			  newDictFromOld,
			  getDictClassTys,
			  instLoc, pprInst, zonkInst, tidyInst, tidyInsts,
			  Inst, LIE, pprInsts, pprInstsInFull, mkLIE, emptyLIE, 
			  plusLIE
			)
import TcEnv		( tcGetGlobalTyVars )
import TcType		( TcType, TcTyVarSet, typeToTcType )
import TcUnify		( unifyTauTy )
import Id		( idType )
import Bag		( bagToList )
import Class		( Class, classBigSig, classInstEnv )
import PrelInfo		( isNumericClass, isCreturnableClass, isCcallishClass )

import Type		( Type, ThetaType, TauType, mkTyVarTy, getTyVar,
			  isTyVarTy, splitSigmaTy, tyVarsOfTypes
			)
import InstEnv		( InstEnv )
import Subst		( mkTopTyVarSubst, substTheta )
import PprType		( pprConstraint )
import TysWiredIn	( unitTy )
import VarSet
import FiniteMap
import BasicTypes	( TopLevelFlag(..) )
import CmdLineOpts	( opt_GlasgowExts )
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
	-> TcTyVarSet			-- ``Local''  type variables
					-- ASSERT: this tyvar set is already zonked
	-> LIE				-- Wanted
	-> TcM s (LIE,			-- Free
		  TcDictBinds,		-- Bindings
		  LIE)			-- Remaining wanteds; no dups

tcSimplify str top_lvl local_tvs wanted_lie
  | isEmptyVarSet local_tvs
  = returnTc (wanted_lie, EmptyMonoBinds, emptyLIE)

  | otherwise
  = reduceContext str try_me [] wanteds		`thenTc` \ (binds, frees, irreds) ->

	-- Check for non-generalisable insts
    let
	cant_generalise = filter (not . instCanBeGeneralised) irreds
    in
    checkTc (null cant_generalise)
	    (genCantGenErr cant_generalise)	`thenTc_`

	-- Check for ambiguous insts.
	-- You might think these can't happen (I did) because an ambiguous
	-- inst like (Eq a) will get tossed out with "frees", and eventually
	-- dealt with by tcSimplifyTop.
	-- But we can get stuck with 
	--	C a b
	-- where "a" is one of the local_tvs, but "b" is unconstrained.
	-- Then we must yell about the ambiguous b
	-- But we must only do so if "b" really is unconstrained; so
	-- we must grab the global tyvars to answer that question
    tcGetGlobalTyVars				`thenNF_Tc` \ global_tvs ->
    let
	avail_tvs	    = local_tvs `unionVarSet` global_tvs
	(irreds', bad_guys) = partition (isEmptyVarSet . ambig_tv_fn) irreds
	ambig_tv_fn dict    = tyVarsOfInst dict `minusVarSet` avail_tvs
    in
    addAmbigErrs ambig_tv_fn bad_guys	`thenNF_Tc_`


	-- Finished
    returnTc (mkLIE frees, binds, mkLIE irreds')
  where
    wanteds = bagToList wanted_lie

    try_me inst 
      -- Does not constrain a local tyvar
      | isEmptyVarSet (tyVarsOfInst inst `intersectVarSet` local_tvs)
      = -- if is_top_level then
	--   FreeIfTautological		  -- Special case for inference on 
	--				  -- top-level defns
	-- else
	Free

      -- We're infering (not checking) the type, and 
      -- the inst constrains a local type variable
      | isDict inst  = DontReduce	    	-- Dicts
      | otherwise    = ReduceMe AddToIrreds	-- Lits and Methods
\end{code}

@tcSimplifyAndCheck@ is similar to the above, except that it checks
that there is an empty wanted-set at the end.  It may still return
some of constant insts, which have to be resolved finally at the end.

\begin{code}
tcSimplifyAndCheck
	 :: SDoc 
	 -> TcTyVarSet		-- ``Local''  type variables
				-- ASSERT: this tyvar set is already zonked
	 -> LIE			-- Given; constrain only local tyvars
	 -> LIE			-- Wanted
	 -> TcM s (LIE,		-- Free
		   TcDictBinds)	-- Bindings

tcSimplifyAndCheck str local_tvs given_lie wanted_lie
  | isEmptyVarSet local_tvs
	-- This can happen quite legitimately; for example in
	-- 	instance Num Int where ...
  = returnTc (wanted_lie, EmptyMonoBinds)

  | otherwise
  = reduceContext str try_me givens wanteds	`thenTc` \ (binds, frees, irreds) ->

	-- Complain about any irreducible ones
    mapNF_Tc complain irreds	`thenNF_Tc_`

	-- Done
    returnTc (mkLIE frees, binds)
  where
    givens  = bagToList given_lie
    wanteds = bagToList wanted_lie
    given_dicts = filter isDict givens

    try_me inst 
      -- Does not constrain a local tyvar
      | isEmptyVarSet (tyVarsOfInst inst `intersectVarSet` local_tvs)
      = Free

      -- When checking against a given signature we always reduce
      -- until we find a match against something given, or can't reduce
      | otherwise
      = ReduceMe AddToIrreds

    complain dict = mapNF_Tc zonkInst givens	`thenNF_Tc` \ givens ->
		    addNoInstanceErr str given_dicts dict
\end{code}

On the LHS of transformation rules we only simplify methods and constants,
getting dictionaries.  We want to keep all of them unsimplified, to serve
as the available stuff for the RHS of the rule.

The same thing is used for specialise pragmas. Consider
	
	f :: Num a => a -> a
	{-# SPECIALISE f :: Int -> Int #-}
	f = ...

The type checker generates a binding like:

	f_spec = (f :: Int -> Int)

and we want to end up with

	f_spec = _inline_me_ (f Int dNumInt)

But that means that we must simplify the Method for f to (f Int dNumInt)! 
So tcSimplifyToDicts squeezes out all Methods.

\begin{code}
tcSimplifyToDicts :: LIE -> TcM s (LIE, TcDictBinds)
tcSimplifyToDicts wanted_lie
  = reduceContext (text "tcSimplifyToDicts") try_me [] wanteds	`thenTc` \ (binds, frees, irreds) ->
    ASSERT( null frees )
    returnTc (mkLIE irreds, binds)
  where
    wanteds	= bagToList wanted_lie

	-- Reduce methods and lits only; stop as soon as we get a dictionary
    try_me inst	| isDict inst = DontReduce
		| otherwise   = ReduceMe AddToIrreds
\end{code}



%************************************************************************
%*									*
\subsection{Data types for the reduction mechanism}
%*									*
%************************************************************************

The main control over context reduction is here

\begin{code}
data WhatToDo 
 = ReduceMe		  -- Try to reduce this
	NoInstanceAction  -- What to do if there's no such instance

 | DontReduce		  -- Return as irreducible

 | Free			  -- Return as free

 | FreeIfTautological	  -- Return as free iff it's tautological; 
			  -- if not, return as irreducible
	-- The FreeIfTautological case is to allow the possibility
	-- of generating functions with types like
	--	f :: C Int => Int -> Int
	-- Here, the C Int isn't a tautology presumably because Int
	-- isn't an instance of C in this module; but perhaps it will
	-- be at f's call site(s).  Haskell doesn't allow this at
	-- present.

data NoInstanceAction
  = Stop		-- Fail; no error message
			-- (Only used when tautology checking.)

  | AddToIrreds		-- Just add the inst to the irreductible ones; don't 
			-- produce an error message of any kind.
			-- It might be quite legitimate such as (Eq a)!
\end{code}



\begin{code}
type RedState s
  = (Avails s,		-- What's available
     [Inst],		-- Insts for which try_me returned Free
     [Inst]		-- Insts for which try_me returned DontReduce
    )

type Avails s = FiniteMap Inst Avail

data Avail
  = Avail
	TcId		-- The "main Id"; that is, the Id for the Inst that 
			-- caused this avail to be put into the finite map in the first place
			-- It is this Id that is bound to the RHS.

	RHS	        -- The RHS: an expression whose value is that Inst.
			-- The main Id should be bound to this RHS

	[TcId]	-- Extra Ids that must all be bound to the main Id.
			-- At the end we generate a list of bindings
			--	 { i1 = main_id; i2 = main_id; i3 = main_id; ... }

data RHS
  = NoRhs		-- Used for irreducible dictionaries,
			-- which are going to be lambda bound, or for those that are
			-- suppplied as "given" when checking againgst a signature.
			--
			-- NoRhs is also used for Insts like (CCallable f)
			-- where no witness is required.

  | Rhs 		-- Used when there is a RHS 
	TcExpr	 
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
	TcExpr
	[Inst]	-- List of Insts that are free in the RHS.
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
reduceContext :: SDoc -> (Inst -> WhatToDo)
	      -> [Inst]	-- Given
	      -> [Inst]	-- Wanted
	      -> TcM s (TcDictBinds, 
			[Inst],		-- Free
			[Inst])		-- Irreducible

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
    reduceList (0,[]) try_me wanteds (avails, [], [])	`thenTc` \ (avails, frees, irreds) ->

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
    pprTrace ("reduceContext end") (vcat [
	     text "----------------------",
	     str,
	     text "given" <+> ppr givens,
	     text "wanted" <+> ppr wanteds,
	     text "----", 
	     text "avails" <+> pprAvails avails,
	     text "irreds" <+> ppr irreds,
	     text "----------------------"
	     ]) $
-}
    returnTc (binds, frees, irreds)
\end{code}

The main context-reduction function is @reduce@.  Here's its game plan.

\begin{code}
reduceList :: (Int,[Inst])		-- Stack (for err msgs)
					-- along with its depth
       	   -> (Inst -> WhatToDo)
       	   -> [Inst]
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

The (n,stack) pair is just used for error reporting.  
n is always the depth of the stack.
The stack is the stack of Insts being reduced: to produce X
I had to produce Y, to produce Y I had to produce Z, and so on.

\begin{code}
reduceList (n,stack) try_me wanteds state
  | n > opt_MaxContextReductionDepth
  = failWithTc (reduceDepthErr n stack)

  | otherwise
  =
#ifdef DEBUG
   (if n > 8 then
	pprTrace "Jeepers! ReduceContext:" (reduceDepthMsg n stack)
    else (\x->x))
#endif
    go wanteds state
  where
    go []     state = returnTc state
    go (w:ws) state = reduce (n+1, w:stack) try_me w state	`thenTc` \ state' ->
		      go ws state'

    -- Base case: we're done!
reduce stack try_me wanted state@(avails, frees, irreds)
    -- It's the same as an existing inst, or a superclass thereof
  | wanted `elemFM` avails
  = returnTc (activate avails wanted, frees, irreds)

  | otherwise
  = case try_me wanted of {

    ReduceMe no_instance_action ->	-- It should be reduced
	lookupInst wanted	      `thenNF_Tc` \ lookup_result ->
	case lookup_result of
	    GenInst wanteds' rhs -> use_instance wanteds' rhs
	    SimpleInst rhs       -> use_instance []       rhs

	    NoInstance ->    -- No such instance! 
		    case no_instance_action of
			Stop        -> failTc		
			AddToIrreds -> add_to_irreds
    ;
    Free ->	-- It's free and this isn't a top-level binding, so just chuck it upstairs
  		-- First, see if the inst can be reduced to a constant in one step
	lookupInst wanted	  `thenNF_Tc` \ lookup_result ->
	case lookup_result of
	    SimpleInst rhs -> use_instance [] rhs
	    other	   -> add_to_frees

    
    
    ;
    FreeIfTautological -> -- It's free and this is a top level binding, so
			  -- check whether it's a tautology or not
	tryTc_
	  add_to_irreds	  -- If tautology trial fails, add to irreds

	  -- If tautology succeeds, just add to frees
	  (reduce stack try_me_taut wanted (avails, [], [])	`thenTc_`
	   returnTc (avails, wanted:frees, irreds))


    ;
    DontReduce ->    -- It's irreducible (or at least should not be reduced)
        -- See if the inst can be reduced to a constant in one step
	lookupInst wanted	  `thenNF_Tc` \ lookup_result ->
	case lookup_result of
	   SimpleInst rhs -> use_instance [] rhs
	   other          -> add_to_irreds
    }
  where
	-- The three main actions
    add_to_frees  = let 
			avails' = addFree avails wanted
			-- Add the thing to the avails set so any identical Insts
			-- will be commoned up with it right here
		    in
		    returnTc (avails', wanted:frees, irreds)

    add_to_irreds = addGiven avails wanted		`thenNF_Tc` \ avails' ->
		    returnTc (avails',  frees, wanted:irreds)

    use_instance wanteds' rhs = addWanted avails wanted rhs	`thenNF_Tc` \ avails' ->
			  	reduceList stack try_me wanteds' (avails', frees, irreds)


    -- The try-me to use when trying to identify tautologies
    -- It blunders on reducing as much as possible
    try_me_taut inst = ReduceMe Stop	-- No error recovery
\end{code}


\begin{code}
activate :: Avails s -> Inst -> Avails s
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

addFree :: Avails s -> Inst -> (Avails s)
	-- When an Inst is tossed upstairs as 'free' we nevertheless add it
	-- to avails, so that any other equal Insts will be commoned up right
	-- here rather than also being tossed upstairs.  This is really just
	-- an optimisation, and perhaps it is more trouble that it is worth,
	-- as the following comments show!
	--
	-- NB1: do *not* add superclasses.  If we have
	--	df::Floating a
	--	dn::Num a
	-- but a is not bound here, then we *don't* want to derive 
	-- dn from df here lest we lose sharing.
	--
	-- NB2: do *not* add the Inst to avails at all if it's a method.
	-- The following situation shows why this is bad:
	--	truncate :: forall a. RealFrac a => forall b. Integral b => a -> b
	-- From an application (truncate f i) we get
	--	t1 = truncate at f 
	--	t2 = t1 at i
	-- If we have also have a secon occurrence of truncate, we get
	--	t3 = truncate at f
	--	t4 = t3 at i
	-- When simplifying with i,f free, we might still notice that
	--   t1=t3; but alas, the binding for t2 (which mentions t1)
	--   will continue to float out!
	-- Solution: never put methods in avail till they are captured
	-- in which case addFree isn't used
addFree avails free
  | isDict free = addToFM avails free (Avail (instToId free) NoRhs [])
  | otherwise   = avails

addGiven :: Avails s -> Inst -> NF_TcM s (Avails s)
addGiven avails given
  =	 -- ASSERT( not (given `elemFM` avails) )
	 -- This assertion isn't necessarily true.  It's permitted
	 -- to given a redundant context in a type signature (eg (Ord a, Eq a) => ...)
	 -- and when typechecking instance decls we generate redundant "givens" too.
    addAvail avails given avail
  where
    avail = Avail (instToId given) NoRhs []

addAvail avails wanted avail
  = addSuperClasses (addToFM avails wanted avail) wanted

addSuperClasses :: Avails s -> Inst -> NF_TcM s (Avails s)
		-- Add all the superclasses of the Inst to Avails
		-- Invariant: the Inst is already in Avails.

addSuperClasses avails dict
  | not (isDict dict)
  = returnNF_Tc avails

  | otherwise	-- It is a dictionary
  = foldlNF_Tc add_sc avails (zipEqual "addSuperClasses" sc_theta' sc_sels)
  where
    (clas, tys) = getDictClassTys dict
    
    (tyvars, sc_theta, sc_sels, _, _) = classBigSig clas
    sc_theta' = substTheta (mkTopTyVarSubst tyvars tys) sc_theta

    add_sc avails ((super_clas, super_tys), sc_sel)
      = newDictFromOld dict super_clas super_tys	`thenNF_Tc` \ super_dict ->
        let
	   sc_sel_rhs = DictApp (TyApp (HsVar sc_sel) tys)
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
tcSimplifyThetas :: (Class -> InstEnv)		-- How to find the InstEnv
	       	 -> ThetaType			-- Wanted
	       	 -> TcM s ThetaType		-- Needed

tcSimplifyThetas inst_mapper wanteds
  = reduceSimple inst_mapper [] wanteds		`thenNF_Tc` \ irreds ->
    let
	-- For multi-param Haskell, check that the returned dictionaries
 	-- don't have any of the form (C Int Bool) for which
	-- we expect an instance here
	-- For Haskell 98, check that all the constraints are of the form C a,
	-- where a is a type variable
    	bad_guys | opt_GlasgowExts = [ct | ct@(clas,tys) <- irreds, 
					   isEmptyVarSet (tyVarsOfTypes tys)]
		 | otherwise       = [ct | ct@(clas,tys) <- irreds, 
					   not (all isTyVarTy tys)]
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
\end{code}


\begin{code}
type AvailsSimple = FiniteMap (Class, [TauType]) Bool
		    -- True  => irreducible 
		    -- False => given, or can be derived from a given or from an irreducible

reduceSimple :: (Class -> InstEnv) 
	     -> ThetaType		-- Given
	     -> ThetaType		-- Wanted
	     -> NF_TcM s ThetaType	-- Irreducible

reduceSimple inst_mapper givens wanteds
  = reduce_simple (0,[]) inst_mapper givens_fm wanteds	`thenNF_Tc` \ givens_fm' ->
    returnNF_Tc [ct | (ct,True) <- fmToList givens_fm']
  where
    givens_fm     = foldl addNonIrred emptyFM givens

reduce_simple :: (Int,ThetaType)		-- Stack
	      -> (Class -> InstEnv) 
	      -> AvailsSimple
	      -> ThetaType
	      -> NF_TcM s AvailsSimple

reduce_simple (n,stack) inst_mapper avails wanteds
  = go avails wanteds
  where
    go avails []     = returnNF_Tc avails
    go avails (w:ws) = reduce_simple_help (n+1,w:stack) inst_mapper avails w	`thenNF_Tc` \ avails' ->
		       go avails' ws

reduce_simple_help stack inst_mapper givens wanted@(clas,tys)
  | wanted `elemFM` givens
  = returnNF_Tc givens

  | otherwise
  = lookupSimpleInst (inst_mapper clas) clas tys	`thenNF_Tc` \ maybe_theta ->

    case maybe_theta of
      Nothing ->    returnNF_Tc (addIrred givens wanted)
      Just theta -> reduce_simple stack inst_mapper (addNonIrred givens wanted) theta

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
   sc_theta = substTheta (mkTopTyVarSubst tyvars tys) sc_theta_tmpl

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
bindInstsOfLocalFuns ::	LIE -> [TcId] -> TcM s (LIE, TcMonoBinds)

bindInstsOfLocalFuns init_lie local_ids
  | null overloaded_ids || null lie_for_here
	-- Common case
  = returnTc (init_lie, EmptyMonoBinds)

  | otherwise
  = reduceContext (text "bindInsts" <+> ppr local_ids)
		  try_me [] lie_for_here	`thenTc` \ (binds, frees, irreds) ->
    ASSERT( null irreds )
    returnTc (mkLIE frees `plusLIE` mkLIE lie_not_for_here, binds)
  where
    overloaded_ids = filter is_overloaded local_ids
    is_overloaded id = case splitSigmaTy (idType id) of
			  (_, theta, _) -> not (null theta)

    overloaded_set = mkVarSet overloaded_ids	-- There can occasionally be a lot of them
						-- so it's worth building a set, so that 
						-- lookup (in isMethodFor) is faster

	-- No sense in repeatedly zonking lots of 
	-- constant constraints so filter them out here
    (lie_for_here, lie_not_for_here) = partition (isMethodFor overloaded_set)
					 	 (bagToList init_lie)
    try_me inst | isMethodFor overloaded_set inst = ReduceMe AddToIrreds
		| otherwise		          = Free
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
tcSimplifyTop :: LIE -> TcM s TcDictBinds
tcSimplifyTop wanted_lie
  = reduceContext (text "tcSimplTop") try_me [] wanteds	`thenTc` \ (binds1, frees, irreds) ->
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
	worth_a_try group@(d:_) = isEmptyVarSet (tyVarsOfInst d `intersectVarSet` non_std_tyvars)
	non_std_tyvars		= unionVarSets (map tyVarsOfInst non_stds)

		-- Collect together all the bad guys
	bad_guys = non_stds ++ concat std_bads
    in

	-- Disambiguate the ones that look feasible
    mapTc disambigGroup std_oks		`thenTc` \ binds_ambig ->

	-- And complain about the ones that don't
    mapNF_Tc complain bad_guys		`thenNF_Tc_`

    returnTc (binds1 `andMonoBinds` andMonoBindList binds_ambig)
  where
    wanteds	= bagToList wanted_lie
    try_me inst	= ReduceMe AddToIrreds

    d1 `cmp_by_tyvar` d2 = get_tv d1 `compare` get_tv d2

    complain d | isEmptyVarSet (tyVarsOfInst d) = addTopInstanceErr d
	       | otherwise			= addAmbigErr tyVarsOfInst d

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
disambigGroup :: [Inst]	-- All standard classes of form (C a)
	      -> TcM s TcDictBinds

disambigGroup dicts
  |   any isNumericClass classes 	-- Guaranteed all standard classes
	  -- see comment at the end of function for reasons as to 
	  -- why the defaulting mechanism doesn't apply to groups that
	  -- include CCallable or CReturnable dicts.
   && not (any isCcallishClass classes)
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
	= tryTc_ (try_default default_tys) $	-- If default_ty fails, we try
						-- default_tys instead
	  tcSimplifyCheckThetas [] thetas	`thenTc` \ _ ->
	  returnTc default_ty
        where
	  thetas = classes `zip` repeat [default_ty]
    in
	-- See if any default works, and if so bind the type variable to it
	-- If not, add an AmbigErr
    recoverTc (complain dicts `thenNF_Tc_` returnTc EmptyMonoBinds)	$

    try_default default_tys		 	`thenTc` \ chosen_default_ty ->

	-- Bind the type variable and reduce the context, for real this time
    let
	chosen_default_tc_ty = typeToTcType chosen_default_ty	-- Tiresome!
    in
    unifyTauTy chosen_default_tc_ty (mkTyVarTy tyvar)	`thenTc_`
    reduceContext (text "disambig" <+> ppr dicts)
		  try_me [] dicts			`thenTc` \ (binds, frees, ambigs) ->
    ASSERT( null frees && null ambigs )
    warnDefault dicts chosen_default_ty			`thenTc_`
    returnTc binds

  | all isCreturnableClass classes
  = 	-- Default CCall stuff to (); we don't even both to check that () is an 
	-- instance of CReturnable, because we know it is.
    unifyTauTy (mkTyVarTy tyvar) unitTy    `thenTc_`
    returnTc EmptyMonoBinds
    
  | otherwise -- No defaults
  = complain dicts	`thenNF_Tc_`
    returnTc EmptyMonoBinds

  where
    complain    = addAmbigErrs tyVarsOfInst
    try_me inst = ReduceMe AddToIrreds		-- This reduce should not fail
    tyvar       = get_tv (head dicts)		-- Should be non-empty
    classes     = map get_clas dicts
\end{code}

[Aside - why the defaulting mechanism is turned off when
 dealing with arguments and results to ccalls.

When typechecking _ccall_s, TcExpr ensures that the external
function is only passed arguments (and in the other direction,
results) of a restricted set of 'native' types. This is
implemented via the help of the pseudo-type classes,
@CReturnable@ (CR) and @CCallable@ (CC.)
 
The interaction between the defaulting mechanism for numeric
values and CC & CR can be a bit puzzling to the user at times.
For example,

    x <- _ccall_ f
    if (x /= 0) then
       _ccall_ g x
     else
       return ()

What type has 'x' got here? That depends on the default list
in operation, if it is equal to Haskell 98's default-default
of (Integer, Double), 'x' has type Double, since Integer
is not an instance of CR. If the default list is equal to
Haskell 1.4's default-default of (Int, Double), 'x' has type
Int. 

To try to minimise the potential for surprises here, the
defaulting mechanism is turned off in the presence of
CCallable and CReturnable.

]

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

addAmbigErrs ambig_tv_fn dicts = mapNF_Tc (addAmbigErr ambig_tv_fn) dicts

addAmbigErr ambig_tv_fn dict
  = addInstErrTcM (instLoc dict)
	(tidy_env,
	 sep [text "Ambiguous type variable(s)" <+>
			hsep (punctuate comma (map (quotes . ppr) ambig_tvs)),
	      nest 4 (text "in the constraint" <+> quotes (pprInst tidy_dict))])
  where
    ambig_tvs = varSetElems (ambig_tv_fn tidy_dict)
    (tidy_env, tidy_dict) = tidyInst emptyTidyEnv dict

warnDefault dicts default_ty
  | not opt_WarnTypeDefaults
  = returnNF_Tc ()

  | otherwise
  = warnTc True msg
  where
    msg | length dicts > 1 
	= (ptext SLIT("Defaulting the following constraint(s) to type") <+> quotes (ppr default_ty))
	  $$ pprInstsInFull tidy_dicts
	| otherwise
	= ptext SLIT("Defaulting") <+> quotes (pprInst (head tidy_dicts)) <+> 
	  ptext SLIT("to type") <+> quotes (ppr default_ty)

    (_, tidy_dicts) = mapAccumL tidyInst emptyTidyEnv dicts

addRuleLhsErr dict
  = addInstErrTcM (instLoc dict)
	(tidy_env,
	 vcat [ptext SLIT("Could not deduce") <+> quotes (pprInst tidy_dict),
	       nest 4 (ptext SLIT("LHS of a rule must have no overloading"))])
  where
    (tidy_env, tidy_dict) = tidyInst emptyTidyEnv dict

-- Used for top-level irreducibles
addTopInstanceErr dict
  = addInstErrTcM (instLoc dict) 
	(tidy_env, 
	 ptext SLIT("No instance for") <+> quotes (pprInst tidy_dict))
  where
    (tidy_env, tidy_dict) = tidyInst emptyTidyEnv dict

addNoInstanceErr str givens dict
  = addInstErrTcM (instLoc dict) 
	(tidy_env, 
	 sep [ptext SLIT("Could not deduce") <+> quotes (pprInst tidy_dict),
	      nest 4 $ ptext SLIT("from the context:") <+> pprInsts tidy_givens]
	$$
	 ptext SLIT("Probable cause:") <+> 
	      vcat [sep [ptext SLIT("missing") <+> quotes (pprInst tidy_dict),
		    ptext SLIT("in") <+> str],
		    if all_tyvars then empty else
		    ptext SLIT("or missing instance declaration for") <+> quotes (pprInst tidy_dict)]
    )
  where
    all_tyvars = all isTyVarTy tys
    (_, tys)   = getDictClassTys dict
    (tidy_env, tidy_dict:tidy_givens) = tidyInsts emptyTidyEnv (dict:givens)

-- Used for the ...Thetas variants; all top level
addNoInstErr (c,ts)
  = addErrTc (ptext SLIT("No instance for") <+> quotes (pprConstraint c ts))

reduceDepthErr n stack
  = vcat [ptext SLIT("Context reduction stack overflow; size =") <+> int n,
	  ptext SLIT("Use -fcontext-stack20 to increase stack size to (e.g.) 20"),
	  nest 4 (pprInstsInFull stack)]

reduceDepthMsg n stack = nest 4 (pprInstsInFull stack)
\end{code}
