%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcSimplify]{TcSimplify}

\begin{code}
#include "HsVersions.h"

module TcSimplify (
	tcSimplify, tcSimplifyAndCheck,
	tcSimplifyTop, tcSimplifyThetas, tcSimplifyCheckThetas, tcSimplifyRank2,
	bindInstsOfLocalFuns
    ) where

IMP_Ubiq()

import HsSyn		( MonoBinds(..), HsExpr(..), InPat, OutPat, HsLit, 
			  Match, HsBinds, HsType, ArithSeqInfo, Fixity,
			  GRHSsAndBinds, Stmt, DoOrListComp, Fake )
import TcHsSyn		( TcIdOcc(..), SYN_IE(TcIdBndr), SYN_IE(TcExpr), SYN_IE(TcMonoBinds) )

import TcMonad
import Inst		( lookupInst, lookupSimpleInst,
			  tyVarsOfInst, isTyVarDict, isDict,
			  matchesInst, instToId, instBindingRequired,
			  instCanBeGeneralised, newDictsAtLoc,
			  pprInst,
			  Inst(..), SYN_IE(LIE), zonkLIE, emptyLIE,
			  plusLIE, unitLIE, consLIE, InstOrigin(..),
			  OverloadedLit )
import TcEnv		( tcGetGlobalTyVars )
import SpecEnv		( SpecEnv )
import TcType		( SYN_IE(TcType), SYN_IE(TcTyVar), SYN_IE(TcTyVarSet), TcMaybe, tcInstType )
import Unify		( unifyTauTy )

import Bag		( Bag, unitBag, listToBag, foldBag, filterBag, emptyBag, bagToList, 
			  snocBag, consBag, unionBags, isEmptyBag )
import Class		( GenClass, SYN_IE(Class), SYN_IE(ClassInstEnv),
			  isSuperClassOf, classSuperDictSelId, classInstEnv
			)
import Id		( GenId )
import PrelInfo		( isNumericClass, isStandardClass, isCcallishClass )

import Maybes		( expectJust, firstJust, catMaybes, seqMaybe, maybeToBool )
import Outputable	( Outputable(..){-instance * []-} )
--import PprStyle--ToDo:rm
import PprType		( GenType, GenTyVar )
import Pretty
import SrcLoc		( noSrcLoc )
import Type		( GenType, SYN_IE(Type), SYN_IE(TauType), mkTyVarTy, getTyVar, eqSimpleTy,
			  getTyVar_maybe )
import TysWiredIn	( intTy, unitTy )
import TyVar		( GenTyVar, SYN_IE(GenTyVarSet), 
			  elementOfTyVarSet, emptyTyVarSet, unionTyVarSets,
			  isEmptyTyVarSet, tyVarSetToList )
import Unique		( Unique )
import Util
\end{code}


%************************************************************************
%*									*
\subsection[tcSimplify-main]{Main entry function}
%*									*
%************************************************************************

* May modify the substitution to bind ambiguous type variables.

Specification
~~~~~~~~~~~~~
(1) If an inst constrains only ``global'' type variables, (or none),
    return it as a ``global'' inst.

OTHERWISE

(2) Simplify it repeatedly (checking for (1) of course) until it is a dict
    constraining only a type variable.

(3) If it constrains a ``local'' type variable, return it as a ``local'' inst.
    Otherwise it must be ambiguous, so try to resolve the ambiguity.


\begin{code}
tcSimpl :: Bool				-- True <=> simplify const insts
	-> TcTyVarSet s			-- ``Global'' type variables
	-> TcTyVarSet s			-- ``Local''  type variables
					-- ASSERT: both these tyvar sets are already zonked
	-> LIE s			-- Given; these constrain only local tyvars
	-> LIE s			-- Wanted
	-> TcM s (LIE s,			-- Free
		  [(TcIdOcc s,TcExpr s)],	-- Bindings
		  LIE s)			-- Remaining wanteds; no dups

tcSimpl squash_consts global_tvs local_tvs givens wanteds
  =	-- ASSSERT: global_tvs and local_tvs are already zonked
	-- Make sure the insts fixed points of the substitution
    zonkLIE givens		 	`thenNF_Tc` \ givens ->
    zonkLIE wanteds		 	`thenNF_Tc` \ wanteds ->

	-- Deal with duplicates and type constructors
    elimTyCons
	 squash_consts (\tv -> tv `elementOfTyVarSet` global_tvs)
	 givens wanteds		`thenTc` \ (globals, tycon_binds, locals_and_ambigs) ->

   	-- Now disambiguate if necessary
    let
	ambigs = filterBag is_ambiguous locals_and_ambigs
    in
    if not (isEmptyBag ambigs) then
	-- Some ambiguous dictionaries.	 We now disambiguate them,
	-- which binds the offending type variables to suitable types in the
	-- substitution, and then we retry the whole process.  This
	-- time there won't be any ambiguous ones.
	-- There's no need to back-substitute on global and local tvs,
	-- because the ambiguous type variables can't be in either.

	-- Why do we retry the whole process?  Because binding a type variable
	-- to a particular type might enable a short-cut simplification which
	-- elimTyCons will have missed the first time.

	disambiguateDicts ambigs		`thenTc_`
	tcSimpl squash_consts global_tvs local_tvs givens wanteds

    else
	-- No ambiguous dictionaries.  Just bash on with the results
	-- of the elimTyCons

	-- Check for non-generalisable insts
    let
  	locals		= locals_and_ambigs	-- ambigs is empty
	cant_generalise = filterBag (not . instCanBeGeneralised) locals
    in
    checkTc (isEmptyBag cant_generalise)
	    (genCantGenErr cant_generalise)	`thenTc_`


	-- Deal with superclass relationships
    elimSCs givens locals		`thenNF_Tc` \ (sc_binds, locals2) ->

	 -- Finished
    returnTc (globals, bagToList (sc_binds `unionBags` tycon_binds), locals2)
  where
    is_ambiguous (Dict _ _ ty _ _)
	= not (getTyVar "is_ambiguous" ty `elementOfTyVarSet` local_tvs)
\end{code}

The main wrapper is @tcSimplify@.  It just calls @tcSimpl@, but with
the ``don't-squash-consts'' flag set depending on top-level ness.  For
top level defns we *do* squash constants, so that they stay local to a
single defn.  This makes things which are inlined more likely to be
exportable, because their constants are "inside".  Later passes will
float them out if poss, after inlinings are sorted out.

\begin{code}
tcSimplify
	:: TcTyVarSet s			-- ``Local''  type variables
	-> LIE s			-- Wanted
	-> TcM s (LIE s,			-- Free
		  [(TcIdOcc s,TcExpr s)],	-- Bindings
		  LIE s)			-- Remaining wanteds; no dups

tcSimplify local_tvs wanteds
  = tcGetGlobalTyVars			`thenNF_Tc` \ global_tvs ->
    tcSimpl False global_tvs local_tvs emptyBag wanteds
\end{code}

@tcSimplifyAndCheck@ is similar to the above, except that it checks
that there is an empty wanted-set at the end.  It may still return
some of constant insts, which have to be resolved finally at the end.

\begin{code}
tcSimplifyAndCheck
	 :: TcTyVarSet s		-- ``Local''  type variables; ASSERT is fixpoint
	 -> LIE s			-- Given
	 -> LIE s			-- Wanted
	 -> TcM s (LIE s,			-- Free
		   [(TcIdOcc s,TcExpr s)])	-- Bindings

tcSimplifyAndCheck local_tvs givens wanteds
  = tcGetGlobalTyVars			`thenNF_Tc` \ global_tvs ->
    tcSimpl False global_tvs local_tvs
	    givens wanteds		`thenTc` \ (free_insts, binds, wanteds') ->
    checkTc (isEmptyBag wanteds')
	    (reduceErr wanteds')	`thenTc_`
    returnTc (free_insts, binds)
\end{code}

@tcSimplifyRank2@ checks that the argument of a rank-2 polymorphic function
is not overloaded.

\begin{code}
tcSimplifyRank2 :: TcTyVarSet s		-- ``Local'' type variables; ASSERT is fixpoint
		-> LIE s		-- Given
		-> TcM s (LIE s,			-- Free
			  [(TcIdOcc s,TcExpr s)])	-- Bindings


tcSimplifyRank2 local_tvs givens
  = zonkLIE givens			`thenNF_Tc` \ givens' ->
    elimTyCons True
	       (\tv -> not (tv `elementOfTyVarSet` local_tvs))
		-- This predicate claims that all
		-- any non-local tyvars are global,
		-- thereby postponing dealing with
		-- ambiguity until the enclosing Gen
	       emptyLIE givens'	`thenTc` \ (free, dict_binds, wanteds) ->

    checkTc (isEmptyBag wanteds) (reduceErr wanteds)	`thenTc_`

    returnTc (free, bagToList dict_binds)
\end{code}

@tcSimplifyTop@ deals with constant @Insts@, using the standard simplification
mechansim with the extra flag to say ``beat out constant insts''.

\begin{code}
tcSimplifyTop :: LIE s -> TcM s [(TcIdOcc s, TcExpr s)]
tcSimplifyTop dicts
  = tcSimpl True emptyTyVarSet emptyTyVarSet emptyBag dicts	`thenTc` \ (_, binds, _) ->
    returnTc binds
\end{code}

%************************************************************************
%*									*
\subsection[elimTyCons]{@elimTyCons@}
%*									*
%************************************************************************

\begin{code}
elimTyCons :: Bool				-- True <=> Simplify const insts
	   -> (TcTyVar s -> Bool)		-- Free tyvar predicate
	   -> LIE s				-- Given
	   -> LIE s				-- Wanted
	   -> TcM s (LIE s,			-- Free
		     Bag (TcIdOcc s, TcExpr s),	-- Bindings
		     LIE s			-- Remaining wanteds; no dups;
						-- dicts only (no Methods)
	       )
\end{code}

The bindings returned may mention any or all of ``givens'', so the
order in which the generated binds are put together is {\em tricky}.
Case~4 of @try@ is the general case to see.

When we do @eTC givens (wanted:wanteds)@ [some details omitted], we...

    (1) first look up @wanted@; this gives us one binding to heave in:
	    wanted = rhs

    (2) step (1) also gave us some @simpler_wanteds@; we simplify
	these and get some (simpler-wanted-)bindings {\em that must be
	in scope} for the @wanted=rhs@ binding above!

    (3) we simplify the remaining @wanteds@ (recursive call), giving
	us yet more bindings.

The final arrangement of the {\em non-recursive} bindings is

    let <simpler-wanted-binds> in
    let wanted = rhs	       in
    let <yet-more-bindings> ...

\begin{code}
elimTyCons squash_consts is_free_tv givens wanteds
  = eTC givens (bagToList wanteds)	`thenTc` \ (_, free, binds, irreds) ->
    returnTc (free,binds,irreds)
  where
--    eTC :: LIE s -> [Inst s]
--	  -> TcM s (LIE s, LIE s, Bag (TcIdOcc s, TcExpr s), LIE s)

    eTC givens [] = returnTc (givens, emptyBag, emptyBag, emptyBag)

    eTC givens (wanted:wanteds)
    -- Case 0: same as an existing inst
      | maybeToBool maybe_equiv
      = eTC givens wanteds	`thenTc` \ (givens1, frees, binds, irreds) ->
	let
	  -- Create a new binding iff it's needed
	  this = expectJust "eTC" maybe_equiv
	  new_binds | instBindingRequired wanted = (instToId wanted, HsVar (instToId this))
						   `consBag` binds
		    | otherwise			 = binds
	in
	returnTc (givens1, frees, new_binds, irreds)

    -- Case 1: constrains no type variables at all
    -- In this case we have a quick go to see if it has an
    -- instance which requires no inputs (ie a constant); if so we use
    -- it; if not, we give up on the instance and just heave it out the
    -- top in the free result
      | isEmptyTyVarSet tvs_of_wanted
      = simplify_it squash_consts	{- If squash_consts is false,
					   simplify only if trival -}
		    givens wanted wanteds

    -- Case 2: constrains free vars only, so fling it out the top in free_ids
      | all is_free_tv (tyVarSetToList tvs_of_wanted)
      = eTC (wanted `consBag` givens) wanteds	`thenTc` \ (givens1, frees, binds, irreds) ->
	returnTc (givens1, wanted `consBag` frees, binds, irreds)

    -- Case 3: is a dict constraining only a tyvar,
    -- so return it as part of the "wanteds" result
      | isTyVarDict wanted
      = eTC (wanted `consBag` givens) wanteds	`thenTc` \ (givens1, frees, binds, irreds) ->
	returnTc (givens1, frees, binds, wanted `consBag` irreds)

    -- Case 4: is not a simple dict, so look up in instance environment
      | otherwise
      = simplify_it True {- Simplify even if not trivial -}
		    givens wanted wanteds
      where
	tvs_of_wanted  = tyVarsOfInst wanted

	-- Look for something in "givens" that matches "wanted"
	Just the_equiv = maybe_equiv
	maybe_equiv    = foldBag seqMaybe try Nothing givens
	try given | wanted `matchesInst` given = Just given
		  | otherwise		       = Nothing


    simplify_it simplify_always givens wanted wanteds
	-- Recover immediately on no-such-instance errors
      = recoverTc (returnTc (wanted `consBag` givens, emptyLIE, emptyBag, emptyLIE)) 
		  (simplify_one simplify_always givens wanted)
				`thenTc` \ (givens1, frees1, binds1, irreds1) ->
	eTC givens1 wanteds	`thenTc` \ (givens2, frees2, binds2, irreds2) ->
	returnTc (givens2, frees1 `plusLIE` frees2,
			   binds1 `unionBags` binds2,
		  	   irreds1 `plusLIE` irreds2)


    simplify_one simplify_always givens wanted
     | not (instBindingRequired wanted)
     = 		-- No binding required for this chap, so squash right away
	   lookupInst wanted		`thenTc` \ (simpler_wanteds, _) ->
	   eTC givens simpler_wanteds	`thenTc` \ (givens1, frees1, binds1, irreds1) ->
	   returnTc (wanted `consBag` givens1, frees1, binds1, irreds1)

     | otherwise
     = 		-- An binding is required for this inst
	lookupInst wanted		`thenTc` \ (simpler_wanteds, bind@(_,rhs)) ->

	if (not_var rhs && not simplify_always) then
	   -- Ho ho!  It isn't trivial to simplify "wanted",
	   -- because the rhs isn't a simple variable.	Unless the flag
	   -- simplify_always is set, just give up now and
	   -- just fling it out the top.
	   returnTc (wanted `consLIE` givens, unitLIE wanted, emptyBag, emptyLIE)
	else
	   -- Aha! Either it's easy, or simplify_always is True
	   -- so we must do it right here.
	   eTC givens simpler_wanteds	`thenTc` \ (givens1, frees1, binds1, irreds1) ->
	   returnTc (wanted `consLIE` givens1, frees1,
		     binds1 `snocBag` bind,
		     irreds1)

    not_var :: TcExpr s -> Bool
    not_var (HsVar _) = False
    not_var other     = True
\end{code}


%************************************************************************
%*									*
\subsection[elimSCs]{@elimSCs@}
%*									*
%************************************************************************

\begin{code}
elimSCs :: LIE s				-- Given; no dups
	-> LIE s				-- Wanted; no dups; all dictionaries, all
						-- constraining just a type variable
	-> NF_TcM s (Bag (TcIdOcc s,TcExpr s),	-- Bindings
		     LIE s)			-- Minimal wanted set

elimSCs givens wanteds
  = -- Sort the wanteds so that subclasses occur before superclasses
    elimSCs_help
	(filterBag isDict givens)	-- Filter out non-dictionaries
	(sortSC wanteds)

elimSCs_help :: LIE s					-- Given; no dups
	     -> [Inst s]				-- Wanted; no dups;
	     -> NF_TcM s (Bag (TcIdOcc s, TcExpr s),	-- Bindings
		    	  LIE s)			-- Minimal wanted set

elimSCs_help given [] = returnNF_Tc (emptyBag, emptyLIE)

elimSCs_help givens (wanted:wanteds)
  = trySC givens wanted 		`thenNF_Tc` \ (givens1, binds1, irreds1) ->
    elimSCs_help givens1 wanteds	`thenNF_Tc` \ (binds2, irreds2) ->
    returnNF_Tc (binds1 `unionBags` binds2, irreds1 `plusLIE` irreds2)


trySC :: LIE s				-- Givens
      -> Inst s				-- Wanted
      -> NF_TcM s (LIE s,			-- New givens,
		   Bag (TcIdOcc s,TcExpr s),	-- Bindings
		   LIE s)			-- Irreducible wanted set

trySC givens wanted@(Dict _ wanted_class wanted_ty wanted_orig loc)
  | not (maybeToBool maybe_best_subclass_chain)
  = 	-- No superclass relationship
    returnNF_Tc ((wanted `consLIE` givens), emptyBag, unitLIE wanted)

  | otherwise
  = 	-- There's a subclass relationship with a "given"
	-- Build intermediate dictionaries
    let
	theta = [ (clas, wanted_ty) | clas <- reverse classes ]
	-- The reverse is because the list comes back in the "wrong" order I think
    in
    newDictsAtLoc wanted_orig loc theta		`thenNF_Tc` \ (intermediates, _) ->

	-- Create bindings for the wanted dictionary and the intermediates.
	-- Later binds may depend on earlier ones, so each new binding is pushed
	-- on the front of the accumulating parameter list of bindings
    let
	mk_bind (dict,clas) dict_sub@(Dict _ dict_sub_class ty _ _)
	  = ((dict_sub, dict_sub_class),
	     (instToId dict, DictApp (TyApp (HsVar (RealId (classSuperDictSelId dict_sub_class 
									      clas)))
					    [ty])
				     [instToId dict_sub]))
	(_, new_binds) = mapAccumR mk_bind (wanted,wanted_class) (given : intermediates)
    in
    returnNF_Tc (wanted `consLIE` givens `plusLIE` listToBag intermediates,
	         listToBag new_binds,
	         emptyLIE)

  where
    maybe_best_subclass_chain = foldBag choose_best find_subclass_chain Nothing givens
    Just (given, classes, _) = maybe_best_subclass_chain

    choose_best c1@(Just (_,_,n1)) c2@(Just (_,_,n2)) | n1 <= n2  = c1
						      | otherwise = c2
    choose_best Nothing		   c2				  = c2
    choose_best c1		   Nothing		  	  = c1

    find_subclass_chain given@(Dict _ given_class given_ty _ _)
	 | wanted_ty `eqSimpleTy` given_ty
	 = case (wanted_class `isSuperClassOf` given_class) of

		 Just classes -> Just (given,
				       classes,
				       length classes)

		 Nothing      -> Nothing

	 | otherwise = Nothing


sortSC :: LIE s     -- Expected to be all dicts (no MethodIds), all of
		    -- which constrain type variables
       -> [Inst s]  -- Sorted with subclasses before superclasses

sortSC dicts = sortLt lt (bagToList dicts)
  where
    (Dict _ c1 ty1 _ _) `lt` (Dict _ c2 ty2 _ _)
       = maybeToBool (c2 `isSuperClassOf` c1)
	-- The ice is a bit thin here because this "lt" isn't a total order
	-- But it *is* transitive, so it works ok
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

\begin{code}
tcSimplifyThetas :: (Class -> ClassInstEnv)		-- How to find the ClassInstEnv
	       	 -> [(Class, TauType)]			-- Given
	       	 -> [(Class, TauType)]			-- Wanted
	       	 -> TcM s [(Class, TauType)]


tcSimplifyThetas inst_mapper given wanted
  = elimTyConsSimple inst_mapper wanted	`thenTc`    \ wanted1 ->
    returnTc (elimSCsSimple given wanted1)
\end{code}

@tcSimplifyCheckThetas@ just checks class-type constraints, essentially;
used with \tr{default} declarations.  We are only interested in
whether it worked or not.

\begin{code}
tcSimplifyCheckThetas :: [(Class, TauType)]	-- Simplify this to nothing at all
		      -> TcM s ()

tcSimplifyCheckThetas theta
  = elimTyConsSimple classInstEnv theta    `thenTc`	\ theta1 ->
    ASSERT( null theta1 )
    returnTc ()
\end{code}


\begin{code}
elimTyConsSimple :: (Class -> ClassInstEnv) 
	         -> [(Class,Type)]
	         -> TcM s [(Class,Type)]
elimTyConsSimple inst_mapper theta
  = elim theta
  where
    elim []	          = returnTc []
    elim ((clas,ty):rest) = elim_one clas ty 	`thenTc` \ r1 ->
			    elim rest		`thenTc` \ r2 ->
			    returnTc (r1++r2)

    elim_one clas ty
	= case getTyVar_maybe ty of

	    Just tv   -> returnTc [(clas,ty)]

	    otherwise -> recoverTc (returnTc []) $
			 lookupSimpleInst (inst_mapper clas) clas ty	`thenTc` \ theta ->
			 elim theta

elimSCsSimple :: [(Class,Type)] 	-- Given
	      -> [(Class,Type)]		-- Wanted
	      -> [(Class,Type)]		-- Subset of wanted; no dups, no subclass relnships

elimSCsSimple givens [] = []
elimSCsSimple givens (c_t@(clas,ty) : rest)
  | any (`subsumes` c_t) givens ||
    any (`subsumes` c_t) rest				-- (clas,ty) is old hat
  = elimSCsSimple givens rest
  | otherwise						-- (clas,ty) is new
  = c_t : elimSCsSimple (c_t : givens) rest
  where
    rest' = elimSCsSimple rest
    (c1,t1) `subsumes` (c2,t2) = t1 `eqSimpleTy` t2 && 
				 (c1 == c2 || maybeToBool (c2 `isSuperClassOf` c1))
-- We deal with duplicates here   ^^^^^^^^
-- It's a simple place to do it, although it's done in elimTyCons in the
-- full-blown version of the simpifier.
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
  = foldrTc bind_inst (emptyBag, EmptyMonoBinds) (bagToList init_lie)
  where
    bind_inst inst@(Method uniq (TcId id) tys rho orig loc) (insts, binds)
      | id `is_elem` local_ids
      = lookupInst inst		`thenTc` \ (dict_insts, (id,rhs)) ->
	returnTc (listToBag dict_insts `plusLIE` insts, 
		  VarMonoBind id rhs `AndMonoBinds` binds)

    bind_inst some_other_inst (insts, binds)
	-- Either not a method, or a method instance for an id not in local_ids
      = returnTc (some_other_inst `consBag` insts, binds)

    is_elem = isIn "bindInstsOfLocalFuns"
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

IMPORTANT: @disambiguate@ assumes that its argument dictionaries
constrain only a simple type variable.

\begin{code}
type SimpleDictInfo s = (Inst s, Class, TcTyVar s)

disambiguateDicts :: LIE s -> TcM s ()

disambiguateDicts insts
  = mapTc disambigOne inst_infos    `thenTc` \ binds_lists ->
    returnTc ()
  where
    inst_infos = equivClasses cmp_tyvars (map mk_inst_info (bagToList insts))
    (_,_,tv1) `cmp_tyvars` (_,_,tv2) = tv1 `cmp` tv2

    mk_inst_info dict@(Dict _ clas ty _ _)
      = (dict, clas, getTyVar "disambiguateDicts" ty)
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
disambigOne :: [SimpleDictInfo s] -> TcM s ()

disambigOne dict_infos
  |  any isNumericClass classes && all isStandardClass classes
  = 	-- THE DICTS OBEY THE DEFAULTABLE CONSTRAINT
	-- SO, TRY DEFAULT TYPES IN ORDER

	-- Failure here is caused by there being no type in the
	-- default list which can satisfy all the ambiguous classes.
	-- For example, if Real a is reqd, but the only type in the
	-- default list is Int.
    tcGetDefaultTys			`thenNF_Tc` \ default_tys ->
    let
      try_default [] 	-- No defaults work, so fail
	= failTc (ambigErr dicts) 

      try_default (default_ty : default_tys)
	= tryTc (try_default default_tys) $	-- If default_ty fails, we try
						-- default_tys instead
	  tcSimplifyCheckThetas thetas	`thenTc` \ _ ->
	  returnTc default_ty
        where
	  thetas = classes `zip` repeat default_ty
    in
	-- See if any default works, and if so bind the type variable to it
    try_default default_tys		`thenTc` \ chosen_default_ty ->
    tcInstType [] chosen_default_ty	`thenNF_Tc` \ chosen_default_tc_ty ->	-- Tiresome!
    unifyTauTy chosen_default_tc_ty (mkTyVarTy tyvar)

  | all isCcallishClass classes
  = 	-- Default CCall stuff to (); we don't even both to check that () is an 
	-- instance of CCallable/CReturnable, because we know it is.
    unifyTauTy (mkTyVarTy tyvar) unitTy    
    
  | otherwise -- No defaults
  = failTc (ambigErr dicts)

  where
    (_,_,tyvar) = head dict_infos		-- Should be non-empty
    dicts   = [dict | (dict,_,_) <- dict_infos]
    classes = [clas | (_,clas,_) <- dict_infos]

\end{code}



Errors and contexts
~~~~~~~~~~~~~~~~~~~
ToDo: for these error messages, should we note the location as coming
from the insts, or just whatever seems to be around in the monad just
now?

\begin{code}
genCantGenErr insts sty	-- Can't generalise these Insts
  = ppHang (ppPStr SLIT("Cannot generalise these overloadings (in a _ccall_):")) 
	   4  (ppAboves (map (ppr sty) (bagToList insts)))
\end{code}

\begin{code}
ambigErr insts sty
  = ppAboves (map (pprInst sty "Ambiguous overloading") insts)
\end{code}

@reduceErr@ complains if we can't express required dictionaries in
terms of the signature.

\begin{code}
reduceErr insts sty
  = ppAboves (map (pprInst sty "Context required by inferred type, but missing on a type signature")
	          (bagToList insts))
\end{code}


