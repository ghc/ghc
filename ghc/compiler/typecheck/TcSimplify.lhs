%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcSimplify]{TcSimplify}

\begin{code}
#include "HsVersions.h"

module TcSimplify (
	tcSimplify, tcSimplifyAndCheck,
	tcSimplifyTop, tcSimplifyThetas, tcSimplifyCheckThetas, tcSimplifyRank2,
	bindInstsOfLocalFuns
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Outputable
import Pretty

import TcMonad		-- typechecking monadic machinery
import TcMonadFns	( newDicts, applyTcSubstAndExpectTyVars )
import AbsSyn		-- the stuff being typechecked

import AbsUniType	( isSuperClassOf, getTyVar, eqTyVar, ltTyVar,
			  instantiateThetaTy, isFunType, getUniDataTyCon,
			  getSuperDictSelId, InstTyEnv(..)
			  IF_ATTACK_PRAGMAS(COMMA isTyVarTy COMMA pprUniType)
			  IF_ATTACK_PRAGMAS(COMMA assocMaybe)
			)
import UniType		( UniType(..) ) -- ******* CHEATING ************
import Disambig		( disambiguateDicts )
import Errors		( reduceErr, genCantGenErr, Error(..) )
import Id		( mkInstId )
import Inst		( extractTyVarsFromInst, isTyVarDict, matchesInst,
			  instBindingRequired, instCanBeGeneralised,
			  Inst(..),	-- We import the CONCRETE type, because
					-- TcSimplify is allowed to see the rep
					-- of Insts
			  InstOrigin, OverloadedLit, InstTemplate
			)
import InstEnv
import LIE
import ListSetOps	( minusList )
import Maybes		( catMaybes, maybeToBool, Maybe(..) )
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
tcSimpl :: Bool				-- True <=> Don't simplify const insts
	-> [TyVar]			-- ``Global'' type variables
	-> [TyVar]			-- ``Local''  type variables
	-> [Inst]			-- Given; these constrain only local tyvars
	-> [Inst]			-- Wanted
	-> TcM ([Inst],			-- Free
		[(Inst,TypecheckedExpr)],-- Bindings
		[Inst])			-- Remaining wanteds; no dups

tcSimpl dont_squash_consts global_tvs local_tvs givens wanteds
  =
	 -- Make sure the insts and type variables are fixed points of the substitution
    applyTcSubstAndExpectTyVars global_tvs `thenNF_Tc` \ global_tvs ->
    applyTcSubstAndExpectTyVars local_tvs  `thenNF_Tc` \ local_tvs ->
    applyTcSubstToInsts givens		 `thenNF_Tc` \ givens ->
    applyTcSubstToInsts wanteds		 `thenNF_Tc` \ wanteds ->
    let
	is_elem1 = isIn "tcSimpl1"
	is_elem2 = isIn "tcSimpl2"
    in
	-- Deal with duplicates and type constructors
    elimTyCons
	 dont_squash_consts (\tv -> tv `is_elem1` global_tvs)
	 givens wanteds		`thenTc` \ (globals, tycon_binds, locals_and_ambigs) ->

	-- Now disambiguate if necessary
    let
	(ambigs, unambigs) = partition (is_ambiguous local_tvs) locals_and_ambigs
	(locals, cant_generalise) = partition instCanBeGeneralised unambigs
    in
    checkTc (not (null cant_generalise)) (genCantGenErr cant_generalise) 	`thenTc_`

    (if (null ambigs) then

	-- No ambiguous dictionaries.  Just bash on with the results
	-- of the elimTyCons
	returnTc (globals, tycon_binds, locals_and_ambigs)

    else

	-- Some ambiguous dictionaries.	 We now disambiguate them,
	-- which binds the offending type variables to suitable types in the
	-- substitution, and then we retry the whole process.  This
	-- time there won't be any ambiguous ones.
	-- There's no need to back-substitute on global and local tvs,
	-- because the ambiguous type variables can't be in either.

	-- Why do we retry the whole process?  Because binding a type variable
	-- to a particular type might enable a short-cut simplification which
	-- elimTyCons will have missed the first time.

	disambiguateDicts ambigs	`thenTc_`
	applyTcSubstToInsts givens	`thenNF_Tc` \ givens ->
	applyTcSubstToInsts wanteds	`thenNF_Tc` \ wanteds ->
	elimTyCons
		dont_squash_consts (\tv -> tv `is_elem2` global_tvs)
		givens wanteds

    ) {- End of the "if" -} `thenTc` \ (globals, tycon_binds, locals) ->

	-- Deal with superclass relationships
    elimSCs givens locals		`thenNF_Tc` \ (sc_binds, locals2) ->

	 -- Finished
    returnTc (globals, sc_binds ++ tycon_binds, locals2)
  where
    is_ambiguous local_tvs (Dict _ _ ty _)
      = getTyVar "is_ambiguous" ty `not_elem` local_tvs
      where
	not_elem = isn'tIn "is_ambiguous"
\end{code}

The main wrapper is @tcSimplify@.  It just calls @tcSimpl@, but with
the ``don't-squash-consts'' flag set depending on top-level ness.  For
top level defns we *do* squash constants, so that they stay local to a
single defn.  This makes things which are inlined more likely to be
exportable, because their constants are "inside".  Later passes will
float them out if poss, after inlinings are sorted out.

\begin{code}
tcSimplify
	:: Bool				-- True <=> top level
	-> [TyVar]			-- ``Global'' type variables
	-> [TyVar]			-- ``Local''  type variables
	-> [Inst]			-- Wanted
	-> TcM ([Inst],			-- Free
		[(Inst, TypecheckedExpr)],-- Bindings
		[Inst])			-- Remaining wanteds; no dups

tcSimplify top_level global_tvs local_tvs wanteds
  = tcSimpl (not top_level) global_tvs local_tvs [] wanteds
\end{code}

@tcSimplifyAndCheck@ is similar to the above, except that it checks
that there is an empty wanted-set at the end.

It may still return some of constant insts, which have
to be resolved finally at the end.

\begin{code}
tcSimplifyAndCheck
	 :: Bool				-- True <=> top level
	 -> [TyVar]				-- ``Global''  type variables
	 -> [TyVar]				-- ``Local''  type variables
	 -> [Inst]				-- Given
	 -> [Inst]				-- Wanted
	 -> UnifyErrContext			-- Context info for error 
	 -> TcM ([Inst],			-- Free
		 [(Inst, TypecheckedExpr)])	-- Bindings

tcSimplifyAndCheck top_level global_tvs local_tvs givens wanteds err_ctxt
  = tcSimpl (not top_level) global_tvs local_tvs givens wanteds
			`thenTc` \ (free_insts, binds, wanteds') ->
    checkTc (not (null wanteds')) (reduceErr wanteds' err_ctxt)
			`thenTc_`
    returnTc (free_insts, binds)
\end{code}

@tcSimplifyRank2@ checks that the argument of a rank-2 polymorphic function
is not overloaded.  

\begin{code}
tcSimplifyRank2 :: [TyVar]		-- ``Local'' type variables; guaranteed fixpoint of subst
		-> [Inst]		-- Given
		-> UnifyErrContext
		-> TcM ([Inst],				-- Free
			[(Inst, TypecheckedExpr)])	-- Bindings

tcSimplifyRank2 local_tvs givens err_ctxt
  = applyTcSubstToInsts givens		 `thenNF_Tc` \ givens' ->
    elimTyCons False 
	       (\tv -> not (tv `is_elem` local_tvs))
		-- This predicate claims that all
		-- any non-local tyvars are global,
		-- thereby postponing dealing with
		-- ambiguity until the enclosing Gen
	       [] givens'	`thenTc` \ (free, dict_binds, wanteds) ->

    checkTc (not (null wanteds)) (reduceErr wanteds err_ctxt)	`thenTc_`

    returnTc (free, dict_binds)
  where
    is_elem = isIn "tcSimplifyRank2"
\end{code}

@tcSimplifyTop@ deals with constant @Insts@, using the standard simplification
mechansim with the extra flag to say ``beat out constant insts''.

\begin{code}
tcSimplifyTop :: [Inst] -> TcM [(Inst, TypecheckedExpr)]
tcSimplifyTop dicts
  = tcSimpl False [] [] [] dicts    `thenTc` \ (_, binds, _) ->
    returnTc binds
\end{code}

@tcSimplifyThetas@ simplifies class-type constraints formed by
@deriving@ declarations and when specialising instances.  We are
only interested in the simplified bunch of class/type constraints.

\begin{code}
tcSimplifyThetas :: (Class -> TauType -> InstOrigin)  -- Creates an origin for the dummy dicts
	       	 -> [(Class, TauType)]		      -- Simplify this
	       	 -> TcM [(Class, TauType)]  	      -- Result

tcSimplifyThetas mk_inst_origin theta
  = let
	dicts = map mk_dummy_dict theta
    in
	 -- Do the business (this is just the heart of "tcSimpl")
    elimTyCons False (\tv -> False) [] dicts    `thenTc`	\ (_, _, dicts2) ->

	  -- Deal with superclass relationships
    elimSCs [] dicts2		    `thenNF_Tc` \ (_, dicts3) ->

    returnTc (map unmk_dummy_dict dicts3)
  where
    mk_dummy_dict (clas, ty)
      = Dict uniq clas ty (mk_inst_origin clas ty)

    uniq = panic "tcSimplifyThetas:uniq"

    unmk_dummy_dict (Dict _ clas ty _) = (clas, ty)
\end{code}

@tcSimplifyCheckThetas@ just checks class-type constraints, essentially;
used with \tr{default} declarations.  We are only interested in
whether it worked or not.

\begin{code}
tcSimplifyCheckThetas :: InstOrigin		-- context; for error msg
		-> [(Class, TauType)]	-- Simplify this
		-> TcM ()

tcSimplifyCheckThetas origin theta
  = let
	dicts = map mk_dummy_dict theta
    in
	 -- Do the business (this is just the heart of "tcSimpl")
    elimTyCons False (\tv -> False) [] dicts    `thenTc`	\ _ ->

    returnTc ()
  where
    mk_dummy_dict (clas, ty)
      = Dict uniq clas ty origin

    uniq = panic "tcSimplifyCheckThetas:uniq"
\end{code}


%************************************************************************
%*									*
\subsection[elimTyCons]{@elimTyCons@}
%*									*
%************************************************************************

\begin{code}
elimTyCons :: Bool				-- True <=> Don't simplify const insts
	   -> (TyVar -> Bool)			-- Free tyvar predicate
	   -> [Inst]				-- Given
	   -> [Inst]				-- Wanted
	   -> TcM ([Inst],			-- Free
		   [(Inst, TypecheckedExpr)],	-- Bindings
		   [Inst]			-- Remaining wanteds; no dups;
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
elimTyCons dont_squash_consts is_free_tv givens wanteds
  = eTC givens wanteds
  where
    eTC :: [Inst] -> [Inst]
	-> TcM ([Inst], [(Inst, TypecheckedExpr)], [Inst])

    eTC _ [] = returnTc ([], [], [])

    eTC givens (wanted:wanteds) = try givens wanted wanteds
				      (extractTyVarsFromInst wanted)
				      (find_equiv givens wanted)
	-- find_equiv looks in "givens" for an inst equivalent to "wanted"
	-- This is used only in Case 2 below; it's like a guard which also
	-- returns a result.

    try :: [Inst] -> Inst -> [Inst] -> [TyVar] -> (Maybe Inst)
	-> TcM ([Inst], [(Inst, TypecheckedExpr)], [Inst])

    -- Case 0: same as existing dict, so build a simple binding
    try givens wanted wanteds tvs_of_wanted (Just this)
     = eTC givens wanteds	`thenTc` \ (frees, binds, wanteds') ->
       let 
	  -- Create a new binding iff it's needed
	  new_binds | instBindingRequired wanted = (wanted, Var (mkInstId this)):binds
	            | otherwise			 = binds
       in
       returnTc (frees, new_binds, wanteds')

    -- Case 1: constrains no type variables at all
    -- In this case we have a quick go to see if it has an
    -- instance which requires no inputs (ie a constant); if so we use
    -- it; if not, we give up on the instance and just heave it out the
    -- top in the free result
    try givens wanted wanteds tvs_of_wanted _ | null tvs_of_wanted
      = simplify_it dont_squash_consts {- If dont_squash_consts is true,
					  simplify only if trival -}
		    givens wanted wanteds

    -- Case 2: constrains free vars only, so fling it out the top in free_ids
    try givens wanted wanteds tvs_of_wanted _
      | all is_free_tv tvs_of_wanted
      = eTC (wanted:givens) wanteds	`thenTc` \ (frees, binds, wanteds') ->
	returnTc (wanted:frees, binds, wanteds')

    -- Case 3: is a dict constraining only a tyvar,
    -- so return it as part of the "wanteds" result
    try givens wanted wanteds tvs_of_wanted _
      | isTyVarDict wanted
      = eTC (wanted:givens) wanteds	`thenTc` \ (frees, binds, wanteds') ->
	returnTc (frees, binds, wanted:wanteds')

    -- Case 4: is not a simple dict, so look up in instance environment
    try givens wanted wanteds tvs_of_wanted _
      = simplify_it False {- Simplify even if not trivial -}
		    givens wanted wanteds

    simplify_it only_if_trivial givens wanted wanteds
      = if not (instBindingRequired wanted) then
		-- No binding required for this chap, so squash right away
	   lookupNoBindInst_Tc wanted	`thenTc` \ simpler_wanteds ->

	   eTC givens simpler_wanteds	`thenTc` \ (frees1, binds1, wanteds1) ->
	   let
	       new_givens = [new_given | (new_given,rhs) <- binds1]
		-- Typically binds1 is empty
	   in
	   eTC givens wanteds		`thenTc` \ (frees2, binds2, wanteds2) ->

	   returnTc (frees1 ++ frees2,
		     binds1 ++ binds2,
		     wanteds1 ++ wanteds2)

	else	-- An binding is required for this inst
	lookupInst_Tc wanted	`thenTc` \ (rhs, simpler_wanteds) ->

        if (only_if_trivial && not_var rhs) then
	   -- Ho ho!  It isn't trivial to simplify "wanted",
	   -- because the rhs isn't a simple variable.	The flag
	   -- dont_squash_consts tells us to give up now and
	   -- just fling it out the top.
	   eTC (wanted:givens) wanteds	`thenTc` \ (frees, binds, wanteds') ->
	   returnTc (wanted:frees, binds, wanteds')
	else
	   -- Aha! Either it's easy, or dont_squash_consts is
	   -- False, so we must do it right here.

	   eTC givens simpler_wanteds	`thenTc` \ (frees1, binds1, wanteds1) ->
	   let
	       new_givens = [new_given | (new_given,rhs) <- binds1]
	   in
	   eTC (new_givens ++ [wanted] ++ wanteds1 ++ givens) wanteds
				   `thenTc` \ (frees2, binds2, wanteds2) ->
	   returnTc (frees1 ++ frees2,
		     binds1 ++ [(wanted, rhs)] ++ binds2,
		     wanteds1 ++ wanteds2)
      where
	not_var :: TypecheckedExpr -> Bool
	not_var (Var _) = False
	not_var other	= True

    find_equiv :: [Inst] -> Inst -> Maybe Inst
	-- Look through the argument list for an inst which is
	-- equivalent to the second arg.

    find_equiv []	      wanted = Nothing
    find_equiv (given:givens) wanted
      | wanted `matchesInst` given = Just given
      | otherwise		   = find_equiv givens wanted
\end{code}


%************************************************************************
%*									*
\subsection[elimSCs]{@elimSCs@}
%*									*
%************************************************************************

\begin{code}
elimSCs :: [Inst]				-- Given; no dups
	-> [Inst]				-- Wanted; no dups; all dictionaries, all
						-- constraining just a type variable
	-> NF_TcM ([(Inst,TypecheckedExpr)],	-- Bindings
		   [Inst])			-- Minimal wanted set

elimSCs givens wanteds
  = -- Sort the wanteds so that subclasses occur before superclasses
    elimSCs_help
	[dict | dict@(Dict _ _ _ _) <- givens]	-- Filter out non-dictionaries
	(sortSC wanteds)

elimSCs_help :: [Inst]				-- Given; no dups
	     -> [Inst]				-- Wanted; no dups;
	     -> NF_TcM ([(Inst,TypecheckedExpr)],-- Bindings
			[Inst])			-- Minimal wanted set

elimSCs_help given [] = returnNF_Tc ([], [])

elimSCs_help givens (wanted@(Dict _ wanted_class wanted_ty wanted_orig):wanteds)
  = case (trySC givens wanted_class wanted_ty) of

      Nothing -> -- No superclass relnship found
		 elimSCs_help (wanted:givens) wanteds `thenNF_Tc` \ (binds, wanteds') ->
		 returnNF_Tc (binds, wanted:wanteds')

      Just (given, classes) ->	-- Aha! There's a superclass relnship

	-- Build intermediate dictionaries
	let
	    theta = [ (clas, wanted_ty) | clas <- classes ]
	in
	newDicts wanted_orig theta		`thenNF_Tc` \ intermediates ->

	-- Deal with the recursive call
	elimSCs_help (wanted : (intermediates ++ givens)) wanteds
						`thenNF_Tc` \ (binds, wanteds') ->

	-- Create bindings for the wanted dictionary and the intermediates.
	-- Later binds may depend on earlier ones, so each new binding is pushed
	-- on the front of the accumulating parameter list of bindings
	let
	    new_binds = mk_binds wanted wanted_class (intermediates ++ [given]) []
	in
	returnNF_Tc (new_binds ++ binds, wanteds')
  where
    mk_binds :: Inst				-- Define this
	     -> Class				-- ...whose class is this
	     -> [Inst]				-- In terms of this sub-class chain
	     -> [(Inst, TypecheckedExpr)]	-- Push the binding on front of these
	     -> [(Inst, TypecheckedExpr)]

    mk_binds dict clas [] binds_so_far = binds_so_far
    mk_binds dict clas (dict_sub@(Dict _ dict_sub_class ty _):dicts_sub) binds_so_far
      = mk_binds dict_sub dict_sub_class dicts_sub (new_bind:binds_so_far)
      where
	new_bind = (dict, DictApp (TyApp (Var (getSuperDictSelId dict_sub_class clas))
					 [ty])
				  [mkInstId dict_sub])


trySC :: [Inst]				-- Givens
      -> Class -> UniType		-- Wanted
      -> Maybe (Inst, [Class])		-- Nothing if no link; Just (given, classes)
					-- if wanted can be given in terms of given, with
					-- intermediate classes specified
trySC givens wanted_class wanted_ty
  = case subclass_relns of
	 [] -> Nothing
	 ((given, classes, _): _) -> Just (given, classes)
  where
    subclass_relns :: [(Inst, [Class], Int)]	-- Subclass of wanted,
						 -- intervening classes,
						 -- and number of intervening classes
						 -- Sorted with shortest link first
    subclass_relns = sortLt reln_lt (catMaybes (map find_subclass_reln givens))

    reln_lt :: (Inst, [Class], Int) -> (Inst, [Class], Int) -> Bool
    (_,_,n1) `reln_lt` (_,_,n2) = n1 < n2

    find_subclass_reln given@(Dict _ given_class given_ty _)
	 | wanted_ty == given_ty
	 = case (wanted_class `isSuperClassOf` given_class) of

		 Just classes -> Just (given,
				       classes,
				       length classes)

		 Nothing      -> Nothing

	 | otherwise = Nothing


sortSC :: [Inst]    -- Expected to be all dicts (no MethodIds), all of
		    -- which constrain type variables
       -> [Inst]    -- Sorted with subclasses before superclasses

sortSC dicts = sortLt lt dicts
  where
    (Dict _ c1 ty1 _) `lt` (Dict _ c2 ty2 _)
       = tv1 `ltTyVar` tv2 ||
	(tv1 `eqTyVar` tv2 && maybeToBool (c2 `isSuperClassOf` c1))
       where
	tv1 = getTyVar "sortSC" ty1
	tv2 = getTyVar "sortSC" ty2
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
@LIE@), as well as the @Binds@ generated.

\begin{code}
bindInstsOfLocalFuns ::	LIE -> [Id] -> NF_TcM (LIE, TypecheckedMonoBinds)

bindInstsOfLocalFuns init_lie local_ids
  = let
	insts = unMkLIE init_lie
    in
    bind_insts insts [] EmptyMonoBinds
  where
    bind_insts	:: [Inst]		-- Insts to mangle
		-> [Inst]		-- accum. Insts to return
		-> TypecheckedMonoBinds -- accum. Binds to return
		-> NF_TcM (LIE, TypecheckedMonoBinds)

    bind_insts [] acc_insts acc_binds
      = returnNF_Tc (mkLIE acc_insts, acc_binds)

    bind_insts (inst@(Method uniq id tys orig):insts) acc_insts acc_binds
      | id `is_elem` local_ids
      = noFailTc (lookupInst_Tc inst)	`thenNF_Tc` \ (expr, dict_insts) ->
	let
	    bind =  VarMonoBind (mkInstId inst) expr
	in
	bind_insts insts (dict_insts ++ acc_insts) (bind `AndMonoBinds` acc_binds)

    bind_insts (some_other_inst:insts) acc_insts acc_binds
	-- Either not a method, or a method instance for an id not in local_ids
      = bind_insts insts (some_other_inst:acc_insts) acc_binds

    is_elem = isIn "bindInstsOfLocalFuns"
\end{code}
