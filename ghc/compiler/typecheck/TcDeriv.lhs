%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[TcDeriv]{Deriving}

Handles @deriving@ clauses on @data@ declarations.

********** Don't forget

Multi-instance checking in renamer should include deriving.

\begin{code}
#include "HsVersions.h"

module TcDeriv (
	tcDeriving,
	con2tag_PN, tag2con_PN, maxtag_PN,
	TagThingWanted(..), DerivEqn(..)
    ) where

IMPORT_Trace		-- ToDo:rm debugging
import Outputable
import Pretty

import TcMonad		-- typechecking monad machinery
import TcMonadFns	( copyTyVars )
import AbsSyn		-- the stuff being typechecked
import TcGenDeriv	-- support code that generates all the grimy bindings
			-- for derived instance decls.

import AbsPrel		( mkFunTy )
import AbsUniType
import UniType		( UniType(..) ) -- *********** CHEATING!!! ****************
import Bag
import CE		( CE(..) )
import CmdLineOpts	( GlobalSwitch(..) )
import E		( E )
import Errors
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- InstancePragmas(..)
import Id		( getDataConSig, isNullaryDataCon, DataCon(..) )
import IdInfo
import Inst		( InstOrigin(..) )
import InstEnv
import Maybes		( assocMaybe, maybeToBool, Maybe(..) )
import NameTypes	( mkFullName, mkPreludeCoreName,
			  Provenance(..), FullName, ShortName
			)
import ProtoName	( eqProtoName, ProtoName(..), Name )
import RenameAuxFuns	-- why not? take all of it...
import RenameBinds4	( rnMethodBinds4, rnTopBinds4 )
import RenameMonad4	-- initRn4, etc.
import SrcLoc		( mkGeneratedSrcLoc, mkUnknownSrcLoc, SrcLoc )
import TCE		-- ( rngTCE, TCE(..), UniqFM )
import TcInstDcls	( InstInfo(..), buildInstanceEnvs, mkInstanceRelatedIds )
import TcSimplify	( tcSimplifyThetas )
import Unique		-- *Key stuff
import Util
\end{code}

%************************************************************************
%*									*
\subsection[TcDeriv-intro]{Introduction to how we do deriving}
%*									*
%************************************************************************

Consider

	data T a b = C1 (Foo a) (Bar b) 
		   | C2 Int (T b a) 
		   | C3 (T a a)
		   deriving (Eq)

We want to come up with an instance declaration of the form

	instance (Ping a, Pong b, ...) => Eq (T a b) where
		x == y = ...

It is pretty easy, albeit tedious, to fill in the code "...".  The
trick is to figure out what the context for the instance decl is,
namely @Ping@, @Pong@ and friends.

Let's call the context reqd for the T instance of class C at types
(a,b, ...)  C (T a b).  Thus:

	Eq (T a b) = (Ping a, Pong b, ...)

Now we can get a (recursive) equation from the @data@ decl:

	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

Foo and Bar may have explicit instances for @Eq@, in which case we can
just substitute for them.  Alternatively, either or both may have
their @Eq@ instances given by @deriving@ clauses, in which case they
form part of the system of equations.

Now all we need do is simplify and solve the equations, iterating to
find the least fixpoint.  Notice that the order of the arguments can
switch around, as here in the recursive calls to T.

Let's suppose Eq (Foo a) = Eq a, and Eq (Bar b) = Ping b.

We start with:

	Eq (T a b) = {}		-- The empty set

Next iteration:
	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

	After simplification:
		   = Eq a u Ping b u {} u {} u {}
		   = Eq a u Ping b

Next iteration:

	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

	After simplification:
		   = Eq a u Ping b 
		   u (Eq b u Ping a)
		   u (Eq a u Ping a)
		
		   = Eq a u Ping b u Eq b u Ping a

The next iteration gives the same result, so this is the fixpoint.  We
need to make a canonical form of the RHS to ensure convergence.  We do
this by simplifying the RHS to a form in which

	- the classes constrain only tyvars
	- the list is sorted by tyvar (major key) and then class (minor key)
	- no duplicates, of course

So, here are the synonyms for the ``equation'' structures:

\begin{code}
type DerivEqn = (Class, TyCon, [TyVar], DerivRhs)
			 -- The tyvars bind all the variables in the RHS
			 -- NEW: it's convenient to re-use InstInfo
			 -- We'll "panic" out some fields...

type DerivRhs = [(Class, TauType)]	-- Same as a ThetaType!

type DerivSoln = DerivRhs
\end{code}

%************************************************************************
%*									*
\subsection[TcDeriv-driver]{Top-level function for \tr{derivings}}
%*									*
%************************************************************************

\begin{code}
tcDeriving  :: FAST_STRING		-- name of module under scrutiny
	    -> GlobalNameFuns		-- for "renaming" bits of generated code
	    -> Bag InstInfo		-- What we already know about instances
	    -> TCE			-- All known TyCon info
	    -> [RenamedFixityDecl]	-- Fixity info; may be used for Text
	    -> TcM (Bag InstInfo,	-- The generated "instance decls".
		    RenamedBinds,	-- Extra generated bindings
		    PprStyle -> Pretty)	-- Printable derived instance decls;
				     	-- for debugging via -ddump-derivings.

tcDeriving modname renamer_name_funs inst_decl_infos_in tce fixities
  =	-- Fish the "deriving"-related information out of the TCE,
	-- from which we make the necessary "equations".
    makeDerivEqns tce	    `thenTc` \ eqns ->

	-- Take the equation list and solve it, to deliver a list of
	-- solutions, a.k.a. the contexts for the instance decls
	-- required for the corresponding equations.
    solveDerivEqns modname inst_decl_infos_in eqns
			    `thenTc` \ new_inst_infos ->

	-- Now augment the InstInfos, adding in the rather boring
	-- actual-code-to-do-the-methods binds.  We may also need to
	-- generate extra not-one-inst-decl-specific binds, notably
	-- "con2tag" and/or "tag2con" functions.  We do these
	-- separately.

    gen_taggery_Names eqns			  `thenTc` \ nm_alist_etc ->
    let
	nm_alist = [ (pn, n) | (pn,n,_,_) <- nm_alist_etc ]

	-- We have the renamer's final "name funs" in our hands
	-- (they were passed in).  So we can handle ProtoNames
	-- that refer to anything "out there".  But our generated
	-- code may also mention "con2tag" (etc.).  So we need
	-- to augment to "name funs" to include those.
	(rn_val_gnf, rn_tc_gnf) = renamer_name_funs

	deriv_val_gnf pname = case (assoc_maybe nm_alist pname) of
				Just xx -> Just xx
				Nothing -> rn_val_gnf pname

	deriver_name_funs = (deriv_val_gnf, rn_tc_gnf)

	assoc_maybe [] _ = Nothing
	assoc_maybe ((v,xxx) : vs) key
	   = if v `eqProtoName` key then Just xxx else assoc_maybe vs key
    in
    gen_tag_n_con_binds deriver_name_funs nm_alist_etc `thenTc` \ extra_binds ->

    mapTc (gen_inst_info modname fixities deriver_name_funs) new_inst_infos
						  `thenTc` \ really_new_inst_infos ->

    returnTc (listToBag really_new_inst_infos,
	      extra_binds,
	      ddump_deriving really_new_inst_infos extra_binds)
  where
    ddump_deriving :: [InstInfo] -> RenamedBinds -> (PprStyle -> Pretty)

    ddump_deriving inst_infos extra_binds sty
      = ppAboves ((map (pp_1 sty) inst_infos) ++ [ppr sty extra_binds])
      where
        pp_1 sty (InstInfo clas tv_tmpls ty inst_decl_theta _ _ _ mbinds _ _ _ _)
	  = ppAbove (ppr sty (mkSigmaTy tv_tmpls inst_decl_theta 
				  (UniDict clas ty)))
		    (ppr sty mbinds)
\end{code}


%************************************************************************
%*									*
\subsection[TcDeriv-eqns]{Forming the equations}
%*									*
%************************************************************************

@makeDerivEqns@ fishes around to find the info about needed derived
instances.  Complicating factors:
\begin{itemize}
\item
We can only derive @Enum@ if the data type is an enumeration
type (all nullary data constructors).

\item
We can only derive @Ix@ if the data type is an enumeration {\em
or} has just one data constructor (e.g., tuples).
\end{itemize}

[See Appendix~E in the Haskell~1.2 report.] This code here deals w/
all those.

\begin{code}
makeDerivEqns :: TCE -> TcM [DerivEqn]

makeDerivEqns tce
  = let
	think_about_deriving = need_deriving (rngTCE tce)
    in
    mapTc (chk_out think_about_deriving) think_about_deriving `thenTc_`

    let 
	(derive_these, _) = removeDups cmp think_about_deriving 
    in

    listNF_Tc (map mk_eqn derive_these)		`thenNF_Tc` \ eqns ->

    returnTc eqns
  where
    ------------------------------------------------------------------
    need_deriving :: [TyCon] -> [(Class, TyCon)]
	-- find the tycons that have `deriving' clauses

    need_deriving tycons_to_consider
      = foldr ( \ tycon acc ->
		   case (getTyConDerivings tycon) of
		     [] -> acc
		     cs -> [ (clas,tycon) | clas <- cs ] ++ acc
	      )
	      []		-- init accumulator
	      tycons_to_consider

    ------------------------------------------------------------------
    chk_out :: [(Class, TyCon)] -> (Class, TyCon) -> TcM ()

    chk_out whole_deriving_list this_one@(clas, tycon)
      =	    -- Are the relevant superclasses catered for?
	    -- E.g., for "... deriving Ord", is there an
	    -- instance of "Eq"?
	let
	    (_, super_classes, _) = getClassSig clas
	    clas_key = getClassKey clas
    	in

	    -- Are things OK for deriving Enum (if appropriate)?
	checkTc (clas_key == enumClassKey && not (isEnumerationTyCon tycon))
		(derivingEnumErr tycon)			`thenTc_`

	    -- Are things OK for deriving Ix (if appropriate)?
	checkTc (clas_key == ixClassKey
	     && not (isEnumerationTyCon tycon
	          || maybeToBool (maybeSingleConstructorTyCon tycon)))
		(derivingIxErr tycon)

    ------------------------------------------------------------------
    cmp :: (Class, TyCon) -> (Class, TyCon) -> TAG_

    cmp (c1, t1) (c2, t2)
      = case cmpClass c1 c2 of
	  EQ_   -> cmpTyCon t1 t2
	  other -> other

    ------------------------------------------------------------------
    mk_eqn :: (Class, TyCon) -> NF_TcM DerivEqn
	-- we swizzle the tyvars, data cons, etc., out of the tycon,
	-- to make the rest of the equation

    mk_eqn (clas, tycon)
      = let
	    tyvar_tmpls	 = getTyConTyVarTemplates tycon
	    data_cons	 = getTyConDataCons tycon
        in
	copyTyVars tyvar_tmpls	`thenNF_Tc` \ (_, tyvars, tyvar_tys) ->

	let 
	    constraints = concat [mk_constraints tyvar_tys con | con <- data_cons]
	in
	returnNF_Tc (clas, tycon, tyvars, constraints)
      where
	mk_constraints tyvar_tys data_con 
	   = [ (clas, instantiateTy inst_env arg_ty)
	     | arg_ty <- arg_tys,
	       not (isPrimType arg_ty)	-- No constraints for primitive types
	     ]
	   where
	     (con_tyvar_tmpls, _, arg_tys, _) = getDataConSig data_con
	     inst_env = con_tyvar_tmpls `zipEqual` tyvar_tys
			-- Type vars in data contructor should be same in number
			-- as in the type contsructor!
\end{code}

%************************************************************************
%*									*
\subsection[TcDeriv-fixpoint]{Finding the fixed point of \tr{deriving} equations}
%*									*
%************************************************************************

A ``solution'' (to one of the equations) is a list of (k,UniTyVar tv)
terms, which is the final correct RHS for the corresponding original
equation.
\begin{itemize}
\item
Each (k,UniTyVarTemplate tv) in a solution constrains only a type
variable, tv.

\item
The (k,UniTyVarTemplate tv) pairs in a solution are canonically
ordered by sorting on type varible, tv, (major key) and then class, k,
(minor key)
\end{itemize}

\begin{code}
solveDerivEqns :: FAST_STRING
	       -> Bag InstInfo
	       -> [DerivEqn] 
	       -> TcM [InstInfo]	-- Solns in same order as eqns.
				  	-- This bunch is Absolutely minimal...

solveDerivEqns modname inst_decl_infos_in orig_eqns
  = iterateDeriv initial_solutions
  where
	-- The initial solutions for the equations claim that each
	-- instance has an empty context; this solution is certainly
	-- in canonical form.
    initial_solutions :: [DerivSoln]
    initial_solutions = [ [] | _ <- orig_eqns ]

	-- iterateDeriv calculates the next batch of solutions,
	-- compares it with the current one; finishes if they are the
	-- same, otherwise recurses with the new solutions.

    iterateDeriv :: [DerivSoln] ->TcM [InstInfo]

    iterateDeriv current_solns
      =	    -- Extend the inst info from the explicit instance decls 
	    -- with the current set of solutions, giving a

	add_solns modname inst_decl_infos_in orig_eqns current_solns
				`thenTc` \ (new_inst_infos, inst_mapper) ->

	    -- Simplify each RHS, using a DerivingOrigin containing an
	    -- inst_mapper reflecting the previous solution
	let
	    mk_deriv_origin clas ty
	      = DerivingOrigin inst_mapper clas is_fun_type tycon locn
	      where
		is_fun_type = isFunType ty
		(tycon,_,_) = getUniDataTyCon ty
		locn = if is_fun_type then mkUnknownSrcLoc{-sigh-} else getSrcLoc tycon
	in
	listTc [ tcSimplifyThetas mk_deriv_origin rhs
	       | (_, _, _, rhs) <- orig_eqns
	       ]		`thenTc` \ next_solns ->

	    -- Canonicalise the solutions, so they compare nicely
	let canonicalised_next_solns
	      = [ sortLt less_than next_soln | next_soln <- next_solns ] in

	if current_solns == canonicalised_next_solns then
	  returnTc new_inst_infos
        else
	  iterateDeriv canonicalised_next_solns

      where
	------------------------------------------------------------------
	less_than :: (Class, TauType) -> (Class, TauType) -> Bool

	less_than (clas1, UniTyVar tv1) (clas2, UniTyVar tv2)
	  = tv1 < tv2 || (tv1 == tv2 && clas1 < clas2)
#ifdef DEBUG
	less_than other_1 other_2
	  = pprPanic "tcDeriv:less_than:" (ppCat [ppr PprDebug other_1, ppr PprDebug other_2])
#endif
\end{code}

\begin{code}
add_solns :: FAST_STRING
	  -> Bag InstInfo			-- The global, non-derived ones
	  -> [DerivEqn] -> [DerivSoln]
	  -> TcM ([InstInfo], 			-- The new, derived ones
		  InstanceMapper)
    -- the eqns and solns move "in lockstep"; we have the eqns
    -- because we need the LHS info for addClassInstance.

add_solns modname inst_infos_in eqns solns
  = listTc (zipWith mk_deriv_inst_info eqns solns) `thenTc` \ new_inst_infos ->

    buildInstanceEnvs (inst_infos_in `unionBags` 
		       listToBag new_inst_infos) `thenTc` \ inst_mapper ->

    returnTc (new_inst_infos, inst_mapper)
  where
    mk_deriv_inst_info (clas, tycon, tyvars, _) theta
	-- The complication here is rather boring: InstInfos need TyVarTemplates,
	-- and we have only TyVars in our hand.
      = let
	    tyvar_tmpls 	= mkTemplateTyVars tyvars
	    tv_tmpl_tys 	= map mkTyVarTemplateTy tyvar_tmpls

	    env			= tyvars `zipEqual` tv_tmpl_tys
	   
	    tycon_tmpl_ty	= applyTyCon tycon tv_tmpl_tys
	    theta_tmpl		= [(clas, mapOverTyVars to_tmpl ty) | (clas,ty) <- theta]

	    to_tmpl = assoc "mk_deriv_inst_info" env

	    (class_tyvar, super_classes, _, class_ops, _, _) = getClassBigSig clas
	in
	returnTc (
	  InstInfo clas tyvar_tmpls tycon_tmpl_ty 
		theta_tmpl
		theta_tmpl		-- Blarg.  This is the dfun_theta slot,
					-- which is needed by buildInstanceEnv;
					-- This works ok for solving the eqns, and
					-- gen_eqns sets it to its final value	
					-- (incl super class dicts) before we
					-- finally return it.
#ifndef DEBUG
		(panic "add_soln:dfun_id") (panic "add_soln:const_meth_ids")
		(panic "add_soln:binds")   (panic "add_soln:from_here")
		(panic "add_soln:modname") mkGeneratedSrcLoc
		(panic "add_soln:upragmas")
	)
#else
		bottom bottom bottom bottom bottom mkGeneratedSrcLoc bottom
	)
      where
	bottom = panic "add_soln"
#endif
\end{code}

%************************************************************************
%*									*
\subsection[TcDeriv-normal-binds]{Bindings for the various classes}
%*									*
%************************************************************************

After all the trouble to figure out the required context for the
derived instance declarations, all that's left is to chug along to
produce them.  They will then be shoved into @tcInstDecls2@, which
will do all its usual business.

There are lots of possibilities for code to generate.  Here are
various general remarks.

PRINCIPLES:
\begin{itemize}
\item
We want derived instances of @Eq@ and @Ord@ (both v common) to be
``you-couldn't-do-better-by-hand'' efficient.

\item
Deriving @Text@---also pretty common, usually just for
@show@---should also be reasonable good code.

\item
Deriving for the other classes isn't that common or that big a deal.
\end{itemize}

PRAGMATICS:

\begin{itemize}
\item
Deriving @Ord@ is done mostly with our non-standard @tagCmp@ method.

\item
Deriving @Eq@ also uses @tagCmp@, if we're deriving @Ord@, too.

\item
We {\em normally} generated code only for the non-defaulted methods;
there are some exceptions for @Eq@ and (especially) @Ord@...

\item
Sometimes we use a @_con2tag_<tycon>@ function, which returns a data
constructor's numeric (@Int#@) tag.  These are generated by
@gen_tag_n_con_binds@, and the heuristic for deciding if one of
these is around is given by @hasCon2TagFun@.


The examples under the different sections below will make this
clearer.

\item
Much less often (really just for deriving @Ix@), we use a
@_tag2con_<tycon>@ function.  See the examples.

\item
We use Pass~4 of the renamer!!!  Reason: we're supposed to be
producing @RenamedMonoBinds@ for the methods, but that means
producing correctly-uniquified code on the fly.  This is entirely
possible (the @TcM@ monad has a @UniqueSupply@), but it is painful.
So, instead, we produce @ProtoNameMonoBinds@ then heave 'em through
the renamer.  What a great hack!
\end{itemize}

\begin{code}
gen_inst_info :: FAST_STRING		-- Module name
	      -> [RenamedFixityDecl]	-- all known fixities;
					-- may be needed for Text
	      -> GlobalNameFuns		-- lookup stuff for names we may use
	      -> InstInfo		-- the main stuff to work on
	      -> TcM InstInfo		-- the gen'd (filled-in) "instance decl"

gen_inst_info modname fixities deriver_name_funs
    info@(InstInfo clas tyvar_tmpls ty inst_decl_theta _ _ _ _ _ _ locn _)
  = 
	-- Generate the various instance-related Ids
    mkInstanceRelatedIds
		(panic "add_solns:E")
			-- These two are only needed if there are pragmas to typecheck;
			-- but there ain't since we are generating the code right here.
		True {-yes, from_here-}
		modname
		NoInstancePragmas
		mkGeneratedSrcLoc
		clas
		tyvar_tmpls ty
		inst_decl_theta
		[{-no user pragmas-}]
			`thenTc` \ (dfun_id, dfun_theta, const_meth_ids) ->

	-- Generate the bindings for the new instance declaration, 
	-- rename it, and check for errors
    getSwitchCheckerTc	`thenNF_Tc` \ sw_chkr ->
    let
	(tycon,_,_)  = getUniDataTyCon ty

	omit_readsPrec = sw_chkr OmitDerivedRead

	proto_mbinds
	  = if	    clas_key == textClassKey	then gen_Text_binds fixities omit_readsPrec tycon
	    else if clas_key == eqClassKey     	then gen_Eq_binds tycon
	    else if clas_key == ordClassKey 	then gen_Ord_binds tycon
	    else if clas_key == enumClassKey	then gen_Enum_binds tycon
	    else if clas_key == ixClassKey	then gen_Ix_binds tycon
	    else if clas_key == binaryClassKey  then gen_Binary_binds tycon
	    else panic "gen_inst_info:bad derived class"
    in
    rn4MtoTcM deriver_name_funs (
	rnMethodBinds4 clas_Name proto_mbinds
    )			`thenNF_Tc` \ (mbinds, errs) ->

    if not (isEmptyBag errs) then
	pprPanic "gen_inst_info:renamer errs!\n" (ppAbove (pprBagOfErrors PprDebug errs) (ppr PprDebug proto_mbinds))
    else
--  pprTrace "derived binds:" (ppr PprDebug proto_mbinds) $

	-- All done
    let 
	from_here = isLocallyDefined tycon	-- If so, then from here
    in
    returnTc (InstInfo clas tyvar_tmpls ty 
		       inst_decl_theta dfun_theta dfun_id const_meth_ids
		       -- and here comes the main point...
		       (if from_here then mbinds else EmptyMonoBinds)
		       from_here modname locn [])
  where
    clas_key = getClassKey clas
    clas_Name
      = let  (mod, nm) = getOrigName clas  in
    	PreludeClass clas_key (mkPreludeCoreName mod nm)
\end{code}

%************************************************************************
%*									*
\subsection[TcGenDeriv-con2tag-tag2con]{Generating extra binds (@con2tag@ and @tag2con@)}
%*									*
%************************************************************************

data Foo ... = ...

con2tag_Foo :: Foo ... -> Int#
tag2con_Foo :: Int -> Foo ...	-- easier if Int, not Int#
maxtag_Foo  :: Int		-- ditto (NB: not unboxed)

\begin{code}
gen_tag_n_con_binds :: GlobalNameFuns
		    -> [(ProtoName, Name, TyCon, TagThingWanted)]
		    -> TcM RenamedBinds

gen_tag_n_con_binds deriver_name_funs nm_alist_etc
  = let
      proto_mbind_list = map gen_tag_n_con_monobind nm_alist_etc
      proto_mbinds     = foldr AndMonoBinds EmptyMonoBinds proto_mbind_list
    in

    rn4MtoTcM deriver_name_funs (
	rnTopBinds4 (SingleBind (RecBind proto_mbinds))
    )			`thenNF_Tc` \ (binds, errs) ->

    if not (isEmptyBag errs) then
	panic "gen_inst_info:renamer errs (2)!"
    else
	returnTc binds
\end{code}

%************************************************************************
%*									*
\subsection[TcDeriv-taggery-Names]{What con2tag/tag2con functions are available?}
%*									*
%************************************************************************

We have a @con2tag@ function for a tycon if:
\begin{itemize}
\item
We're deriving @Eq@ and the tycon has nullary data constructors.

\item
Or: we're deriving @Ord@ (unless single-constructor), @Enum@, @Ix@
(enum type only????)
\end{itemize}

We have a @tag2con@ function for a tycon if:
\begin{itemize}
\item
We're deriving @Enum@, or @Ix@ (enum type only???)
\end{itemize}

If we have a @tag2con@ function, we also generate a @maxtag@ constant.

\begin{code}
data TagThingWanted
  = GenCon2Tag | GenTag2Con | GenMaxTag

gen_taggery_Names :: [DerivEqn]
		  -> TcM [(ProtoName, Name,	-- for an assoc list
		  	   TyCon,		-- related tycon
			   TagThingWanted)]

gen_taggery_Names eqns
  = let all_tycons = [ tc | (_, tc, _, _) <- eqns ]
	(tycons_of_interest, _) = removeDups cmpTyCon all_tycons
    in
	foldlTc do_con2tag []           tycons_of_interest `thenTc` \ names_so_far ->
	foldlTc do_tag2con names_so_far tycons_of_interest
  where
    do_con2tag acc_Names tycon
      = if (we_are_deriving eqClassKey tycon
	    && any isNullaryDataCon (getTyConDataCons tycon))
	|| (we_are_deriving ordClassKey  tycon
	    && not (maybeToBool (maybeSingleConstructorTyCon tycon)))
	|| (we_are_deriving enumClassKey tycon)
	|| (we_are_deriving ixClassKey   tycon)
	then
	  getUniqueTc	`thenNF_Tc` ( \ u ->
	  returnTc ((con2tag_PN tycon, OtherTopId u (con2tag_FN tycon), tycon, GenCon2Tag)
		   : acc_Names) )
	else
	  returnTc acc_Names

    do_tag2con acc_Names tycon
      = if (we_are_deriving enumClassKey tycon)
	|| (we_are_deriving ixClassKey   tycon)
	then
	  getUniqueTc	`thenNF_Tc` \ u1 ->
	  getUniqueTc	`thenNF_Tc` \ u2 ->
	  returnTc ( (tag2con_PN tycon, OtherTopId u1 (tag2con_FN tycon), tycon, GenTag2Con)
		   : (maxtag_PN  tycon, OtherTopId u2 (maxtag_FN  tycon), tycon, GenMaxTag)
		   : acc_Names)
	else
	  returnTc acc_Names

    we_are_deriving clas_key tycon
      = is_in_eqns clas_key tycon eqns
      where
	is_in_eqns clas_key tycon [] = False
	is_in_eqns clas_key tycon ((c,t,_,_):eqns) -- ToDo: InstInfo
	  =  (clas_key == getClassKey c && tycon == t)
	  || is_in_eqns clas_key tycon eqns

con2tag_PN, tag2con_PN, maxtag_PN :: TyCon -> ProtoName
con2tag_FN, tag2con_FN, maxtag_FN :: TyCon -> FullName

con2tag_PN tycon
  = let	(mod, nm) = getOrigName tycon
	con2tag	  = SLIT("con2tag_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    Imp mod con2tag [mod] con2tag

con2tag_FN tycon
  = let	(mod, nm) = getOrigName tycon
	con2tag	  = SLIT("con2tag_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    mkFullName mod con2tag InventedInThisModule NotExported mkGeneratedSrcLoc

tag2con_PN tycon
  = let	(mod, nm) = getOrigName tycon
	tag2con	  = SLIT("tag2con_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    Imp mod tag2con [mod] tag2con

tag2con_FN tycon
  = let	(mod, nm) = getOrigName tycon
	tag2con	  = SLIT("tag2con_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    mkFullName mod tag2con InventedInThisModule NotExported mkGeneratedSrcLoc

maxtag_PN tycon
  = let	(mod, nm) = getOrigName tycon
	maxtag	  = SLIT("maxtag_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    Imp mod maxtag [mod] maxtag

maxtag_FN tycon
  = let	(mod, nm) = getOrigName tycon
	maxtag	  = SLIT("maxtag_") _APPEND_ nm _APPEND_ SLIT("#")
    in
    mkFullName mod maxtag InventedInThisModule NotExported mkGeneratedSrcLoc
\end{code}
