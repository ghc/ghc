%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[TcDeriv]{Deriving}

Handles @deriving@ clauses on @data@ declarations.

\begin{code}
#include "HsVersions.h"

module TcDeriv (
	tcDeriving
    ) where

import Ubiq

import HsSyn		( FixityDecl, Sig, HsBinds(..), Bind(..), MonoBinds(..),
			  GRHSsAndBinds, Match, HsExpr, HsLit, InPat,
			  ArithSeqInfo, Fake, MonoType )
import HsPragmas	( InstancePragmas(..) )
import RnHsSyn		( RenamedHsBinds(..), RenamedFixityDecl(..) )
import TcHsSyn		( TcIdOcc )

import TcMonad
import Inst		( InstOrigin(..), InstanceMapper(..) )
import TcEnv		( getEnv_TyCons )
import TcKind		( TcKind )
import TcGenDeriv	-- Deriv stuff
import TcInstUtil	( InstInfo(..), mkInstanceRelatedIds, buildInstanceEnvs )
import TcSimplify	( tcSimplifyThetas )

import RnMonad4
import RnUtils		( GlobalNameMappers(..), GlobalNameMapper(..) )
import RnBinds4		( rnMethodBinds, rnTopBinds )

import Bag		( Bag, isEmptyBag, unionBags, listToBag )
import Class		( GenClass, getClassKey )
import ErrUtils		( pprBagOfErrors, addErrLoc )
import Id		( dataConSig, dataConArity )
import Maybes		( assocMaybe, maybeToBool, Maybe(..) )
import Name		( Name(..) )
import NameTypes	( mkPreludeCoreName, Provenance(..) )
import Outputable
import PprType		( GenType, GenTyVar, GenClass, TyCon )
import PprStyle
import Pretty
import ProtoName	( eqProtoName, ProtoName(..), Name )
import SrcLoc		( mkGeneratedSrcLoc, mkUnknownSrcLoc, SrcLoc )
import TyCon		( tyConTyVars, tyConDataCons, tyConDerivings,
			  maybeTyConSingleCon, isEnumerationTyCon, TyCon )
import Type		( GenType(..), TauType(..), mkTyVarTys, applyTyCon,
			  mkSigmaTy, mkDictTy, isPrimType, instantiateTy,
			  getAppTyCon, getAppDataTyCon )
import TyVar		( GenTyVar )
import UniqFM		( eltsUFM )
import Unique		-- Keys stuff
import Util		( zipWithEqual, zipEqual, sortLt, removeDups, 
			  thenCmp, cmpList, panic, pprPanic, pprPanic# )
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
	    -> GlobalNameMappers	-- for "renaming" bits of generated code
	    -> Bag InstInfo		-- What we already know about instances
	    -> [RenamedFixityDecl]	-- Fixity info; used by Read and Show
	    -> TcM s (Bag InstInfo,	-- The generated "instance decls".
		      RenamedHsBinds,	-- Extra generated bindings
		      PprStyle -> Pretty)  -- Printable derived instance decls;
				     	   -- for debugging via -ddump-derivings.

tcDeriving modname renamer_name_funs inst_decl_infos_in fixities
  =	-- Fish the "deriving"-related information out of the TcEnv
	-- and make the necessary "equations".
    makeDerivEqns	    	`thenTc` \ eqns ->

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

    gen_taggery_Names eqns	`thenTc` \ nm_alist_etc ->
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
	assoc_maybe ((k,v) : vs) key
	  = if k `eqProtoName` key then Just v else assoc_maybe vs key
    in
    gen_tag_n_con_binds deriver_name_funs nm_alist_etc `thenTc` \ extra_binds ->

    mapTc (gen_inst_info modname fixities deriver_name_funs) new_inst_infos
						  `thenTc` \ really_new_inst_infos ->

    returnTc (listToBag really_new_inst_infos,
	      extra_binds,
	      ddump_deriving really_new_inst_infos extra_binds)
  where
    ddump_deriving :: [InstInfo] -> RenamedHsBinds -> (PprStyle -> Pretty)

    ddump_deriving inst_infos extra_binds sty
      = ppAboves ((map pp_info inst_infos) ++ [ppr sty extra_binds])
      where
	pp_info (InstInfo clas tvs ty inst_decl_theta _ _ _ mbinds _ _ _ _)
	  = ppAbove (ppr sty (mkSigmaTy tvs inst_decl_theta (mkDictTy clas ty)))
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
makeDerivEqns :: TcM s [DerivEqn]

makeDerivEqns
  = tcGetEnv `thenNF_Tc` \ env ->
    let
	tycons = getEnv_TyCons env
	think_about_deriving = need_deriving tycons
    in
    mapTc (chk_out think_about_deriving) think_about_deriving `thenTc_`
    let
	(derive_these, _) = removeDups cmp_deriv think_about_deriving
	eqns = map mk_eqn derive_these
    in
    returnTc eqns
  where
    ------------------------------------------------------------------
    need_deriving :: [TyCon] -> [(Class, TyCon)]
	-- find the tycons that have `deriving' clauses

    need_deriving tycons_to_consider
      = foldr ( \ tycon acc ->
		   case (tyConDerivings tycon) of
		     [] -> acc
		     cs -> [ (clas,tycon) | clas <- cs ] ++ acc
	      )
	      []
	      tycons_to_consider

    ------------------------------------------------------------------
    chk_out :: [(Class, TyCon)] -> (Class, TyCon) -> TcM s ()
    chk_out whole_deriving_list this_one@(clas, tycon)
      =	let
	    clas_key = getClassKey clas
    	in

	    -- Are things OK for deriving Enum (if appropriate)?
	checkTc (clas_key == enumClassKey && not (isEnumerationTyCon tycon))
		(derivingEnumErr tycon)			`thenTc_`

	    -- Are things OK for deriving Ix (if appropriate)?
	checkTc (clas_key == ixClassKey
	         && not (isEnumerationTyCon tycon
		         || maybeToBool (maybeTyConSingleCon tycon)))
		(derivingIxErr tycon)

    ------------------------------------------------------------------
    cmp_deriv :: (Class, TyCon) -> (Class, TyCon) -> TAG_
    cmp_deriv (c1, t1) (c2, t2)
      = (c1 `cmp` c2) `thenCmp` (t1 `cmp` t2)

    ------------------------------------------------------------------
    mk_eqn :: (Class, TyCon) -> DerivEqn
	-- we swizzle the tyvars and datacons out of the tycon
	-- to make the rest of the equation

    mk_eqn (clas, tycon)
      = (clas, tycon, tyvars, constraints)
      where
	tyvars    = tyConTyVars tycon	-- ToDo: Do we need new tyvars ???
	tyvar_tys = mkTyVarTys tyvars
	data_cons = tyConDataCons tycon
	constraints = concat (map mk_constraints data_cons)

	mk_constraints data_con
	   = [ (clas, instantiateTy inst_env arg_ty)
	     | arg_ty <- arg_tys,
	       not (isPrimType arg_ty)	-- No constraints for primitive types
	     ]
	   where
	     (con_tyvars, _, arg_tys, _) = dataConSig data_con
	     inst_env = con_tyvars `zipEqual` tyvar_tys
	                -- same number of tyvars in data constr and type constr!
\end{code}

%************************************************************************
%*									*
\subsection[TcDeriv-fixpoint]{Finding the fixed point of \tr{deriving} equations}
%*									*
%************************************************************************

A ``solution'' (to one of the equations) is a list of (k,TyVarTy tv)
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
	       -> TcM s [InstInfo]	-- Solns in same order as eqns.
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

    iterateDeriv :: [DerivSoln] ->TcM s [InstInfo]

    iterateDeriv current_solns
      =	    -- Extend the inst info from the explicit instance decls
	    -- with the current set of solutions, giving a

	add_solns modname inst_decl_infos_in orig_eqns current_solns
				`thenTc` \ (new_inst_infos, inst_mapper) ->

	    -- Simplify each RHS, using a DerivingOrigin containing an
	    -- inst_mapper reflecting the previous solution
	let
	    mk_deriv_origin clas ty
	      = DerivingOrigin inst_mapper clas tycon
	      where
		(tycon,_) = getAppTyCon ty
	in
	listTc [ tcSimplifyThetas mk_deriv_origin rhs
	       | (_, _, _, rhs) <- orig_eqns
	       ]		`thenTc` \ next_solns ->

	    -- Canonicalise the solutions, so they compare nicely
	let canonicalised_next_solns
	      = [ sortLt lt_rhs next_soln | next_soln <- next_solns ] in

	if current_solns `eq_solns` canonicalised_next_solns then
	    returnTc new_inst_infos
	else
	    iterateDeriv canonicalised_next_solns

      where
	------------------------------------------------------------------
	lt_rhs    r1 r2 = case cmp_rhs   r1 r2 of { LT_ -> True; _ -> False }
        eq_solns  s1 s2 = case cmp_solns s1 s2 of { EQ_ -> True; _ -> False }
	cmp_solns s1 s2 = cmpList (cmpList cmp_rhs) s1 s2
	cmp_rhs (c1, TyVarTy tv1) (c2, TyVarTy tv2)
	  = (tv1 `cmp` tv2) `thenCmp` (c1 `cmp` c2)
#ifdef DEBUG
	cmp_rhs other_1 other_2
	  = pprPanic# "tcDeriv:cmp_rhs:" (ppCat [ppr PprDebug other_1, ppr PprDebug other_2])
#endif

\end{code}

\begin{code}
add_solns :: FAST_STRING
	  -> Bag InstInfo			-- The global, non-derived ones
	  -> [DerivEqn] -> [DerivSoln]
	  -> TcM s ([InstInfo], 		-- The new, derived ones
		    InstanceMapper)
    -- the eqns and solns move "in lockstep"; we have the eqns
    -- because we need the LHS info for addClassInstance.

add_solns modname inst_infos_in eqns solns
  = buildInstanceEnvs all_inst_infos `thenTc` \ inst_mapper ->
    returnTc (new_inst_infos, inst_mapper)
  where
    new_inst_infos = zipWithEqual mk_deriv_inst_info eqns solns

    all_inst_infos = inst_infos_in `unionBags` listToBag new_inst_infos

    mk_deriv_inst_info (clas, tycon, tyvars, _) theta
      = InstInfo clas tyvars (applyTyCon tycon (mkTyVarTys tyvars))
		 theta
		 theta			-- Blarg.  This is the dfun_theta slot,
					-- which is needed by buildInstanceEnv;
					-- This works ok for solving the eqns, and
					-- gen_eqns sets it to its final value
					-- (incl super class dicts) before we
					-- finally return it.
#ifdef DEBUG
		 (panic "add_soln:dfun_id") (panic "add_soln:const_meth_ids")
		 (panic "add_soln:binds")   (panic "add_soln:from_here")
		 (panic "add_soln:modname") mkGeneratedSrcLoc
		 (panic "add_soln:upragmas")
#else
		bottom bottom bottom bottom bottom mkGeneratedSrcLoc bottom
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
	      -> GlobalNameMappers		-- lookup stuff for names we may use
	      -> InstInfo		-- the main stuff to work on
	      -> TcM s InstInfo		-- the gen'd (filled-in) "instance decl"

gen_inst_info modname fixities deriver_name_funs
    info@(InstInfo clas tyvars ty inst_decl_theta _ _ _ _ _ _ locn _)
  =
	-- Generate the various instance-related Ids
    mkInstanceRelatedIds
		True {-from_here-} modname
		NoInstancePragmas
		clas tyvars ty
		inst_decl_theta
		[{-no user pragmas-}]
			`thenTc` \ (dfun_id, dfun_theta, const_meth_ids) ->

	-- Generate the bindings for the new instance declaration,
	-- rename it, and check for errors
    let
	(tycon,_,_)  = getAppDataTyCon ty

	proto_mbinds
	  | clas_key == eqClassKey     = gen_Eq_binds tycon
	  | clas_key == showClassKey   = gen_Show_binds fixities tycon
	  | clas_key == ordClassKey    = gen_Ord_binds tycon
	  | clas_key == enumClassKey   = gen_Enum_binds tycon
	  | clas_key == ixClassKey     = gen_Ix_binds tycon
	  | clas_key == readClassKey   = gen_Read_binds fixities tycon
	  | clas_key == binaryClassKey = gen_Binary_binds tycon
	  | otherwise = panic "gen_inst_info:bad derived class"
    in
    rn4MtoTcM deriver_name_funs (
	rnMethodBinds clas_Name proto_mbinds
    )			`thenNF_Tc` \ (mbinds, errs) ->

    if not (isEmptyBag errs) then
	pprPanic "gen_inst_info:renamer errs!\n"
		 (ppAbove (pprBagOfErrors PprDebug errs) (ppr PprDebug proto_mbinds))
    else
    --pprTrace "derived binds:" (ppr PprDebug proto_mbinds) $

	-- All done
    let
	from_here = isLocallyDefined tycon	-- If so, then from here
    in
    returnTc (InstInfo clas tyvars ty inst_decl_theta
		       dfun_theta dfun_id const_meth_ids
		       (if from_here then mbinds else EmptyMonoBinds)
		       from_here modname locn [])
  where
    clas_key = getClassKey clas
    clas_Name
      = let  (mod, nm) = getOrigName clas  in
    	ClassName clas_key (mkPreludeCoreName mod nm) []
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
gen_tag_n_con_binds :: GlobalNameMappers
		    -> [(ProtoName, Name, TyCon, TagThingWanted)]
		    -> TcM s RenamedHsBinds

gen_tag_n_con_binds deriver_name_funs nm_alist_etc
  = let
      proto_mbind_list = map gen_tag_n_con_monobind nm_alist_etc
      proto_mbinds     = foldr AndMonoBinds EmptyMonoBinds proto_mbind_list
    in

    rn4MtoTcM deriver_name_funs (
	rnTopBinds (SingleBind (RecBind proto_mbinds))
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
gen_taggery_Names :: [DerivEqn]
		  -> TcM s [(ProtoName, Name,	-- for an assoc list
		  	     TyCon,		-- related tycon
			     TagThingWanted)]

gen_taggery_Names eqns
  = let
	all_tycons = [ tc | (_, tc, _, _) <- eqns ]
	(tycons_of_interest, _) = removeDups cmp all_tycons
    in
	foldlTc do_con2tag []           tycons_of_interest `thenTc` \ names_so_far ->
	foldlTc do_tag2con names_so_far tycons_of_interest
  where
    do_con2tag acc_Names tycon
      = if (we_are_deriving eqClassKey tycon
	    && any ( (== 0).dataConArity ) (tyConDataCons tycon))
	|| (we_are_deriving ordClassKey  tycon
	    && not (maybeToBool (maybeTyConSingleCon tycon)))
	|| (we_are_deriving enumClassKey tycon)
	|| (we_are_deriving ixClassKey   tycon)
	then
	  tcGetUnique	`thenNF_Tc` ( \ u ->
	  returnTc ((con2tag_PN tycon, ValName u (con2tag_FN tycon), tycon, GenCon2Tag)
		   : acc_Names) )
	else
	  returnTc acc_Names

    do_tag2con acc_Names tycon
      = if (we_are_deriving enumClassKey tycon)
	|| (we_are_deriving ixClassKey   tycon)
	then
	  tcGetUnique	`thenNF_Tc` \ u1 ->
	  tcGetUnique	`thenNF_Tc` \ u2 ->
	  returnTc ( (tag2con_PN tycon, ValName u1 (tag2con_FN tycon), tycon, GenTag2Con)
		   : (maxtag_PN  tycon, ValName u2 (maxtag_FN  tycon), tycon, GenMaxTag)
		   : acc_Names)
	else
	  returnTc acc_Names

    we_are_deriving clas_key tycon
      = is_in_eqns clas_key tycon eqns
      where
	is_in_eqns clas_key tycon [] = False
	is_in_eqns clas_key tycon ((c,t,_,_):eqns)
	  =  (clas_key == getClassKey c && tycon == t)
	  || is_in_eqns clas_key tycon eqns

\end{code}

\begin{code}
derivingEnumErr :: TyCon -> TcError
derivingEnumErr tycon
  = addErrLoc (getSrcLoc tycon) "Can't derive an instance of `Enum'" ( \ sty ->
    ppBesides [ppStr "type `", ppr sty tycon, ppStr "'"] )

derivingIxErr :: TyCon -> TcError
derivingIxErr tycon
  = addErrLoc (getSrcLoc tycon) "Can't derive an instance of `Ix'" ( \ sty ->
    ppBesides [ppStr "type `", ppr sty tycon, ppStr "'"] )
\end{code}
