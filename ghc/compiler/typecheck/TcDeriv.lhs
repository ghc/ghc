%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcDeriv]{Deriving}

Handles @deriving@ clauses on @data@ declarations.

\begin{code}
#include "HsVersions.h"

module TcDeriv ( tcDeriving ) where

IMP_Ubiq()

import HsSyn		( FixityDecl, Sig, HsBinds(..), Bind(..), MonoBinds(..),
			  GRHSsAndBinds, Match, HsExpr, HsLit, InPat,
			  ArithSeqInfo, Fake, MonoType )
import HsPragmas	( InstancePragmas(..) )
import RnHsSyn		( mkRnName, RnName(..), RenamedHsBinds(..), RenamedFixityDecl(..) )
import TcHsSyn		( TcIdOcc )

import TcMonad
import Inst		( InstanceMapper(..) )
import TcEnv		( getEnv_TyCons, tcLookupClassByKey )
import TcKind		( TcKind )
import TcGenDeriv	-- Deriv stuff
import TcInstUtil	( InstInfo(..), mkInstanceRelatedIds, buildInstanceEnvs )
import TcSimplify	( tcSimplifyThetas )

import RnMonad
import RnUtils		( SYN_IE(RnEnv), extendGlobalRnEnv )
import RnBinds		( rnMethodBinds, rnTopBinds )

import Bag		( emptyBag{-ToDo:rm-}, Bag, isEmptyBag, unionBags, listToBag )
import Class		( classKey, needsDataDeclCtxtClassKeys, GenClass )
import ErrUtils		( pprBagOfErrors, addErrLoc, SYN_IE(Error) )
import Id		( dataConArgTys, isNullaryDataCon, mkDictFunId )
import Maybes		( maybeToBool )
import Name		( isLocallyDefined, getSrcLoc,
			  mkTopLevName, origName, mkImplicitName, ExportFlag(..),
			  RdrName(..), Name{--O only-}
			)
import Outputable	( Outputable(..){-instances e.g., (,)-} )
import PprType		( GenType, GenTyVar, GenClass, TyCon )
import PprStyle		( PprStyle(..) )
import Pretty		( ppAbove, ppAboves, ppCat, ppBesides, ppStr, ppHang, SYN_IE(Pretty) )
import Pretty--ToDo:rm
import FiniteMap--ToDo:rm
import SrcLoc		( mkGeneratedSrcLoc, SrcLoc )
import TyCon		( tyConTyVars, tyConDataCons, tyConDerivings,
			  tyConTheta, maybeTyConSingleCon,
			  isEnumerationTyCon, isDataTyCon, TyCon
			)
import Type		( GenType(..), SYN_IE(TauType), mkTyVarTys, applyTyCon,
			  mkSigmaTy, mkDictTy, isPrimType, instantiateTy,
			  getAppDataTyCon, getAppTyCon
			)
import TysPrim		( voidTy )
import TyVar		( GenTyVar )
import UniqFM		( emptyUFM )
import Unique		-- Keys stuff
import Util		( zipWithEqual, zipEqual, sortLt, removeDups,  assoc,
			  thenCmp, cmpList, panic, pprPanic, pprPanic#,
			  assertPanic, pprTrace{-ToDo:rm-}
			)
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

[NOTE: See end of these comments for what to do with 
	data (C a, D b) => T a b = ...
]

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


A note about contexts on data decls
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

	data (RealFloat a) => Complex a = !a :+ !a deriving( Read )

We will need an instance decl like:

	instance (Read a, RealFloat a) => Read (Complex a) where
	  ...

The RealFloat in the context is because the read method for Complex is bound
to construct a Complex, and doing that requires that the argument type is
in RealFloat. 

But this ain't true for Show, Eq, Ord, etc, since they don't construct
a Complex; they only take them apart.

Our approach: identify the offending classes, and add the data type
context to the instance decl.  The "offending classes" are

	Read, Enum?


%************************************************************************
%*									*
\subsection[TcDeriv-driver]{Top-level function for \tr{derivings}}
%*									*
%************************************************************************

\begin{code}
tcDeriving  :: Module			-- name of module under scrutiny
	    -> RnEnv			-- for "renaming" bits of generated code
	    -> Bag InstInfo		-- What we already know about instances
	    -> [RenamedFixityDecl]	-- Fixity info; used by Read and Show
	    -> TcM s (Bag InstInfo,	-- The generated "instance decls".
		      RenamedHsBinds,	-- Extra generated bindings
		      PprStyle -> Pretty)  -- Printable derived instance decls;
				     	   -- for debugging via -ddump-derivings.

tcDeriving modname rn_env inst_decl_infos_in fixities
  =	-- Fish the "deriving"-related information out of the TcEnv
	-- and make the necessary "equations".
    makeDerivEqns	    	`thenTc` \ eqns ->

	-- Take the equation list and solve it, to deliver a list of
	-- solutions, a.k.a. the contexts for the instance decls
	-- required for the corresponding equations.
    solveDerivEqns inst_decl_infos_in eqns
			    	`thenTc` \ new_inst_infos ->

	-- Now augment the InstInfos, adding in the rather boring
	-- actual-code-to-do-the-methods binds.  We may also need to
	-- generate extra not-one-inst-decl-specific binds, notably
	-- "con2tag" and/or "tag2con" functions.  We do these
	-- separately.

    gen_taggery_Names new_inst_infos	`thenTc` \ nm_alist_etc ->
    gen_tag_n_con_binds rn_env nm_alist_etc
				`thenTc` \ (extra_binds, deriver_rn_env) ->

    mapTc (gen_inst_info modname fixities deriver_rn_env) new_inst_infos
				`thenTc` \ really_new_inst_infos ->
    let
	ddump_deriv = ddump_deriving really_new_inst_infos extra_binds
    in
    --pprTrace "derived:\n" (ddump_deriv PprDebug) $

    returnTc (listToBag really_new_inst_infos,
	      extra_binds,
	      ddump_deriv)
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
  = tcGetEnv			    `thenNF_Tc` \ env ->
    tcLookupClassByKey evalClassKey `thenNF_Tc` \ eval_clas ->
    let
	tycons = filter isDataTyCon (getEnv_TyCons env)
	-- ToDo: what about newtypes???
	think_about_deriving = need_deriving eval_clas tycons
    in
    mapTc chk_out think_about_deriving `thenTc_`
    let
	(derive_these, _) = removeDups cmp_deriv think_about_deriving
	eqns = map mk_eqn derive_these
    in
    returnTc eqns
  where
    ------------------------------------------------------------------
    need_deriving :: Class -> [TyCon] -> [(Class, TyCon)]
	-- find the tycons that have `deriving' clauses;
	-- we handle the "every datatype in Eval" by
	-- doing a dummy "deriving" for it.

    need_deriving eval_clas tycons_to_consider
      = foldr ( \ tycon acc ->
		   let
			acc_plus = if isLocallyDefined tycon
				   then (eval_clas, tycon) : acc
				   else acc
		   in
		   case (tyConDerivings tycon) of
		     [] -> acc_plus
		     cs -> [ (clas,tycon) | clas <- cs ] ++ acc_plus
	      )
	      []
	      tycons_to_consider

    ------------------------------------------------------------------
    chk_out :: (Class, TyCon) -> TcM s ()
    chk_out this_one@(clas, tycon)
      =	let
	    clas_key = classKey clas

	    is_enumeration = isEnumerationTyCon tycon
	    is_single_con  = maybeToBool (maybeTyConSingleCon tycon)

	    chk_clas clas_uniq clas_str cond
	      = if (clas_uniq == clas_key)
		then checkTc cond (derivingThingErr clas_str tycon)
		else returnTc ()
    	in
	    -- Are things OK for deriving Enum (if appropriate)?
	chk_clas enumClassKey "Enum" is_enumeration `thenTc_`

	    -- Are things OK for deriving Bounded (if appropriate)?
	chk_clas boundedClassKey "Bounded"
		(is_enumeration || is_single_con) `thenTc_`

	    -- Are things OK for deriving Ix (if appropriate)?
	chk_clas ixClassKey "Ix.Ix" (is_enumeration || is_single_con)

    ------------------------------------------------------------------
    cmp_deriv :: (Class, TyCon) -> (Class, TyCon) -> TAG_
    cmp_deriv (c1, t1) (c2, t2)
      = (c1 `cmp` c2) `thenCmp` (t1 `cmp` t2)

    ------------------------------------------------------------------
    mk_eqn :: (Class, TyCon) -> DerivEqn
	-- we swizzle the tyvars and datacons out of the tycon
	-- to make the rest of the equation

    mk_eqn (clas, tycon)
      = (clas, tycon, tyvars, if_not_Eval constraints)
      where
	clas_key  = classKey clas
	tyvars    = tyConTyVars tycon	-- ToDo: Do we need new tyvars ???
	tyvar_tys = mkTyVarTys tyvars
	data_cons = tyConDataCons tycon

	if_not_Eval cs = if clas_key == evalClassKey then [] else cs

	constraints = extra_constraints ++ concat (map mk_constraints data_cons)

	-- "extra_constraints": see notes above about contexts on data decls
	extra_constraints
	  | offensive_class = tyConTheta tycon
	  | otherwise	    = []
	   where
	    offensive_class = clas_key `elem` needsDataDeclCtxtClassKeys

	mk_constraints data_con
	   = [ (clas, arg_ty)
	     | arg_ty <- instd_arg_tys,
	       not (isPrimType arg_ty)	-- No constraints for primitive types
	     ]
	   where
	     instd_arg_tys  = dataConArgTys data_con tyvar_tys
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
Each (k,TyVarTy tv) in a solution constrains only a type
variable, tv.

\item
The (k,TyVarTy tv) pairs in a solution are canonically
ordered by sorting on type varible, tv, (major key) and then class, k,
(minor key)
\end{itemize}

\begin{code}
solveDerivEqns :: Bag InstInfo
	       -> [DerivEqn]
	       -> TcM s [InstInfo]	-- Solns in same order as eqns.
				  	-- This bunch is Absolutely minimal...

solveDerivEqns inst_decl_infos_in orig_eqns
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

	add_solns inst_decl_infos_in orig_eqns current_solns
				`thenTc` \ (new_inst_infos, inst_mapper) ->
	let
	   class_to_inst_env cls = fst (inst_mapper cls)
	in
	    -- Simplify each RHS

	listTc [ tcSimplifyThetas class_to_inst_env [{-Nothing "given"-}] deriv_rhs
	       | (_,_,_,deriv_rhs) <- orig_eqns ]  `thenTc` \ next_solns ->

	    -- Canonicalise the solutions, so they compare nicely
	let canonicalised_next_solns
	      = [ sortLt lt_rhs next_soln | next_soln <- next_solns ] in

	if (current_solns `eq_solns` canonicalised_next_solns) then
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
add_solns :: Bag InstInfo			-- The global, non-derived ones
	  -> [DerivEqn] -> [DerivSoln]
	  -> TcM s ([InstInfo], 		-- The new, derived ones
		    InstanceMapper)
    -- the eqns and solns move "in lockstep"; we have the eqns
    -- because we need the LHS info for addClassInstance.

add_solns inst_infos_in eqns solns
  = buildInstanceEnvs all_inst_infos `thenTc` \ inst_mapper ->
    returnTc (new_inst_infos, inst_mapper)
  where
    new_inst_infos = zipWithEqual "add_solns" mk_deriv_inst_info eqns solns

    all_inst_infos = inst_infos_in `unionBags` listToBag new_inst_infos

    mk_deriv_inst_info (clas, tycon, tyvars, _) theta
      = InstInfo clas tyvars (applyTyCon tycon (mkTyVarTys tyvars))
		 theta
		 (my_panic "dfun_theta")

		 dummy_dfun_id

		 (my_panic "const_meth_ids")
		 (my_panic "binds")   (my_panic "from_here")
		 (my_panic "modname") mkGeneratedSrcLoc
		 (my_panic "upragmas")
      where
	dummy_dfun_id
	  = mkDictFunId bottom bottom bottom dummy_dfun_ty
			bottom bottom bottom bottom
	  where
	    bottom = panic "dummy_dfun_id"

	dummy_dfun_ty = mkSigmaTy tyvars theta voidTy
		-- All we need from the dfun is its "theta" part, used during
		-- equation simplification (tcSimplifyThetas).  The final
		-- dfun_id will have the superclass dictionaries as arguments too,
		-- but that'll be added after the equations are solved.  For now,
		-- it's enough just to make a dummy dfun with the simple theta part.
		-- 
		-- The part after the theta is dummied here as voidTy; actually it's
		-- 	(C (T a b)), but it doesn't seem worth constructing it.
		-- We can't leave it as a panic because to get the theta part we
		-- have to run down the type!

	my_panic str = pprPanic ("add_soln:"++str) (ppCat [ppChar ':', ppr PprDebug clas, ppr PprDebug tycon])
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
Deriving @Show@---also pretty common--- should also be reasonable good code.

\item
Deriving for the other classes isn't that common or that big a deal.
\end{itemize}

PRAGMATICS:

\begin{itemize}
\item
Deriving @Ord@ is done mostly with the 1.3 @compare@ method.

\item
Deriving @Eq@ also uses @compare@, if we're deriving @Ord@, too.

\item
We {\em normally} generate code only for the non-defaulted methods;
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
We use the renamer!!!  Reason: we're supposed to be
producing @RenamedMonoBinds@ for the methods, but that means
producing correctly-uniquified code on the fly.  This is entirely
possible (the @TcM@ monad has a @UniqueSupply@), but it is painful.
So, instead, we produce @RdrNameMonoBinds@ then heave 'em through
the renamer.  What a great hack!
\end{itemize}

\begin{code}
gen_inst_info :: Module			-- Module name
	      -> [RenamedFixityDecl]	-- all known fixities;
					-- may be needed for Text
	      -> RnEnv			-- lookup stuff for names we may use
	      -> InstInfo		-- the main stuff to work on
	      -> TcM s InstInfo		-- the gen'd (filled-in) "instance decl"

gen_inst_info modname fixities deriver_rn_env
    (InstInfo clas tyvars ty inst_decl_theta _ _ _ _ _ _ locn _)
  =
	-- Generate the various instance-related Ids
    mkInstanceRelatedIds
		True {-from_here-} locn modname
		NoInstancePragmas
		clas tyvars ty
		inst_decl_theta
		[{-no user pragmas-}]
			`thenTc` \ (dfun_id, dfun_theta, const_meth_ids) ->

	-- Generate the bindings for the new instance declaration,
	-- rename it, and check for errors
    let
	(tycon,_,_)  = --pprTrace "gen_inst_info:ty" (ppCat[ppr PprDebug clas, ppr PprDebug ty]) $
		       getAppDataTyCon ty

	proto_mbinds
	  = assoc "gen_inst_info:bad derived class"
		[(eqClassKey,	   gen_Eq_binds)
		,(ordClassKey,	   gen_Ord_binds)
		,(enumClassKey,	   gen_Enum_binds)
		,(evalClassKey,	   gen_Eval_binds)
		,(boundedClassKey, gen_Bounded_binds)
		,(showClassKey,	   gen_Show_binds fixities)
		,(readClassKey,	   gen_Read_binds fixities)
		,(ixClassKey,	   gen_Ix_binds)
		]
		clas_key $ tycon
    in
{-
    let
	((qual, unqual, tc_qual, tc_unqual), stack) = deriver_rn_env
    in
    pprTrace "gen_inst:qual:"      (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (n,m) <- keysFM qual]) $
    pprTrace "gen_inst:unqual:"    (ppCat (map ppPStr (keysFM unqual))) $
    pprTrace "gen_inst:tc_qual:"   (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (n,m) <- keysFM tc_qual]) $
    pprTrace "gen_inst:tc_unqual:" (ppCat (map ppPStr (keysFM tc_unqual))) $
-}
    -- pprTrace "derived binds:" (ppr PprDebug proto_mbinds) $

    rnMtoTcM deriver_rn_env (
	setExtraRn emptyUFM{-no fixities-} $
	rnMethodBinds clas_Name proto_mbinds
    )			`thenNF_Tc` \ (mbinds, errs) ->

    if not (isEmptyBag errs) then
	pprPanic "gen_inst_info:renamer errs!\n"
		 (ppAbove (pprBagOfErrors PprDebug errs) (ppr PprDebug proto_mbinds))
    else
	-- All done
    let
	from_here = isLocallyDefined tycon	-- If so, then from here
    in
    returnTc (InstInfo clas tyvars ty inst_decl_theta
		       dfun_theta dfun_id const_meth_ids
		       (if from_here then mbinds else EmptyMonoBinds)
		       from_here modname locn [])
  where
    clas_key  = classKey clas
    clas_Name = RnImplicitClass (mkImplicitName clas_key (origName "gen_inst_info" clas))
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
gen_tag_n_con_binds :: RnEnv
		    -> [(RdrName, TyCon, TagThingWanted)]
		    -> TcM s (RenamedHsBinds,
			      RnEnv) -- input one with any new names added

gen_tag_n_con_binds rn_env nm_alist_etc
  = 
    let
	-- We have the renamer's final "name funs" in our hands
	-- (they were passed in).  So we can handle ProtoNames
	-- that refer to anything "out there".  But our generated
	-- code may also mention "con2tag" (etc.).  So we need
	-- to augment to "name funs" to include those.

	names_to_add = [ pn | (pn,_,_) <- nm_alist_etc ]
    in
    tcGetUniques (length names_to_add)	`thenNF_Tc` \ uniqs ->
    let
	pairs_to_add = [ case pn of { Qual pnm pnn ->
			 (pn, mkRnName (mkTopLevName u (OrigName pnm pnn) mkGeneratedSrcLoc ExportAll [])) }
		       | (pn,u) <- zipEqual "gen_tag..." names_to_add uniqs ]

	deriver_rn_env
	  = if null names_to_add
	    then rn_env else added_rn_env

	(added_rn_env, errs_bag)
	  = extendGlobalRnEnv rn_env pairs_to_add [{-no tycons-}]

	----------------
	proto_mbind_list = map gen_tag_n_con_monobind nm_alist_etc
	proto_mbinds     = foldr AndMonoBinds EmptyMonoBinds proto_mbind_list
    in
    ASSERT(isEmptyBag errs_bag)

    rnMtoTcM deriver_rn_env (
	setExtraRn emptyUFM{-no fixities-} $
	rnTopBinds (SingleBind (RecBind proto_mbinds))
    )			`thenNF_Tc` \ (binds, errs) ->

    if not (isEmptyBag errs) then
	pprPanic "gen_tag_n_con_binds:renamer errs!\n"
		 (ppAbove (pprBagOfErrors PprDebug errs) (ppr PprDebug binds))
    else
	returnTc (binds, deriver_rn_env)
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
gen_taggery_Names :: [InstInfo]
		  -> TcM s [(RdrName,	-- for an assoc list
		  	     TyCon,	-- related tycon
			     TagThingWanted)]

gen_taggery_Names inst_infos
  = --pprTrace "gen_taggery:\n" (ppAboves [ppCat [ppr PprDebug c, ppr PprDebug t] | (c,t) <- all_CTs]) $
    foldlTc do_con2tag []           tycons_of_interest `thenTc` \ names_so_far ->
    foldlTc do_tag2con names_so_far tycons_of_interest
  where
    all_CTs = [ mk_CT c ty | (InstInfo c _ ty _ _ _ _ _ _ _ _ _) <- inst_infos ]
		    
    mk_CT c ty = (c, fst (getAppTyCon ty))

    all_tycons = map snd all_CTs
    (tycons_of_interest, _) = removeDups cmp all_tycons
    
    do_con2tag acc_Names tycon
      = if (we_are_deriving eqClassKey tycon
	    && any isNullaryDataCon (tyConDataCons tycon))
	|| (we_are_deriving ordClassKey  tycon
	    && not (maybeToBool (maybeTyConSingleCon tycon)))
	|| (we_are_deriving enumClassKey tycon)
	|| (we_are_deriving ixClassKey   tycon)
	then
	  returnTc ((con2tag_PN tycon, tycon, GenCon2Tag)
		   : acc_Names)
	else
	  returnTc acc_Names

    do_tag2con acc_Names tycon
      = if (we_are_deriving enumClassKey tycon)
	|| (we_are_deriving ixClassKey   tycon)
	then
	  returnTc ( (tag2con_PN tycon, tycon, GenTag2Con)
		   : (maxtag_PN  tycon, tycon, GenMaxTag)
		   : acc_Names)
	else
	  returnTc acc_Names

    we_are_deriving clas_key tycon
      = is_in_eqns clas_key tycon all_CTs
      where
	is_in_eqns clas_key tycon [] = False
	is_in_eqns clas_key tycon ((c,t):cts)
	  =  (clas_key == classKey c && tycon == t)
	  || is_in_eqns clas_key tycon cts

\end{code}

\begin{code}
derivingThingErr :: String -> TyCon -> Error

derivingThingErr thing tycon sty
  = ppHang (ppCat [ppStr "Can't make a derived instance of", ppStr thing])
	 4 (ppBesides [ppStr "for the type `", ppr sty tycon, ppStr "'"])
\end{code}
