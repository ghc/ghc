%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcDeriv]{Deriving}

Handles @deriving@ clauses on @data@ declarations.

\begin{code}
module TcDeriv ( tcDeriving ) where

#include "HsVersions.h"

import HsSyn		( HsBinds(..), MonoBinds(..), TyClDecl(..),
			  collectLocatedMonoBinders )
import RdrHsSyn		( RdrNameMonoBinds )
import RnHsSyn		( RenamedHsBinds, RenamedMonoBinds, RenamedTyClDecl, RenamedHsPred )
import CmdLineOpts	( DynFlag(..), DynFlags )

import TcMonad
import TcEnv		( tcSetInstEnv, newDFunName, InstInfo(..), pprInstInfo,
			  tcLookupClass, tcLookupTyCon, tcExtendTyVarEnv
			)
import TcGenDeriv	-- Deriv stuff
import InstEnv		( InstEnv, simpleDFunClassTyCon, extendInstEnv )
import TcMonoType	( tcHsPred )
import TcSimplify	( tcSimplifyThetas )

import RnBinds		( rnMethodBinds, rnTopMonoBinds )
import RnEnv		( bindLocatedLocalsRn )
import RnMonad		( renameDerivedCode, thenRn, mapRn, returnRn )
import HscTypes		( DFunId, PersistentRenamerState )

import BasicTypes	( Fixity, NewOrData(..) )
import Class		( className, classKey, classTyVars, Class )
import ErrUtils		( dumpIfSet_dyn, Message )
import MkId		( mkDictFunId )
import DataCon		( dataConRepArgTys, isNullaryDataCon, isExistentialDataCon )
import PrelInfo		( needsDataDeclCtxtClassKeys )
import Maybes		( maybeToBool, catMaybes )
import Module		( Module )
import Name		( Name, getSrcLoc, nameUnique )
import RdrName		( RdrName )

import TyCon		( tyConTyVars, tyConDataCons, tyConArity, newTyConRep,
			  tyConTheta, maybeTyConSingleCon, isDataTyCon,
			  isEnumerationTyCon, TyCon
			)
import TcType		( TcType, ThetaType, mkTyVarTys, mkTyConApp, getClassPredTys_maybe,
			  isUnLiftedType, mkClassPred, tyVarsOfTypes, tcSplitFunTys, 
			  tcSplitTyConApp_maybe, tcEqTypes )
import Var		( TyVar, tyVarKind )
import VarSet		( mkVarSet, subVarSet )
import PrelNames
import Util		( zipWithEqual, sortLt )
import ListSetOps	( removeDups,  assoc )
import Outputable
import Maybe		( isJust )
import List		( nub )
import FastString 	( FastString )
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
type DerivEqn = (Name, Class, TyCon, [TyVar], DerivRhs)
		-- The Name is the name for the DFun we'll build
		-- The tyvars bind all the variables in the RHS

type DerivRhs  = ThetaType
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
tcDeriving  :: PersistentRenamerState
	    -> Module			-- name of module under scrutiny
	    -> InstEnv			-- What we already know about instances
	    -> (Name -> Maybe Fixity)	-- used in deriving Show and Read
	    -> [RenamedTyClDecl]	-- All type constructors
	    -> TcM ([InstInfo],		-- The generated "instance decls".
		    RenamedHsBinds)	-- Extra generated bindings

tcDeriving prs mod inst_env get_fixity tycl_decls
  = recoverTc (returnTc ([], EmptyBinds)) $
    getDOptsTc				  `thenNF_Tc` \ dflags ->

  	-- Fish the "deriving"-related information out of the TcEnv
	-- and make the necessary "equations".
    makeDerivEqns tycl_decls		    		`thenTc` \ (ordinary_eqns, newtype_inst_info) ->
    let
	-- Add the newtype-derived instances to the inst env
	-- before tacking the "ordinary" ones
	inst_env1 = extend_inst_env dflags inst_env 
				    (map iDFunId newtype_inst_info)
    in    
    deriveOrdinaryStuff mod prs inst_env1 get_fixity 
			ordinary_eqns			`thenTc` \ (ordinary_inst_info, binds) ->
    let
	inst_info  = newtype_inst_info ++ ordinary_inst_info
    in

    ioToTc (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances" 
	 	          (ddump_deriving inst_info binds))	`thenTc_`

    returnTc (inst_info, binds)

  where
    ddump_deriving :: [InstInfo] -> RenamedHsBinds -> SDoc
    ddump_deriving inst_infos extra_binds
      = vcat (map pprInstInfo inst_infos) $$ ppr extra_binds


-----------------------------------------
deriveOrdinaryStuff mod prs inst_env_in get_fixity []	-- Short cut
  = returnTc ([], EmptyBinds)

deriveOrdinaryStuff mod prs inst_env_in get_fixity eqns
  =	-- Take the equation list and solve it, to deliver a list of
	-- solutions, a.k.a. the contexts for the instance decls
	-- required for the corresponding equations.
    solveDerivEqns inst_env_in eqns	    	`thenTc` \ new_dfuns ->

	-- Now augment the InstInfos, adding in the rather boring
	-- actual-code-to-do-the-methods binds.  We may also need to
	-- generate extra not-one-inst-decl-specific binds, notably
	-- "con2tag" and/or "tag2con" functions.  We do these
	-- separately.
    gen_taggery_Names new_dfuns			`thenTc` \ nm_alist_etc ->

    tcGetEnv					`thenNF_Tc` \ env ->
    getDOptsTc					`thenNF_Tc` \ dflags ->
    let
	extra_mbind_list = map gen_tag_n_con_monobind nm_alist_etc
	extra_mbinds     = foldr AndMonoBinds EmptyMonoBinds extra_mbind_list
	method_binds_s   = map (gen_bind get_fixity) new_dfuns
	mbinders	 = collectLocatedMonoBinders extra_mbinds
	
	-- Rename to get RenamedBinds.
	-- The only tricky bit is that the extra_binds must scope over the
	-- method bindings for the instances.
	(rn_method_binds_s, rn_extra_binds)
		= renameDerivedCode dflags mod prs (
			bindLocatedLocalsRn (ptext (SLIT("deriving"))) mbinders	$ \ _ ->
			rnTopMonoBinds extra_mbinds []		`thenRn` \ (rn_extra_binds, _) ->
			mapRn rn_meths method_binds_s		`thenRn` \ rn_method_binds_s ->
			returnRn (rn_method_binds_s, rn_extra_binds)
		  )
	new_inst_infos = zipWith gen_inst_info new_dfuns rn_method_binds_s
    in
    returnTc (new_inst_infos, rn_extra_binds)

  where
	-- Make a Real dfun instead of the dummy one we have so far
    gen_inst_info :: DFunId -> RenamedMonoBinds -> InstInfo
    gen_inst_info dfun binds
      = InstInfo { iDFunId = dfun, iBinds = binds, iPrags = [] }

    rn_meths (cls, meths) = rnMethodBinds cls [] meths `thenRn` \ (meths', _) -> 
			    returnRn meths'	-- Ignore the free vars returned
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
makeDerivEqns :: [RenamedTyClDecl] 
	      -> TcM ([DerivEqn],	-- Ordinary derivings
		      [InstInfo])	-- Special newtype derivings

makeDerivEqns tycl_decls
  = mapAndUnzipTc mk_eqn derive_these	 	`thenTc` \ (maybe_ordinaries, maybe_newtypes) ->
    returnTc (catMaybes maybe_ordinaries, catMaybes maybe_newtypes)
  where
    ------------------------------------------------------------------
    derive_these :: [(NewOrData, Name, RenamedHsPred)]
	-- Find the (nd, TyCon, Pred) pairs that must be `derived'
	-- NB: only source-language decls have deriving, no imported ones do
    derive_these = [ (nd, tycon, pred) 
		   | TyData {tcdND = nd, tcdName = tycon, tcdDerivs = Just preds} <- tycl_decls,
		     pred <- preds ]

    ------------------------------------------------------------------
    mk_eqn :: (NewOrData, Name, RenamedHsPred) -> NF_TcM (Maybe DerivEqn, Maybe InstInfo)
	-- We swizzle the tyvars and datacons out of the tycon
	-- to make the rest of the equation

    mk_eqn (new_or_data, tycon_name, pred)
      = tcLookupTyCon tycon_name		`thenNF_Tc` \ tycon ->
	tcAddSrcLoc (getSrcLoc tycon)		$
        tcAddErrCtxt (derivCtxt tycon)		$
	tcExtendTyVarEnv (tyConTyVars tycon)	$	-- Deriving preds may (now) mention
							-- the type variables for the type constructor
        tcHsPred pred				`thenTc` \ pred' ->
	case getClassPredTys_maybe pred' of
	   Nothing 	    -> bale_out (malformedPredErr tycon pred)
	   Just (clas, tys) -> mk_eqn_help new_or_data tycon clas tys

    ------------------------------------------------------------------
    mk_eqn_help DataType tycon clas tys
      | Just err <- chk_out clas tycon tys
      = bale_out (derivingThingErr clas tys tycon tyvars err)
      | otherwise 
      = new_dfun_name clas tycon	 `thenNF_Tc` \ dfun_name ->
	returnNF_Tc (Just (dfun_name, clas, tycon, tyvars, constraints), Nothing)
      where
	tyvars    = tyConTyVars tycon
	data_cons = tyConDataCons tycon
	constraints = extra_constraints ++ 
	  	      [ mkClassPred clas [arg_ty] 
		      | data_con <- tyConDataCons tycon,
		        arg_ty   <- dataConRepArgTys data_con,	
				-- Use the same type variables
				-- as the type constructor,
				-- hence no need to instantiate
			not (isUnLiftedType arg_ty)	-- No constraints for unlifted types?
		      ]

	
	 -- "extra_constraints": see notes above about contexts on data decls
	extra_constraints | offensive_class = tyConTheta tycon
			  | otherwise	    = []
	
	offensive_class = classKey clas `elem` needsDataDeclCtxtClassKeys


    mk_eqn_help NewType tycon clas tys
      =	doptsTc Opt_GlasgowExts			`thenTc` \ gla_exts ->
        if can_derive_via_isomorphism && (gla_exts || standard_instance) then
		-- Go ahead and use the isomorphism
       	   new_dfun_name clas tycon  		`thenNF_Tc` \ dfun_name ->
	   returnTc (Nothing, Just (NewTypeDerived (mk_dfun dfun_name)))
	else
	   if standard_instance then
		mk_eqn_help DataType tycon clas []	-- Go via bale-out route
	   else
	   	bale_out cant_derive_err
      where
	-- Here is the plan for newtype derivings.  We see
	--	  newtype T a1...an = T (t ak...an) deriving (C1...Cm)
	-- where aj...an do not occur free in t, and the Ci are *partial applications* of
	-- classes with the last parameter missing
	--
	-- We generate the instances
	--	 instance Ci (t ak...aj) => Ci (T a1...aj)
	-- where T a1...aj is the partial application of the LHS of the correct kind
	--
	-- Running example: newtype T s a = MkT (ST s a) deriving( Monad )

	kind = tyVarKind (last (classTyVars clas))
		-- Kind of the thing we want to instance
		--   e.g. argument kind of Monad, *->*

	(arg_kinds, _) = tcSplitFunTys kind
	n_args_to_drop = length arg_kinds	
		-- Want to drop 1 arg from (T s a) and (ST s a)
		-- to get 	instance Monad (ST s) => Monad (T s)

	(tyvars, rep_ty) 	   = newTyConRep tycon
 	maybe_rep_app		   = tcSplitTyConApp_maybe rep_ty	
	Just (rep_tc, rep_ty_args) = maybe_rep_app

	n_tyvars_to_keep = tyConArity tycon  - n_args_to_drop
	tyvars_to_drop   = drop n_tyvars_to_keep tyvars
	tyvars_to_keep   = take n_tyvars_to_keep tyvars

	n_args_to_keep = tyConArity rep_tc - n_args_to_drop
	args_to_drop   = drop n_args_to_keep rep_ty_args
 	args_to_keep   = take n_args_to_keep rep_ty_args

	ctxt_pred = mkClassPred clas (tys ++ [mkTyConApp rep_tc args_to_keep])

	mk_dfun dfun_name = mkDictFunId dfun_name clas tyvars 
		    				  (tys ++ [mkTyConApp tycon (mkTyVarTys tyvars_to_keep)] )
						  [ctxt_pred]

	-- We can only do this newtype deriving thing if:
	standard_instance = null tys && classKey clas `elem` derivableClassKeys

	can_derive_via_isomorphism
	   =  not (clas `hasKey` readClassKey)	-- Never derive Read,Show this way
	   && not (clas `hasKey` showClassKey)
	   && n_tyvars_to_keep >= 0		-- Well kinded; 
						-- eg not: newtype T = T Int deriving( Monad )
	   && isJust maybe_rep_app	 	-- The rep type is a type constructor app
	   && n_args_to_keep   >= 0		-- Well kinded: 
						-- eg not: newtype T a = T Int deriving( Monad )
	   && eta_ok				-- Eta reduction works

	-- Check that eta reduction is OK
	-- 	(a) the dropped-off args are identical
	--	(b) the remaining type args mention 
	--	    only the remaining type variables
	eta_ok = (args_to_drop `tcEqTypes` mkTyVarTys tyvars_to_drop)
	      && (tyVarsOfTypes args_to_keep `subVarSet` mkVarSet tyvars_to_keep) 

	cant_derive_err = derivingThingErr clas tys tycon tyvars_to_keep
					   SLIT("too hard for cunning newtype deriving")


    bale_out err = addErrTc err `thenNF_Tc_` returnNF_Tc (Nothing, Nothing) 

    ------------------------------------------------------------------
    chk_out :: Class -> TyCon -> [TcType] -> Maybe FastString
    chk_out clas tycon tys
	| not (null tys)						= Just non_std_why
	| not (getUnique clas `elem` derivableClassKeys)		= Just non_std_why
	| clas `hasKey` enumClassKey    && not is_enumeration 	        = Just nullary_why
	| clas `hasKey` boundedClassKey && not is_enumeration_or_single = Just single_nullary_why
	| clas `hasKey` ixClassKey      && not is_enumeration_or_single = Just single_nullary_why
	| null data_cons		    			 	= Just no_cons_why
	| any isExistentialDataCon data_cons 				= Just existential_why     
	| otherwise			     				= Nothing
	where
	    data_cons = tyConDataCons tycon
	    is_enumeration = isEnumerationTyCon tycon
	    is_single_con  = maybeToBool (maybeTyConSingleCon tycon)
	    is_enumeration_or_single = is_enumeration || is_single_con

    single_nullary_why = SLIT("one constructor data type or type with all nullary constructors expected")
    nullary_why        = SLIT("data type with all nullary constructors expected")
    no_cons_why	       = SLIT("type has no data constructors")
    non_std_why	       = SLIT("not a derivable class")
    existential_why    = SLIT("it has existentially-quantified constructor(s)")

new_dfun_name clas tycon 	-- Just a simple wrapper
  = newDFunName clas [mkTyConApp tycon []] (getSrcLoc tycon)
	-- The type passed to newDFunName is only used to generate
	-- a suitable string; hence the empty type arg list
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
solveDerivEqns :: InstEnv
	       -> [DerivEqn]
	       -> TcM [DFunId]	-- Solns in same order as eqns.
				-- This bunch is Absolutely minimal...

solveDerivEqns inst_env_in orig_eqns
  = iterateDeriv initial_solutions
  where
	-- The initial solutions for the equations claim that each
	-- instance has an empty context; this solution is certainly
	-- in canonical form.
    initial_solutions :: [DerivSoln]
    initial_solutions = [ [] | _ <- orig_eqns ]

    ------------------------------------------------------------------
	-- iterateDeriv calculates the next batch of solutions,
	-- compares it with the current one; finishes if they are the
	-- same, otherwise recurses with the new solutions.
	-- It fails if any iteration fails
    iterateDeriv :: [DerivSoln] ->TcM [DFunId]
    iterateDeriv current_solns
      = checkNoErrsTc (iterateOnce current_solns)
						`thenTc` \ (new_dfuns, new_solns) ->
	if (current_solns == new_solns) then
	    returnTc new_dfuns
	else
	    iterateDeriv new_solns

    ------------------------------------------------------------------
    iterateOnce current_solns
      =	    -- Extend the inst info from the explicit instance decls
	    -- with the current set of solutions, giving a
	getDOptsTc				`thenNF_Tc` \ dflags ->
        let 
	    new_dfuns = zipWithEqual "add_solns" mk_deriv_dfun orig_eqns current_solns
	    inst_env  = extend_inst_env dflags inst_env_in new_dfuns
	    -- the eqns and solns move "in lockstep"; we have the eqns
	    -- because we need the LHS info for addClassInstance.
        in
	    -- Simplify each RHS
	tcSetInstEnv inst_env (
	  listTc [ tcAddSrcLoc (getSrcLoc tc)	$
		   tcAddErrCtxt (derivCtxt tc)	$
		   tcSimplifyThetas deriv_rhs
	         | (_, _,tc,_,deriv_rhs) <- orig_eqns ]  
	)					`thenTc` \ next_solns ->

	    -- Canonicalise the solutions, so they compare nicely
	let canonicalised_next_solns = [ sortLt (<) next_soln | next_soln <- next_solns ]
	in
	returnTc (new_dfuns, canonicalised_next_solns)
\end{code}

\begin{code}
extend_inst_env dflags inst_env new_dfuns
  = new_inst_env
  where
    (new_inst_env, _errs) = extendInstEnv dflags inst_env new_dfuns
	-- Ignore the errors about duplicate instances.
	-- We don't want repeated error messages
	-- They'll appear later, when we do the top-level extendInstEnvs

mk_deriv_dfun (dfun_name, clas, tycon, tyvars, _) theta
  = mkDictFunId dfun_name clas tyvars 
		[mkTyConApp tycon (mkTyVarTys tyvars)] 
		theta
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
-- Generate the method bindings for the required instance
-- (paired with class name, as we need that when renaming
--  the method binds)
gen_bind :: (Name -> Maybe Fixity) -> DFunId -> (Name, RdrNameMonoBinds)
gen_bind get_fixity dfun
  = (cls_nm, binds)
  where
    cls_nm	  = className clas
    (clas, tycon) = simpleDFunClassTyCon dfun

    binds = assoc "gen_bind:bad derived class" gen_list 
		  (nameUnique cls_nm) tycon

    gen_list = [(eqClassKey,      gen_Eq_binds)
	       ,(ordClassKey,     gen_Ord_binds)
	       ,(enumClassKey,    gen_Enum_binds)
	       ,(boundedClassKey, gen_Bounded_binds)
	       ,(ixClassKey,      gen_Ix_binds)
	       ,(showClassKey,    gen_Show_binds get_fixity)
	       ,(readClassKey,    gen_Read_binds get_fixity)
	       ]
\end{code}


%************************************************************************
%*									*
\subsection[TcDeriv-taggery-Names]{What con2tag/tag2con functions are available?}
%*									*
%************************************************************************


data Foo ... = ...

con2tag_Foo :: Foo ... -> Int#
tag2con_Foo :: Int -> Foo ...	-- easier if Int, not Int#
maxtag_Foo  :: Int		-- ditto (NB: not unlifted)


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
gen_taggery_Names :: [DFunId]
		  -> TcM [(RdrName,	-- for an assoc list
		  	   TyCon,	-- related tycon
			   TagThingWanted)]

gen_taggery_Names dfuns
  = foldlTc do_con2tag []           tycons_of_interest `thenTc` \ names_so_far ->
    foldlTc do_tag2con names_so_far tycons_of_interest
  where
    all_CTs = map simpleDFunClassTyCon dfuns
    all_tycons		    = map snd all_CTs
    (tycons_of_interest, _) = removeDups compare all_tycons
    
    do_con2tag acc_Names tycon
      | isDataTyCon tycon &&
        ((we_are_deriving eqClassKey tycon
	    && any isNullaryDataCon (tyConDataCons tycon))
	 || (we_are_deriving ordClassKey  tycon
	    && not (maybeToBool (maybeTyConSingleCon tycon)))
	 || (we_are_deriving enumClassKey tycon)
	 || (we_are_deriving ixClassKey   tycon))
	
      = returnTc ((con2tag_RDR tycon, tycon, GenCon2Tag)
		   : acc_Names)
      | otherwise
      = returnTc acc_Names

    do_tag2con acc_Names tycon
      | isDataTyCon tycon &&
         (we_are_deriving enumClassKey tycon ||
	  we_are_deriving ixClassKey   tycon
	  && isEnumerationTyCon tycon)
      = returnTc ( (tag2con_RDR tycon, tycon, GenTag2Con)
		 : (maxtag_RDR  tycon, tycon, GenMaxTag)
		 : acc_Names)
      | otherwise
      = returnTc acc_Names

    we_are_deriving clas_key tycon
      = is_in_eqns clas_key tycon all_CTs
      where
	is_in_eqns clas_key tycon [] = False
	is_in_eqns clas_key tycon ((c,t):cts)
	  =  (clas_key == classKey c && tycon == t)
	  || is_in_eqns clas_key tycon cts
\end{code}

\begin{code}
derivingThingErr clas tys tycon tyvars why
  = sep [hsep [ptext SLIT("Can't make a derived instance of"), quotes (ppr pred)],
	 parens (ptext why)]
  where
    pred = mkClassPred clas (tys ++ [mkTyConApp tycon (mkTyVarTys tyvars)])

malformedPredErr tycon pred = ptext SLIT("Illegal deriving item") <+> ppr pred

derivCtxt tycon
  = ptext SLIT("When deriving classes for") <+> quotes (ppr tycon)
\end{code}

