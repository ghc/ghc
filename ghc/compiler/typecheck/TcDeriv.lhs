%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcDeriv]{Deriving}

Handles @deriving@ clauses on @data@ declarations.

\begin{code}
module TcDeriv ( tcDeriving ) where

#include "HsVersions.h"

import HsSyn		( HsBinds(..), MonoBinds(..), TyClDecl(..),
			  collectMonoBinders )
import RdrHsSyn		( RdrNameMonoBinds )
import RnHsSyn		( RenamedHsBinds, RenamedMonoBinds, RenamedTyClDecl, RenamedHsPred )
import CmdLineOpts	( DynFlag(..) )

import TcRnMonad
import TcEnv		( tcGetInstEnv, tcSetInstEnv, newDFunName, 
			  InstInfo(..), pprInstInfo, InstBindings(..),
			  pprInstInfoDetails, tcLookupTyCon, tcExtendTyVarEnv
			)
import TcGenDeriv	-- Deriv stuff
import InstEnv		( InstEnv, simpleDFunClassTyCon, extendInstEnv )
import TcMonoType	( tcHsPred )
import TcSimplify	( tcSimplifyDeriv )

import RnBinds		( rnMethodBinds, rnTopMonoBinds )
import RnEnv		( bindLocalsFVRn )
import TcRnMonad		( thenM, returnM, mapAndUnzipM )
import HscTypes		( DFunId )

import BasicTypes	( NewOrData(..) )
import Class		( className, classArity, classKey, classTyVars, classSCTheta, Class )
import Subst		( mkTyVarSubst, substTheta )
import ErrUtils		( dumpIfSet_dyn )
import MkId		( mkDictFunId )
import DataCon		( dataConRepArgTys, dataConOrigArgTys, isNullaryDataCon, isExistentialDataCon )
import Maybes		( maybeToBool, catMaybes )
import Name		( Name, getSrcLoc, nameUnique )
import NameSet
import RdrName		( RdrName )

import TyCon		( tyConTyVars, tyConDataCons, tyConArity, 
			  tyConTheta, maybeTyConSingleCon, isDataTyCon,
			  isEnumerationTyCon, isRecursiveTyCon, TyCon
			)
import TcType		( TcType, ThetaType, mkTyVarTys, mkTyConApp, getClassPredTys_maybe,
			  isUnLiftedType, mkClassPred, tyVarsOfTypes, tcSplitFunTys, 
			  tcEqTypes, tcSplitAppTys, mkAppTys )
import Var		( TyVar, tyVarKind )
import VarSet		( mkVarSet, subVarSet )
import PrelNames
import Util		( zipWithEqual, sortLt, notNull )
import ListSetOps	( removeDups,  assoc )
import Outputable
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

pprDerivEqn (n,c,tc,tvs,rhs)
  = parens (hsep [ppr n, ppr c, ppr tc, ppr tvs] <+> equals <+> ppr rhs)

type DerivRhs  = ThetaType
type DerivSoln = DerivRhs
\end{code}


[Data decl contexts] A note about contexts on data decls
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

FURTHER NOTE ADDED March 2002.  In fact, Haskell98 now requires that
pattern matching against a constructor from a data type with a context
gives rise to the constraints for that context -- or at least the thinned
version.  So now all classes are "offending".



%************************************************************************
%*									*
\subsection[TcDeriv-driver]{Top-level function for \tr{derivings}}
%*									*
%************************************************************************

\begin{code}
tcDeriving  :: [RenamedTyClDecl]	-- All type constructors
	    -> TcM ([InstInfo],		-- The generated "instance decls".
		    RenamedHsBinds,	-- Extra generated bindings
		    FreeVars)		-- These are free in the generated bindings

tcDeriving tycl_decls
  = recoverM (returnM ([], EmptyBinds, emptyFVs)) $
    getDOpts			`thenM` \ dflags ->
    tcGetInstEnv		`thenM` \ inst_env ->

  	-- Fish the "deriving"-related information out of the TcEnv
	-- and make the necessary "equations".
    makeDerivEqns tycl_decls		    		`thenM` \ (ordinary_eqns, newtype_inst_info) ->
    let
	-- Add the newtype-derived instances to the inst env
	-- before tacking the "ordinary" ones
	inst_env1 = extend_inst_env dflags inst_env 
				    (map iDFunId newtype_inst_info)
    in    
    deriveOrdinaryStuff inst_env1 ordinary_eqns		`thenM` \ (ordinary_inst_info, binds, fvs) ->
    let
	inst_info  = newtype_inst_info ++ ordinary_inst_info
    in

    ioToTcRn (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances" 
	     (ddump_deriving inst_info binds))		`thenM_`

    returnM (inst_info, binds, fvs)

  where
    ddump_deriving :: [InstInfo] -> RenamedHsBinds -> SDoc
    ddump_deriving inst_infos extra_binds
      = vcat (map ppr_info inst_infos) $$ ppr extra_binds

    ppr_info inst_info = pprInstInfo inst_info $$ 
			 nest 4 (pprInstInfoDetails inst_info)
	-- pprInstInfo doesn't print much: only the type

-----------------------------------------
deriveOrdinaryStuff inst_env_in []	-- Short cut
  = returnM ([], EmptyBinds, emptyFVs)

deriveOrdinaryStuff inst_env_in eqns
  =	-- Take the equation list and solve it, to deliver a list of
	-- solutions, a.k.a. the contexts for the instance decls
	-- required for the corresponding equations.
    solveDerivEqns inst_env_in eqns	    	`thenM` \ new_dfuns ->

	-- Now augment the InstInfos, adding in the rather boring
	-- actual-code-to-do-the-methods binds.  We may also need to
	-- generate extra not-one-inst-decl-specific binds, notably
	-- "con2tag" and/or "tag2con" functions.  We do these
	-- separately.
    gen_taggery_Names new_dfuns		`thenM` \ nm_alist_etc ->

    let
	extra_mbind_list = map gen_tag_n_con_monobind nm_alist_etc
	extra_mbinds     = foldr AndMonoBinds EmptyMonoBinds extra_mbind_list
	mbinders	 = collectMonoBinders extra_mbinds
    in
    mappM gen_bind new_dfuns		`thenM` \ method_binds_s ->
	
    traceTc (text "tcDeriv" <+> ppr method_binds_s)	`thenM_`
    getModule						`thenM` \ this_mod ->
    initRn (InterfaceMode this_mod) (
	-- Rename to get RenamedBinds.
	-- The only tricky bit is that the extra_binds must scope 
	-- over the method bindings for the instances.
	bindLocalsFVRn (ptext (SLIT("deriving"))) mbinders	$ \ _ ->
	rnTopMonoBinds extra_mbinds []			`thenM` \ (rn_extra_binds, fvs) ->
	mapAndUnzipM rn_meths method_binds_s		`thenM` \ (rn_method_binds_s, fvs_s) ->
	returnM ((rn_method_binds_s, rn_extra_binds), 
		  fvs `plusFV` plusFVs fvs_s)
    )				`thenM` \ ((rn_method_binds_s, rn_extra_binds), fvs) ->
    let
	new_inst_infos = zipWith gen_inst_info new_dfuns rn_method_binds_s
    in
    returnM (new_inst_infos, rn_extra_binds, fvs)

  where
	-- Make a Real dfun instead of the dummy one we have so far
    gen_inst_info :: DFunId -> RenamedMonoBinds -> InstInfo
    gen_inst_info dfun binds
      = InstInfo { iDFunId = dfun, iBinds = VanillaInst binds [] }

    rn_meths (cls, meths) = rnMethodBinds cls [] meths
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
  = mapAndUnzipM mk_eqn derive_these	 	`thenM` \ (maybe_ordinaries, maybe_newtypes) ->
    returnM (catMaybes maybe_ordinaries, catMaybes maybe_newtypes)
  where
    ------------------------------------------------------------------
    derive_these :: [(NewOrData, Name, RenamedHsPred)]
	-- Find the (nd, TyCon, Pred) pairs that must be `derived'
	-- NB: only source-language decls have deriving, no imported ones do
    derive_these = [ (nd, tycon, pred) 
		   | TyData {tcdND = nd, tcdName = tycon, tcdDerivs = Just preds} <- tycl_decls,
		     pred <- preds ]

    ------------------------------------------------------------------
    mk_eqn :: (NewOrData, Name, RenamedHsPred) -> TcM (Maybe DerivEqn, Maybe InstInfo)
	-- We swizzle the tyvars and datacons out of the tycon
	-- to make the rest of the equation

    mk_eqn (new_or_data, tycon_name, pred)
      = tcLookupTyCon tycon_name		`thenM` \ tycon ->
	addSrcLoc (getSrcLoc tycon)		$
        addErrCtxt (derivCtxt Nothing tycon)	$
	tcExtendTyVarEnv (tyConTyVars tycon)	$	-- Deriving preds may (now) mention
							-- the type variables for the type constructor
        tcHsPred pred				`thenM` \ pred' ->
	case getClassPredTys_maybe pred' of
	   Nothing 	    -> bale_out (malformedPredErr tycon pred)
	   Just (clas, tys) -> mk_eqn_help new_or_data tycon clas tys

    ------------------------------------------------------------------
    mk_eqn_help DataType tycon clas tys
      | Just err <- chk_out clas tycon tys
      = bale_out (derivingThingErr clas tys tycon tyvars err)
      | otherwise 
      = new_dfun_name clas tycon	 `thenM` \ dfun_name ->
	returnM (Just (dfun_name, clas, tycon, tyvars, constraints), Nothing)
      where
	tyvars    = tyConTyVars tycon
	data_cons = tyConDataCons tycon
	constraints = extra_constraints ++ 
	  	      [ mkClassPred clas [arg_ty] 
		      | data_con <- tyConDataCons tycon,
		        arg_ty   <- dataConRepArgTys data_con,		-- dataConOrigArgTys???
				-- Use the same type variables
				-- as the type constructor,
				-- hence no need to instantiate
			not (isUnLiftedType arg_ty)	-- No constraints for unlifted types?
		      ]

	 -- "extra_constraints": see note [Data decl contexts] above
	extra_constraints = tyConTheta tycon

    mk_eqn_help NewType tycon clas tys
      =	doptM Opt_GlasgowExts			`thenM` \ gla_exts ->
        if can_derive_via_isomorphism && (gla_exts || standard_instance) then
		-- Go ahead and use the isomorphism
	   traceTc (text "newtype deriving:" <+> ppr tycon <+> ppr rep_tys)	`thenM_`
       	   new_dfun_name clas tycon  		`thenM` \ dfun_name ->
	   returnM (Nothing, Just (InstInfo { iDFunId = mk_dfun dfun_name,
					      iBinds = NewTypeDerived rep_tys }))
	else
	   if standard_instance then
		mk_eqn_help DataType tycon clas []	-- Go via bale-out route
	   else
	   	bale_out cant_derive_err
      where
	-- Here is the plan for newtype derivings.  We see
	--	  newtype T a1...an = T (t ak...an) deriving (.., C s1 .. sm, ...)
	-- where aj...an do not occur free in t, and the (C s1 ... sm) is a 
	-- *partial applications* of class C with the last parameter missing
	--
	-- We generate the instances
	--	 instance C s1 .. sm (t ak...aj) => C s1 .. sm (T a1...aj)
	-- where T a1...aj is the partial application of the LHS of the correct kind
	--
	-- Running example: newtype T s a = MkT (ST s a) deriving( Monad )
	--	instance Monad (ST s) => Monad (T s) where 
	--	  fail = coerce ... (fail @ ST s)

	clas_tyvars = classTyVars clas
	kind = tyVarKind (last clas_tyvars)
		-- Kind of the thing we want to instance
		--   e.g. argument kind of Monad, *->*

	(arg_kinds, _) = tcSplitFunTys kind
	n_args_to_drop = length arg_kinds	
		-- Want to drop 1 arg from (T s a) and (ST s a)
		-- to get 	instance Monad (ST s) => Monad (T s)

	-- Note [newtype representation]
	-- We must not use newTyConRep to get the representation 
	-- type, because that looks through all intermediate newtypes
	-- To get the RHS of *this* newtype, just look at the data
	-- constructor.  For example
	--	newtype B = MkB Int
	--	newtype A = MkA B deriving( Num )
	-- We want the Num instance of B, *not* the Num instance of Int,
	-- when making the Num instance of A!
	tyvars 		      = tyConTyVars tycon
        rep_ty 		      = head (dataConOrigArgTys (head (tyConDataCons tycon)))
	(rep_fn, rep_ty_args) = tcSplitAppTys rep_ty

	n_tyvars_to_keep = tyConArity tycon  - n_args_to_drop
	tyvars_to_drop   = drop n_tyvars_to_keep tyvars
	tyvars_to_keep   = take n_tyvars_to_keep tyvars

	n_args_to_keep = length rep_ty_args - n_args_to_drop
	args_to_drop   = drop n_args_to_keep rep_ty_args
 	args_to_keep   = take n_args_to_keep rep_ty_args

	rep_tys  = tys ++ [mkAppTys rep_fn args_to_keep]
	rep_pred = mkClassPred clas rep_tys
		-- rep_pred is the representation dictionary, from where
		-- we are gong to get all the methods for the newtype dictionary

	inst_tys = (tys ++ [mkTyConApp tycon (mkTyVarTys tyvars_to_keep)])
		-- The 'tys' here come from the partial application
		-- in the deriving clause. The last arg is the new
		-- instance type.

		-- We must pass the superclasses; the newtype might be an instance
		-- of them in a different way than the representation type
		-- E.g.		newtype Foo a = Foo a deriving( Show, Num, Eq )
		-- Then the Show instance is not done via isomprphism; it shows
		-- 	Foo 3 as "Foo 3"
		-- The Num instance is derived via isomorphism, but the Show superclass
		-- dictionary must the Show instance for Foo, *not* the Show dictionary
		-- gotten from the Num dictionary. So we must build a whole new dictionary
		-- not just use the Num one.  The instance we want is something like:
		--	instance (Num a, Show (Foo a), Eq (Foo a)) => Num (Foo a) where
		--		(+) = ((+)@a)
		--		...etc...
		-- There's no 'corece' needed because after the type checker newtypes
		-- are transparent.

	sc_theta = substTheta (mkTyVarSubst clas_tyvars inst_tys)
			      (classSCTheta clas)

		-- If there are no tyvars, there's no need
		-- to abstract over the dictionaries we need
	dict_args | null tyvars = []
		  | otherwise   = rep_pred : sc_theta

		-- Finally! Here's where we build the dictionary Id
	mk_dfun dfun_name = mkDictFunId dfun_name tyvars dict_args clas inst_tys

	-------------------------------------------------------------------
	--  Figuring out whether we can only do this newtype-deriving thing

	standard_instance = null tys && classKey clas `elem` derivableClassKeys

	can_derive_via_isomorphism
	   =  not (clas `hasKey` readClassKey)	-- Never derive Read,Show this way
	   && not (clas `hasKey` showClassKey)
	   && length tys + 1 == classArity clas	-- Well kinded;
						-- eg not: newtype T ... deriving( ST )
						--	because ST needs *2* type params
	   && n_tyvars_to_keep >= 0		-- Well kinded; 
						-- eg not: newtype T = T Int deriving( Monad )
	   && n_args_to_keep   >= 0		-- Well kinded: 
						-- eg not: newtype T a = T Int deriving( Monad )
	   && eta_ok				-- Eta reduction works
	   && not (isRecursiveTyCon tycon)	-- Does not work for recursive tycons:
						--	newtype A = MkA [A]
						-- Don't want
						--	instance Eq [A] => Eq A !!

	-- Check that eta reduction is OK
	-- 	(a) the dropped-off args are identical
	--	(b) the remaining type args mention 
	--	    only the remaining type variables
	eta_ok = (args_to_drop `tcEqTypes` mkTyVarTys tyvars_to_drop)
	      && (tyVarsOfTypes args_to_keep `subVarSet` mkVarSet tyvars_to_keep) 

	cant_derive_err = derivingThingErr clas tys tycon tyvars_to_keep
				(vcat [ptext SLIT("too hard for cunning newtype deriving"),
				       ptext SLIT("debug info:") <+> ppr n_tyvars_to_keep <+>
					ppr n_args_to_keep <+> ppr eta_ok <+>
					ppr (isRecursiveTyCon tycon)
				      ])

    bale_out err = addErrTc err `thenM_` returnM (Nothing, Nothing) 

    ------------------------------------------------------------------
    chk_out :: Class -> TyCon -> [TcType] -> Maybe SDoc
    chk_out clas tycon tys
	| notNull tys						        = Just ty_args_why
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

	    single_nullary_why = ptext SLIT("one constructor data type or type with all nullary constructors expected")
	    nullary_why        = quotes (ppr tycon) <+> ptext SLIT("has non-nullary constructors")
	    no_cons_why	       = quotes (ppr tycon) <+> ptext SLIT("has no data constructors")
	    ty_args_why	       = quotes (ppr pred) <+> ptext SLIT("is not a class")
   	    non_std_why	       = quotes (ppr clas) <+> ptext SLIT("is not a derivable class")
	    existential_why    = quotes (ppr tycon) <+> ptext SLIT("has existentially-quantified constructor(s)")

	    pred = mkClassPred clas tys

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
  = iterateDeriv 1 initial_solutions
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
    iterateDeriv :: Int -> [DerivSoln] ->TcM [DFunId]
    iterateDeriv n current_solns
      | n > 20 	-- Looks as if we are in an infinite loop
		-- This can happen if we have -fallow-undecidable-instances
		-- (See TcSimplify.tcSimplifyDeriv.)
      = pprPanic "solveDerivEqns: probable loop" 
		 (vcat (map pprDerivEqn orig_eqns) $$ ppr current_solns)
      | otherwise
      =	getDOpts				`thenM` \ dflags ->
        let 
	    dfuns    = zipWithEqual "add_solns" mk_deriv_dfun orig_eqns current_solns
	    inst_env = extend_inst_env dflags inst_env_in dfuns
        in
        checkNoErrs (
		  -- Extend the inst info from the explicit instance decls
		  -- with the current set of solutions, and simplify each RHS
	    tcSetInstEnv inst_env $
	    mappM gen_soln orig_eqns
	)				`thenM` \ new_solns ->
	if (current_solns == new_solns) then
	    returnM dfuns
	else
	    iterateDeriv (n+1) new_solns

    ------------------------------------------------------------------

    gen_soln (_, clas, tc,tyvars,deriv_rhs)
      = addSrcLoc (getSrcLoc tc)		$
	addErrCtxt (derivCtxt (Just clas) tc)	$
	tcSimplifyDeriv tyvars deriv_rhs	`thenM` \ theta ->
	returnM (sortLt (<) theta)	-- Canonicalise before returning the soluction
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
  = mkDictFunId dfun_name tyvars theta
		clas [mkTyConApp tycon (mkTyVarTys tyvars)] 
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
gen_bind :: DFunId -> TcM (Name, RdrNameMonoBinds)
gen_bind dfun
  = getFixityEnv		`thenM` \ fix_env -> 
    returnM (cls_nm, gen_binds_fn fix_env cls_nm tycon)
  where
    cls_nm	  = className clas
    (clas, tycon) = simpleDFunClassTyCon dfun

gen_binds_fn fix_env cls_nm
  = assoc "gen_bind:bad derived class"
	  gen_list (nameUnique cls_nm)
  where
    gen_list = [(eqClassKey,      gen_Eq_binds)
	       ,(ordClassKey,     gen_Ord_binds)
	       ,(enumClassKey,    gen_Enum_binds)
	       ,(boundedClassKey, gen_Bounded_binds)
	       ,(ixClassKey,      gen_Ix_binds)
	       ,(showClassKey,    gen_Show_binds fix_env)
	       ,(readClassKey,    gen_Read_binds fix_env)
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
  = foldlM do_con2tag []           tycons_of_interest `thenM` \ names_so_far ->
    foldlM do_tag2con names_so_far tycons_of_interest
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
	
      = returnM ((con2tag_RDR tycon, tycon, GenCon2Tag)
		   : acc_Names)
      | otherwise
      = returnM acc_Names

    do_tag2con acc_Names tycon
      | isDataTyCon tycon &&
         (we_are_deriving enumClassKey tycon ||
	  we_are_deriving ixClassKey   tycon
	  && isEnumerationTyCon tycon)
      = returnM ( (tag2con_RDR tycon, tycon, GenTag2Con)
		 : (maxtag_RDR  tycon, tycon, GenMaxTag)
		 : acc_Names)
      | otherwise
      = returnM acc_Names

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
	 parens why]
  where
    pred = mkClassPred clas (tys ++ [mkTyConApp tycon (mkTyVarTys tyvars)])

malformedPredErr tycon pred = ptext SLIT("Illegal deriving item") <+> ppr pred

derivCtxt :: Maybe Class -> TyCon -> SDoc
derivCtxt maybe_cls tycon
  = ptext SLIT("When deriving") <+> cls <+> ptext SLIT("for type") <+> quotes (ppr tycon)
  where
    cls = case maybe_cls of
	    Nothing -> ptext SLIT("instances")
	    Just c  -> ptext SLIT("the") <+> quotes (ppr c) <+> ptext SLIT("instance")
\end{code}

