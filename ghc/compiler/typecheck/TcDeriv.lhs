%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcDeriv]{Deriving}

Handles @deriving@ clauses on @data@ declarations.

\begin{code}
module TcDeriv ( tcDeriving ) where

#include "HsVersions.h"

import HsSyn
import DynFlags	( DynFlag(..) )

import Generics		( mkTyConGenericBinds )
import TcRnMonad
import TcEnv		( newDFunName, pprInstInfoDetails, 
			  InstInfo(..), InstBindings(..),
			  tcLookupClass, tcLookupTyCon, tcExtendTyVarEnv
			)
import TcGenDeriv	-- Deriv stuff
import InstEnv		( simpleDFunClassTyCon, extendInstEnvList )
import TcHsType		( tcHsDeriv )
import TcSimplify	( tcSimplifyDeriv )

import RnBinds		( rnMethodBinds, rnTopBinds )
import RnEnv		( bindLocalNames )
import HscTypes		( DFunId, FixityEnv )

import Class		( className, classArity, classKey, classTyVars, classSCTheta, Class )
import Type		( zipOpenTvSubst, substTheta )
import ErrUtils		( dumpIfSet_dyn )
import MkId		( mkDictFunId )
import DataCon		( isNullarySrcDataCon, isVanillaDataCon, dataConOrigArgTys )
import Maybes		( catMaybes )
import RdrName		( RdrName )
import Name		( Name, getSrcLoc )
import NameSet		( duDefs )
import Kind		( splitKindFunTys )
import TyCon		( tyConTyVars, tyConDataCons, tyConArity, tyConHasGenerics,
			  tyConStupidTheta, isProductTyCon, isDataTyCon, newTyConRhs,
			  isEnumerationTyCon, isRecursiveTyCon, TyCon
			)
import TcType		( TcType, ThetaType, mkTyVarTys, mkTyConApp, tcTyConAppTyCon,
			  isUnLiftedType, mkClassPred, tyVarsOfTypes, isArgTypeKind,
			  tcEqTypes, tcSplitAppTys, mkAppTys, tcSplitDFunTy )
import Var		( TyVar, tyVarKind, idType, varName )
import VarSet		( mkVarSet, subVarSet )
import PrelNames
import SrcLoc		( srcLocSpan, Located(..) )
import Util		( zipWithEqual, sortLe, notNull )
import ListSetOps	( removeDups,  assocMaybe )
import Outputable
import Bag
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

[Newtype deriving]
~~~~~~~~~~~~~~~~~~
Consider this:
    class C a b
    instance C [a] Char
    newtype T = T Char deriving( C [a] )

Notice the free 'a' in the deriving.  We have to fill this out to 
    newtype T = T Char deriving( forall a. C [a] )

And then translate it to:
    instance C [a] Char => C [a] T where ...
    
	


%************************************************************************
%*									*
\subsection[TcDeriv-driver]{Top-level function for \tr{derivings}}
%*									*
%************************************************************************

\begin{code}
tcDeriving  :: [LTyClDecl Name]	-- All type constructors
	    -> TcM ([InstInfo],		-- The generated "instance decls"
		    [HsBindGroup Name])	-- Extra generated top-level bindings

tcDeriving tycl_decls
  = recoverM (returnM ([], [])) $
    do	{   	-- Fish the "deriving"-related information out of the TcEnv
		-- and make the necessary "equations".
	; (ordinary_eqns, newtype_inst_info) <- makeDerivEqns tycl_decls

	; (ordinary_inst_info, deriv_binds) 
		<- extendLocalInstEnv (map iDFunId newtype_inst_info)  $
		   deriveOrdinaryStuff ordinary_eqns
		-- Add the newtype-derived instances to the inst env
		-- before tacking the "ordinary" ones

	; let inst_info  = newtype_inst_info ++ ordinary_inst_info

	-- If we are compiling a hs-boot file, 
	-- don't generate any derived bindings
	; is_boot <- tcIsHsBoot
	; if is_boot then
		return (inst_info, [])
	  else do
	{

	-- Generate the generic to/from functions from each type declaration
	; gen_binds <- mkGenericBinds tycl_decls

	-- Rename these extra bindings, discarding warnings about unused bindings etc
	-- Set -fglasgow exts so that we can have type signatures in patterns,
	-- which is used in the generic binds
	; rn_binds
		<- discardWarnings $ setOptM Opt_GlasgowExts $ do
			{ (rn_deriv, _dus1) <- rnTopBinds deriv_binds []
			; (rn_gen, dus_gen) <- rnTopBinds gen_binds   []
			; keepAliveSetTc (duDefs dus_gen)	-- Mark these guys to
								-- be kept alive
			; return (rn_deriv ++ rn_gen) }


	; dflags <- getDOpts
	; ioToTcRn (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances" 
	  	   (ddump_deriving inst_info rn_binds))

	; returnM (inst_info, rn_binds)
	}}
  where
    ddump_deriving :: [InstInfo] -> [HsBindGroup Name] -> SDoc
    ddump_deriving inst_infos extra_binds
      = vcat (map pprInstInfoDetails inst_infos) $$ vcat (map ppr extra_binds)

-----------------------------------------
deriveOrdinaryStuff []	-- Short cut
  = returnM ([], emptyLHsBinds)

deriveOrdinaryStuff eqns
  = do	{	-- Take the equation list and solve it, to deliver a list of
		-- solutions, a.k.a. the contexts for the instance decls
		-- required for the corresponding equations.
	; new_dfuns <- solveDerivEqns eqns

	-- Generate the InstInfo for each dfun, 
	-- plus any auxiliary bindings it needs
	; (inst_infos, aux_binds_s) <- mapAndUnzipM genInst new_dfuns

	-- Generate any extra not-one-inst-decl-specific binds, 
	-- notably "con2tag" and/or "tag2con" functions.  
	; extra_binds <- genTaggeryBinds new_dfuns

	-- Done
	; returnM (inst_infos, unionManyBags (extra_binds : aux_binds_s))
   }

-----------------------------------------
mkGenericBinds tycl_decls
  = do	{ tcs <- mapM tcLookupTyCon 
			[ tc_name | 
			  L _ (TyData { tcdLName = L _ tc_name }) <- tycl_decls]
		-- We are only interested in the data type declarations
	; return (unionManyBags [ mkTyConGenericBinds tc | 
				  tc <- tcs, tyConHasGenerics tc ]) }
		-- And then only in the ones whose 'has-generics' flag is on
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
makeDerivEqns :: [LTyClDecl Name] 
	      -> TcM ([DerivEqn],	-- Ordinary derivings
		      [InstInfo])	-- Special newtype derivings

makeDerivEqns tycl_decls
  = mapAndUnzipM mk_eqn derive_these	 	`thenM` \ (maybe_ordinaries, maybe_newtypes) ->
    returnM (catMaybes maybe_ordinaries, catMaybes maybe_newtypes)
  where
    ------------------------------------------------------------------
    derive_these :: [(NewOrData, Name, LHsType Name)]
	-- Find the (nd, TyCon, Pred) pairs that must be `derived'
    derive_these = [ (nd, tycon, pred) 
		   | L _ (TyData { tcdND = nd, tcdLName = L _ tycon, 
			          tcdDerivs = Just preds }) <- tycl_decls,
		     pred <- preds ]

    ------------------------------------------------------------------
    mk_eqn :: (NewOrData, Name, LHsType Name) -> TcM (Maybe DerivEqn, Maybe InstInfo)
	-- We swizzle the tyvars and datacons out of the tycon
	-- to make the rest of the equation
	--
	-- The "deriv_ty" is a LHsType to take account of the fact that for newtype derivign
	-- we allow deriving (forall a. C [a]).

    mk_eqn (new_or_data, tycon_name, hs_deriv_ty)
      = tcLookupTyCon tycon_name		`thenM` \ tycon ->
	setSrcSpan (srcLocSpan (getSrcLoc tycon))		$
        addErrCtxt (derivCtxt Nothing tycon)	$
	tcExtendTyVarEnv (tyConTyVars tycon)	$	-- Deriving preds may (now) mention
							-- the type variables for the type constructor
	tcHsDeriv hs_deriv_ty			`thenM` \ (deriv_tvs, clas, tys) ->
	doptM Opt_GlasgowExts			`thenM` \ gla_exts ->
        mk_eqn_help gla_exts new_or_data tycon deriv_tvs clas tys

    ------------------------------------------------------------------
    mk_eqn_help gla_exts DataType tycon deriv_tvs clas tys
      | Just err <- checkSideConditions gla_exts tycon deriv_tvs clas tys
      = bale_out (derivingThingErr clas tys tycon (tyConTyVars tycon) err)
      | otherwise 
      = do { eqn <- mkDataTypeEqn tycon clas
	   ; returnM (Just eqn, Nothing) }

    mk_eqn_help gla_exts NewType tycon deriv_tvs clas tys
      | can_derive_via_isomorphism && (gla_exts || std_class_via_iso clas)
      =		-- Go ahead and use the isomorphism
	   traceTc (text "newtype deriving:" <+> ppr tycon <+> ppr rep_tys)	`thenM_`
       	   new_dfun_name clas tycon  		`thenM` \ dfun_name ->
	   returnM (Nothing, Just (InstInfo { iDFunId = mk_dfun dfun_name,
					      iBinds = NewTypeDerived rep_tys }))
      | std_class gla_exts clas
      = mk_eqn_help gla_exts DataType tycon deriv_tvs clas tys	-- Go via bale-out route

      | otherwise 				-- Non-standard instance
      = bale_out (if gla_exts then	
		   	cant_derive_err	-- Too hard
		  else
			non_std_err)	-- Just complain about being a non-std instance
      where
	-- Here is the plan for newtype derivings.  We see
	--	  newtype T a1...an = T (t ak...an) deriving (.., C s1 .. sm, ...)
	-- where t is a type,
	-- 	 ak...an is a suffix of a1..an
	--	 ak...an do not occur free in t, 
	-- 	 (C s1 ... sm) is a  *partial applications* of class C 
	--			with the last parameter missing
	--
	-- We generate the instances
	--	 instance C s1 .. sm (t ak...ap) => C s1 .. sm (T a1...ap)
	-- where T a1...ap is the partial application of the LHS of the correct kind
	-- and p >= k
	--
	-- Running example: newtype T s a = MkT (ST s a) deriving( Monad )
	--	instance Monad (ST s) => Monad (T s) where 
	--	  fail = coerce ... (fail @ ST s)
	-- (Actually we don't need the coerce, because non-rec newtypes are transparent

	clas_tyvars = classTyVars clas
	kind = tyVarKind (last clas_tyvars)
		-- Kind of the thing we want to instance
		--   e.g. argument kind of Monad, *->*

	(arg_kinds, _) = splitKindFunTys kind
	n_args_to_drop = length arg_kinds	
		-- Want to drop 1 arg from (T s a) and (ST s a)
		-- to get 	instance Monad (ST s) => Monad (T s)

	-- Note [newtype representation]
	-- Need newTyConRhs *not* newTyConRep to get the representation 
	-- type, because the latter looks through all intermediate newtypes
	-- For example
	--	newtype B = MkB Int
	--	newtype A = MkA B deriving( Num )
	-- We want the Num instance of B, *not* the Num instance of Int,
	-- when making the Num instance of A!
        (tc_tvs, rep_ty)      = newTyConRhs tycon
	(rep_fn, rep_ty_args) = tcSplitAppTys rep_ty

	n_tyvars_to_keep = tyConArity tycon  - n_args_to_drop
	tyvars_to_drop   = drop n_tyvars_to_keep tc_tvs
	tyvars_to_keep   = take n_tyvars_to_keep tc_tvs

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

	sc_theta = substTheta (zipOpenTvSubst clas_tyvars inst_tys)
			      (classSCTheta clas)

		-- If there are no tyvars, there's no need
		-- to abstract over the dictionaries we need
	dict_tvs = deriv_tvs ++ tc_tvs
	dict_args | null dict_tvs = []
		  | otherwise     = rep_pred : sc_theta

		-- Finally! Here's where we build the dictionary Id
	mk_dfun dfun_name = mkDictFunId dfun_name dict_tvs dict_args clas inst_tys

	-------------------------------------------------------------------
	--  Figuring out whether we can only do this newtype-deriving thing

	right_arity = length tys + 1 == classArity clas

		-- Never derive Read,Show,Typeable,Data this way 
	non_iso_classes = [readClassKey, showClassKey, typeableClassKey, dataClassKey]
	can_derive_via_isomorphism
	   =  not (getUnique clas `elem` non_iso_classes)
	   && right_arity 			-- Well kinded;
						-- eg not: newtype T ... deriving( ST )
						--	because ST needs *2* type params
	   && n_tyvars_to_keep >= 0		-- Type constructor has right kind:
						-- eg not: newtype T = T Int deriving( Monad )
	   && n_args_to_keep   >= 0		-- Rep type has right kind: 
						-- eg not: newtype T a = T Int deriving( Monad )
	   && eta_ok				-- Eta reduction works
	   && not (isRecursiveTyCon tycon)	-- Does not work for recursive tycons:
						--	newtype A = MkA [A]
						-- Don't want
						--	instance Eq [A] => Eq A !!
			-- Here's a recursive newtype that's actually OK
			--	newtype S1 = S1 [T1 ()]
			--	newtype T1 a = T1 (StateT S1 IO a ) deriving( Monad )
			-- It's currently rejected.  Oh well.
			-- In fact we generate an instance decl that has method of form
			--	meth @ instTy = meth @ repTy
			-- (no coerce's).  We'd need a coerce if we wanted to handle
			-- recursive newtypes too

	-- Check that eta reduction is OK
	-- 	(a) the dropped-off args are identical
	--	(b) the remaining type args mention 
	--	    only the remaining type variables
	eta_ok = (args_to_drop `tcEqTypes` mkTyVarTys tyvars_to_drop)
	      && (tyVarsOfTypes args_to_keep `subVarSet` mkVarSet tyvars_to_keep) 

	cant_derive_err = derivingThingErr clas tys tycon tyvars_to_keep
				(vcat [ptext SLIT("even with cunning newtype deriving:"),
					if isRecursiveTyCon tycon then
					  ptext SLIT("the newtype is recursive")
					else empty,
					if not right_arity then 
					  quotes (ppr (mkClassPred clas tys)) <+> ptext SLIT("does not have arity 1")
					else empty,
					if not (n_tyvars_to_keep >= 0) then 
					  ptext SLIT("the type constructor has wrong kind")
					else if not (n_args_to_keep >= 0) then
					  ptext SLIT("the representation type has wrong kind")
					else if not eta_ok then 
					  ptext SLIT("the eta-reduction property does not hold")
					else empty
				      ])

	non_std_err = derivingThingErr clas tys tycon tyvars_to_keep
				(vcat [non_std_why clas,
				       ptext SLIT("Try -fglasgow-exts for GHC's newtype-deriving extension")])

    bale_out err = addErrTc err `thenM_` returnM (Nothing, Nothing) 

std_class gla_exts clas 
  =  key `elem` derivableClassKeys
  || (gla_exts && (key == typeableClassKey || key == dataClassKey))
  where
     key = classKey clas
    
std_class_via_iso clas	-- These standard classes can be derived for a newtype
			-- using the isomorphism trick *even if no -fglasgow-exts*
  = classKey clas `elem`  [eqClassKey, ordClassKey, ixClassKey, boundedClassKey]
	-- Not Read/Show because they respect the type
	-- Not Enum, becuase newtypes are never in Enum


new_dfun_name clas tycon 	-- Just a simple wrapper
  = newDFunName clas [mkTyConApp tycon []] (getSrcLoc tycon)
	-- The type passed to newDFunName is only used to generate
	-- a suitable string; hence the empty type arg list

------------------------------------------------------------------
mkDataTypeEqn :: TyCon -> Class -> TcM DerivEqn
mkDataTypeEqn tycon clas
  | clas `hasKey` typeableClassKey
  =	-- The Typeable class is special in several ways
	-- 	  data T a b = ... deriving( Typeable )
	-- gives
	--	  instance Typeable2 T where ...
	-- Notice that:
	-- 1. There are no constraints in the instance
	-- 2. There are no type variables either
	-- 3. The actual class we want to generate isn't necessarily
	--	Typeable; it depends on the arity of the type
    do	{ real_clas <- tcLookupClass (typeableClassNames !! tyConArity tycon)
	; dfun_name <- new_dfun_name real_clas tycon
  	; return (dfun_name, real_clas, tycon, [], []) }

  | otherwise
  = do	{ dfun_name <- new_dfun_name clas tycon
  	; return (dfun_name, clas, tycon, tyvars, constraints) }
  where
    tyvars            = tyConTyVars tycon
    constraints       = extra_constraints ++ ordinary_constraints
    extra_constraints = tyConStupidTheta tycon
	 -- "extra_constraints": see note [Data decl contexts] above

    ordinary_constraints
      = [ mkClassPred clas [arg_ty] 
        | data_con <- tyConDataCons tycon,
          arg_ty   <- dataConOrigArgTys data_con,
          not (isUnLiftedType arg_ty)	-- No constraints for unlifted types?
        ]


------------------------------------------------------------------
-- Check side conditions that dis-allow derivability for particular classes
-- This is *apart* from the newtype-deriving mechanism

checkSideConditions :: Bool -> TyCon -> [TyVar] -> Class -> [TcType] -> Maybe SDoc
checkSideConditions gla_exts tycon deriv_tvs clas tys
  | notNull deriv_tvs || notNull tys	
  = Just ty_args_why	-- e.g. deriving( Foo s )
  | otherwise
  = case [cond | (key,cond) <- sideConditions, key == getUnique clas] of
	[]     -> Just (non_std_why clas)
	[cond] -> cond (gla_exts, tycon)
	other  -> pprPanic "checkSideConditions" (ppr clas)
  where
    ty_args_why	= quotes (ppr (mkClassPred clas tys)) <+> ptext SLIT("is not a class")

non_std_why clas = quotes (ppr clas) <+> ptext SLIT("is not a derivable class")

sideConditions :: [(Unique, Condition)]
sideConditions
  = [	(eqClassKey, 	   cond_std),
	(ordClassKey,	   cond_std),
	(readClassKey,	   cond_std),
	(showClassKey,	   cond_std),
	(enumClassKey,	   cond_std `andCond` cond_isEnumeration),
	(ixClassKey,	   cond_std `andCond` (cond_isEnumeration `orCond` cond_isProduct)),
	(boundedClassKey,  cond_std `andCond` (cond_isEnumeration `orCond` cond_isProduct)),
 	(typeableClassKey, cond_glaExts `andCond` cond_typeableOK),
	(dataClassKey,	   cond_glaExts `andCond` cond_std)
    ]

type Condition = (Bool, TyCon) -> Maybe SDoc	-- Nothing => OK

orCond :: Condition -> Condition -> Condition
orCond c1 c2 tc 
  = case c1 tc of
	Nothing -> Nothing		-- c1 succeeds
	Just x  -> case c2 tc of	-- c1 fails
		     Nothing -> Nothing
		     Just y  -> Just (x $$ ptext SLIT("  and") $$ y)
					-- Both fail

andCond c1 c2 tc = case c1 tc of
		     Nothing -> c2 tc	-- c1 succeeds
		     Just x  -> Just x	-- c1 fails

cond_std :: Condition
cond_std (gla_exts, tycon)
  | any (not . isVanillaDataCon) data_cons = Just existential_why     
  | null data_cons		    	   = Just no_cons_why
  | otherwise      			   = Nothing
  where
    data_cons       = tyConDataCons tycon
    no_cons_why	    = quotes (ppr tycon) <+> ptext SLIT("has no data constructors")
    existential_why = quotes (ppr tycon) <+> ptext SLIT("has existentially-quantified constructor(s)")
  
cond_isEnumeration :: Condition
cond_isEnumeration (gla_exts, tycon)
  | isEnumerationTyCon tycon = Nothing
  | otherwise		     = Just why
  where
    why = quotes (ppr tycon) <+> ptext SLIT("has non-nullary constructors")

cond_isProduct :: Condition
cond_isProduct (gla_exts, tycon)
  | isProductTyCon tycon = Nothing
  | otherwise	         = Just why
  where
    why = quotes (ppr tycon) <+> ptext SLIT("has more than one constructor")

cond_typeableOK :: Condition
-- OK for Typeable class
-- Currently: (a) args all of kind *
--	      (b) 7 or fewer args
cond_typeableOK (gla_exts, tycon)
  | tyConArity tycon > 7				      = Just too_many
  | not (all (isArgTypeKind . tyVarKind) (tyConTyVars tycon)) = Just bad_kind
  | otherwise	  				              = Nothing
  where
    too_many = quotes (ppr tycon) <+> ptext SLIT("has too many arguments")
    bad_kind = quotes (ppr tycon) <+> ptext SLIT("has arguments of kind other than `*'")

cond_glaExts :: Condition
cond_glaExts (gla_exts, tycon) | gla_exts  = Nothing
			       | otherwise = Just why
  where
    why  = ptext SLIT("You need -fglasgow-exts to derive an instance for this class")
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
solveDerivEqns :: [DerivEqn]
	       -> TcM [DFunId]	-- Solns in same order as eqns.
				-- This bunch is Absolutely minimal...

solveDerivEqns orig_eqns
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
      =	let 
	    dfuns = zipWithEqual "add_solns" mk_deriv_dfun orig_eqns current_solns
        in
        checkNoErrs (
		  -- Extend the inst info from the explicit instance decls
		  -- with the current set of solutions, and simplify each RHS
	    extendLocalInstEnv dfuns $
	    mappM gen_soln orig_eqns
	)				`thenM` \ new_solns ->
	if (current_solns == new_solns) then
	    returnM dfuns
	else
	    iterateDeriv (n+1) new_solns

    ------------------------------------------------------------------

    gen_soln (_, clas, tc,tyvars,deriv_rhs)
      = setSrcSpan (srcLocSpan (getSrcLoc tc))		$
	addErrCtxt (derivCtxt (Just clas) tc)	$
	tcSimplifyDeriv tyvars deriv_rhs	`thenM` \ theta ->
	returnM (sortLe (<=) theta)	-- Canonicalise before returning the soluction

mk_deriv_dfun (dfun_name, clas, tycon, tyvars, _) theta
  = mkDictFunId dfun_name tyvars theta
		clas [mkTyConApp tycon (mkTyVarTys tyvars)] 

extendLocalInstEnv :: [DFunId] -> TcM a -> TcM a
-- Add new locall-defined instances; don't bother to check
-- for functional dependency errors -- that'll happen in TcInstDcls
extendLocalInstEnv dfuns thing_inside
 = do { env <- getGblEnv
      ; let  inst_env' = extendInstEnvList (tcg_inst_env env) dfuns 
	     env'      = env { tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }
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
producing @LHsBinds Name@ for the methods, but that means
producing correctly-uniquified code on the fly.  This is entirely
possible (the @TcM@ monad has a @UniqueSupply@), but it is painful.
So, instead, we produce @MonoBinds RdrName@ then heave 'em through
the renamer.  What a great hack!
\end{itemize}

\begin{code}
-- Generate the InstInfo for the required instance,
-- plus any auxiliary bindings required
genInst :: DFunId -> TcM (InstInfo, LHsBinds RdrName)
genInst dfun
  = getFixityEnv		`thenM` \ fix_env -> 
    let
	(tyvars,_,clas,[ty]) 	= tcSplitDFunTy (idType dfun)
	clas_nm			= className clas
	tycon  	      	     	= tcTyConAppTyCon ty 
    	(meth_binds, aux_binds) = genDerivBinds clas fix_env tycon
    in
	-- Bring the right type variables into 
	-- scope, and rename the method binds
    bindLocalNames (map varName tyvars)		$
    rnMethodBinds clas_nm [] meth_binds		`thenM` \ (rn_meth_binds, _fvs) ->

	-- Build the InstInfo
    returnM (InstInfo { iDFunId = dfun, iBinds = VanillaInst rn_meth_binds [] }, 
	     aux_binds)

genDerivBinds clas fix_env tycon
  | className clas `elem` typeableClassNames
  = (gen_Typeable_binds tycon, emptyLHsBinds)

  | otherwise
  = case assocMaybe gen_list (getUnique clas) of
	Just gen_fn -> gen_fn fix_env tycon
	Nothing	    -> pprPanic "genDerivBinds: bad derived class" (ppr clas)
  where
    gen_list :: [(Unique, FixityEnv -> TyCon -> (LHsBinds RdrName, LHsBinds RdrName))]
    gen_list = [(eqClassKey,      no_aux_binds (ignore_fix_env gen_Eq_binds))
 	       ,(ordClassKey,     no_aux_binds (ignore_fix_env gen_Ord_binds))
 	       ,(enumClassKey,    no_aux_binds (ignore_fix_env gen_Enum_binds))
 	       ,(boundedClassKey, no_aux_binds (ignore_fix_env gen_Bounded_binds))
 	       ,(ixClassKey,      no_aux_binds (ignore_fix_env gen_Ix_binds))
	       ,(typeableClassKey,no_aux_binds (ignore_fix_env gen_Typeable_binds))
 	       ,(showClassKey,    no_aux_binds gen_Show_binds)
 	       ,(readClassKey,    no_aux_binds gen_Read_binds)
	       ,(dataClassKey,    gen_Data_binds)
 	       ]

      -- no_aux_binds is used for generators that don't 
      -- need to produce any auxiliary bindings
    no_aux_binds f fix_env tc = (f fix_env tc, emptyLHsBinds)
    ignore_fix_env f fix_env tc = f tc
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
genTaggeryBinds :: [DFunId] -> TcM (LHsBinds RdrName)
genTaggeryBinds dfuns
  = do	{ names_so_far <- foldlM do_con2tag []           tycons_of_interest
	; nm_alist_etc <- foldlM do_tag2con names_so_far tycons_of_interest
	; return (listToBag (map gen_tag_n_con_monobind nm_alist_etc)) }
  where
    all_CTs = map simpleDFunClassTyCon dfuns
    all_tycons		    = map snd all_CTs
    (tycons_of_interest, _) = removeDups compare all_tycons
    
    do_con2tag acc_Names tycon
      | isDataTyCon tycon &&
        ((we_are_deriving eqClassKey tycon
	    && any isNullarySrcDataCon (tyConDataCons tycon))
	 || (we_are_deriving ordClassKey  tycon
	    && not (isProductTyCon tycon))
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

derivCtxt :: Maybe Class -> TyCon -> SDoc
derivCtxt maybe_cls tycon
  = ptext SLIT("When deriving") <+> cls <+> ptext SLIT("for type") <+> quotes (ppr tycon)
  where
    cls = case maybe_cls of
	    Nothing -> ptext SLIT("instances")
	    Just c  -> ptext SLIT("the") <+> quotes (ppr c) <+> ptext SLIT("instance")
\end{code}

