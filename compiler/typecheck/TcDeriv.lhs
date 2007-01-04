%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Handles @deriving@ clauses on @data@ declarations.

\begin{code}
module TcDeriv ( tcDeriving ) where

#include "HsVersions.h"

import HsSyn
import DynFlags

import Generics
import TcRnMonad
import TcEnv
import TcClassDcl( tcAddDeclCtxt )	-- Small helper
import TcGenDeriv			-- Deriv stuff
import InstEnv
import Inst
import TcHsType
import TcMType
import TcSimplify

import RnBinds
import RnEnv
import HscTypes

import Class
import Type
import ErrUtils
import MkId
import DataCon
import Maybes
import RdrName
import Name
import NameSet
import TyCon
import TcType
import Var
import VarSet
import PrelNames
import SrcLoc
import Util
import ListSetOps
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
type DerivRhs  = ThetaType
type DerivSoln = DerivRhs
type DerivEqn  = (SrcSpan, InstOrigin, Name, [TyVar], Class, Type, DerivRhs)
	-- (span, orig, df, tvs, C, ty, rhs)
	--    implies a dfun declaration of the form
	--	 df :: forall tvs. rhs => C ty
	-- The Name is the name for the DFun we'll build
	-- The tyvars bind all the variables in the RHS
	-- For family indexes, the tycon is the *family* tycon
	--		(not the representation tycon)

pprDerivEqn :: DerivEqn -> SDoc
pprDerivEqn (l, _, n, tvs, c, ty, rhs)
  = parens (hsep [ppr l, ppr n, ppr tvs, ppr c, ppr ty]
	    <+> equals <+> ppr rhs)
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
            -> [LDerivDecl Name] -- All stand-alone deriving declarations
	    -> TcM ([InstInfo],		-- The generated "instance decls"
		    HsValBinds Name)	-- Extra generated top-level bindings

tcDeriving tycl_decls deriv_decls
  = recoverM (returnM ([], emptyValBindsOut)) $
    do	{   	-- Fish the "deriving"-related information out of the TcEnv
		-- and make the necessary "equations".
	; (ordinary_eqns, newtype_inst_info) <- makeDerivEqns tycl_decls deriv_decls

	; (ordinary_inst_info, deriv_binds) 
		<- extendLocalInstEnv (map iSpec newtype_inst_info)  $
		   deriveOrdinaryStuff ordinary_eqns
		-- Add the newtype-derived instances to the inst env
		-- before tacking the "ordinary" ones

	; let inst_info = newtype_inst_info ++ ordinary_inst_info

	-- If we are compiling a hs-boot file, 
	-- don't generate any derived bindings
	; is_boot <- tcIsHsBoot
	; if is_boot then
		return (inst_info, emptyValBindsOut)
	  else do
	{

	-- Generate the generic to/from functions from each type declaration
	; gen_binds <- mkGenericBinds tycl_decls

	-- Rename these extra bindings, discarding warnings about unused bindings etc
	-- Set -fglasgow exts so that we can have type signatures in patterns,
	-- which is used in the generic binds
	; rn_binds
		<- discardWarnings $ setOptM Opt_GlasgowExts $ do
			{ (rn_deriv, _dus1) <- rnTopBinds (ValBindsIn deriv_binds [])
			; (rn_gen, dus_gen) <- rnTopBinds (ValBindsIn gen_binds   [])
			; keepAliveSetTc (duDefs dus_gen)	-- Mark these guys to
								-- be kept alive
			; return (rn_deriv `plusHsValBinds` rn_gen) }


	; dflags <- getDOpts
	; ioToTcRn (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances" 
	  	   (ddump_deriving inst_info rn_binds))

	; returnM (inst_info, rn_binds)
	}}
  where
    ddump_deriving :: [InstInfo] -> HsValBinds Name -> SDoc
    ddump_deriving inst_infos extra_binds
      = vcat (map pprInstInfoDetails inst_infos) $$ ppr extra_binds

-----------------------------------------
deriveOrdinaryStuff []	-- Short cut
  = returnM ([], emptyLHsBinds)

deriveOrdinaryStuff eqns
  = do	{	-- Take the equation list and solve it, to deliver a list of
		-- solutions, a.k.a. the contexts for the instance decls
		-- required for the corresponding equations.
	  overlap_flag <- getOverlapFlag
	; inst_specs <- solveDerivEqns overlap_flag eqns

	-- Generate the InstInfo for each dfun, 
	-- plus any auxiliary bindings it needs
	; (inst_infos, aux_binds_s) <- mapAndUnzipM genInst inst_specs

	-- Generate any extra not-one-inst-decl-specific binds, 
	-- notably "con2tag" and/or "tag2con" functions.  
	; extra_binds <- genTaggeryBinds inst_infos

	-- Done
	; returnM (map fst inst_infos, 
		   unionManyBags (extra_binds : aux_binds_s))
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

Note [Newtype deriving superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'tys' here come from the partial application in the deriving
clause. The last arg is the new instance type.

We must pass the superclasses; the newtype might be an instance
of them in a different way than the representation type
E.g.		newtype Foo a = Foo a deriving( Show, Num, Eq )
Then the Show instance is not done via isomorphism; it shows
	Foo 3 as "Foo 3"
The Num instance is derived via isomorphism, but the Show superclass
dictionary must the Show instance for Foo, *not* the Show dictionary
gotten from the Num dictionary. So we must build a whole new dictionary
not just use the Num one.  The instance we want is something like:
     instance (Num a, Show (Foo a), Eq (Foo a)) => Num (Foo a) where
     	(+) = ((+)@a)
     	...etc...
There may be a coercion needed which we get from the tycon for the newtype
when the dict is constructed in TcInstDcls.tcInstDecl2


\begin{code}
makeDerivEqns :: [LTyClDecl Name] 
	      -> [LDerivDecl Name] 
	      -> TcM ([DerivEqn],	-- Ordinary derivings
		      [InstInfo])	-- Special newtype derivings

makeDerivEqns tycl_decls deriv_decls
  = do	{ eqns1 <- mapM deriveTyData $
		   [ (p,d) | d@(L _ (TyData {tcdDerivs = Just preds})) <- tycl_decls
			   , p <- preds ]
	; eqns2 <- mapM deriveStandalone deriv_decls
	; return ([eqn  | (Just eqn, _)  <- eqns1 ++ eqns2],
		  [inst | (_, Just inst) <- eqns1 ++ eqns2]) }

------------------------------------------------------------------
deriveStandalone :: LDerivDecl Name -> TcM (Maybe DerivEqn, Maybe InstInfo)
-- Standalone deriving declarations
-- 	e.g.   derive instance Show T
-- Rather like tcLocalInstDecl
deriveStandalone (L loc (DerivDecl deriv_ty))
  = setSrcSpan loc                   $
    addErrCtxt (standaloneCtxt deriv_ty)  $
    do	{ (tvs, theta, tau) <- tcHsInstHead deriv_ty
	; (cls, inst_tys) <- checkValidInstHead tau
	; let cls_tys = take (length inst_tys - 1) inst_tys
	      inst_ty = last inst_tys

	; mkEqnHelp StandAloneDerivOrigin tvs cls cls_tys inst_ty }

------------------------------------------------------------------
deriveTyData :: (LHsType Name, LTyClDecl Name) -> TcM (Maybe DerivEqn, Maybe InstInfo)
deriveTyData (deriv_pred, L loc decl@(TyData { tcdLName = L _ tycon_name, 
					       tcdTyVars = tv_names, 
				    	       tcdTyPats = ty_pats }))
  = setSrcSpan loc                   $
    tcAddDeclCtxt decl		     $
    do	{ let hs_ty_args = ty_pats `orElse` map (nlHsTyVar . hsLTyVarName) tv_names
	      hs_app     = nlHsTyConApp tycon_name hs_ty_args
		-- We get kinding info for the tyvars by typechecking (T a b)
		-- Hence forming a tycon application and then dis-assembling it
	; (tvs, tc_app) <- tcHsQuantifiedType tv_names hs_app
	; tcExtendTyVarEnv tvs $	-- Deriving preds may (now) mention
					-- the type variables for the type constructor
    do	{ (deriv_tvs, cls, cls_tys) <- tcHsDeriv deriv_pred
		-- The "deriv_pred" is a LHsType to take account of the fact that for
		-- newtype deriving we allow deriving (forall a. C [a]).
	; mkEqnHelp DerivOrigin (tvs++deriv_tvs) cls cls_tys tc_app } }

------------------------------------------------------------------
mkEqnHelp orig tvs cls cls_tys tc_app
  | Just (tycon, tc_args) <- tcSplitTyConApp_maybe tc_app
  = do	{ 	-- Make tc_app saturated, because that's what the
		-- mkDataTypeEqn things expect
		-- It might not be saturated in the standalone deriving case
		-- 	derive instance Monad (T a)
	  let extra_tvs = dropList tc_args (tyConTyVars tycon)
	      full_tc_args = tc_args ++ mkTyVarTys extra_tvs
	      full_tvs = tvs ++ extra_tvs
		
	; (rep_tc, rep_tc_args) <- tcLookupFamInst tycon full_tc_args

	; gla_exts <- doptM Opt_GlasgowExts
	; overlap_flag <- getOverlapFlag
	; if isDataTyCon tycon then
		mkDataTypeEqn orig gla_exts full_tvs cls cls_tys 
			      tycon full_tc_args rep_tc rep_tc_args
	  else
		mkNewTypeEqn  orig gla_exts overlap_flag full_tvs cls cls_tys 
			      tycon full_tc_args rep_tc rep_tc_args }
  | otherwise
  = baleOut (derivingThingErr cls cls_tys tc_app
		(ptext SLIT("Last argument of the instance must be a type application")))

baleOut err = addErrTc err >> returnM (Nothing, Nothing) 
\end{code}


%************************************************************************
%*									*
		Deriving data types
%*									*
%************************************************************************

\begin{code}
mkDataTypeEqn orig gla_exts tvs cls cls_tys tycon tc_args rep_tc rep_tc_args
  | Just err <- checkSideConditions gla_exts cls cls_tys rep_tc
  = baleOut (derivingThingErr cls cls_tys (mkTyConApp tycon tc_args) err)

  | otherwise 
  = ASSERT( null cls_tys )
    do	{ loc <- getSrcSpanM
	; eqn <- mk_data_eqn loc orig tvs cls tycon tc_args rep_tc rep_tc_args
	; return (Just eqn, Nothing) }

mk_data_eqn :: SrcSpan -> InstOrigin -> [TyVar] -> Class 
	    -> TyCon -> [TcType] -> TyCon -> [TcType] -> TcM DerivEqn
mk_data_eqn loc orig tvs cls tycon tc_args rep_tc rep_tc_args
  | cls `hasKey` typeableClassKey
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
  	; return (loc, orig, dfun_name, [], real_clas, mkTyConApp tycon [], []) }

  | otherwise
  = do	{ dfun_name <- new_dfun_name cls tycon
	; let ordinary_constraints
	        = [ mkClassPred cls [arg_ty] 
	          | data_con <- tyConDataCons rep_tc,
	            arg_ty   <- dataConInstOrigArgTys data_con rep_tc_args,
	            not (isUnLiftedType arg_ty) ] -- No constraints for unlifted types?

	      tiresome_subst = zipTopTvSubst (tyConTyVars rep_tc) rep_tc_args
	      stupid_constraints = substTheta tiresome_subst (tyConStupidTheta rep_tc)
		 -- see note [Data decl contexts] above

  	; return (loc, orig, dfun_name, tvs, cls, mkTyConApp tycon tc_args, 
		  stupid_constraints ++ ordinary_constraints)
	}

------------------------------------------------------------------
-- Check side conditions that dis-allow derivability for particular classes
-- This is *apart* from the newtype-deriving mechanism
--
-- Here we get the representation tycon in case of family instances as it has
-- the data constructors - but we need to be careful to fall back to the
-- family tycon (with indexes) in error messages.

checkSideConditions :: Bool -> Class -> [TcType] -> TyCon -> Maybe SDoc
checkSideConditions gla_exts cls cls_tys rep_tc
  | notNull cls_tys	
  = Just ty_args_why	-- e.g. deriving( Foo s )
  | otherwise
  = case [cond | (key,cond) <- sideConditions, key == getUnique cls] of
	[]     -> Just (non_std_why cls)
	[cond] -> cond (gla_exts, rep_tc)
	other  -> pprPanic "checkSideConditions" (ppr cls)
  where
    ty_args_why	= quotes (ppr (mkClassPred cls cls_tys)) <+> ptext SLIT("is not a class")

non_std_why cls = quotes (ppr cls) <+> ptext SLIT("is not a derivable class")

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
cond_std (gla_exts, rep_tc)
  | any (not . isVanillaDataCon) data_cons = Just existential_why     
  | null data_cons		    	   = Just no_cons_why
  | otherwise      			   = Nothing
  where
    data_cons       = tyConDataCons rep_tc
    no_cons_why	    = quotes (pprSourceTyCon rep_tc) <+> 
		      ptext SLIT("has no data constructors")
    existential_why = quotes (pprSourceTyCon rep_tc) <+> 
		      ptext SLIT("has non-Haskell-98 constructor(s)")
  
cond_isEnumeration :: Condition
cond_isEnumeration (gla_exts, rep_tc)
  | isEnumerationTyCon rep_tc = Nothing
  | otherwise		      = Just why
  where
    why = quotes (pprSourceTyCon rep_tc) <+> 
	  ptext SLIT("has non-nullary constructors")

cond_isProduct :: Condition
cond_isProduct (gla_exts, rep_tc)
  | isProductTyCon rep_tc = Nothing
  | otherwise	          = Just why
  where
    why = (pprSourceTyCon rep_tc) <+> 
	  ptext SLIT("has more than one constructor")

cond_typeableOK :: Condition
-- OK for Typeable class
-- Currently: (a) args all of kind *
--	      (b) 7 or fewer args
cond_typeableOK (gla_exts, rep_tc)
  | tyConArity rep_tc > 7	= Just too_many
  | not (all (isSubArgTypeKind . tyVarKind) (tyConTyVars rep_tc)) 
                                = Just bad_kind
  | isFamInstTyCon rep_tc	= Just fam_inst  -- no Typable for family insts
  | otherwise	  		= Nothing
  where
    too_many = quotes (pprSourceTyCon rep_tc) <+> 
	       ptext SLIT("has too many arguments")
    bad_kind = quotes (pprSourceTyCon rep_tc) <+> 
	       ptext SLIT("has arguments of kind other than `*'")
    fam_inst = quotes (pprSourceTyCon rep_tc) <+> 
	       ptext SLIT("is a type family")

cond_glaExts :: Condition
cond_glaExts (gla_exts, _rep_tc) | gla_exts  = Nothing
			         | otherwise = Just why
  where
    why  = ptext SLIT("You need -fglasgow-exts to derive an instance for this class")

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
\end{code}


%************************************************************************
%*									*
		Deriving newtypes
%*									*
%************************************************************************

\begin{code}
mkNewTypeEqn orig gla_exts overlap_flag tvs cls cls_tys
	     tycon tc_args 
	     rep_tycon rep_tc_args
  | can_derive_via_isomorphism && (gla_exts || std_class_via_iso cls)
  =	do { traceTc (text "newtype deriving:" <+> ppr tycon <+> ppr rep_tys)
	   ; 	-- Go ahead and use the isomorphism
	     dfun_name <- new_dfun_name cls tycon
	   ; return (Nothing, Just (InstInfo { iSpec  = mk_inst_spec dfun_name,
					       iBinds = NewTypeDerived ntd_info })) }
  | std_class gla_exts cls
  = mkDataTypeEqn orig gla_exts tvs cls cls_tys tycon tc_args rep_tycon rep_tc_args	-- Go via bale-out route

  	-- Otherwise its a non-standard instance
  | gla_exts  = baleOut cant_derive_err	-- Too hard
  | otherwise = baleOut non_std_err	-- Just complain about being a non-std instance
  where
	-- Here is the plan for newtype derivings.  We see
	--	  newtype T a1...an = MkT (t ak+1...an) deriving (.., C s1 .. sm, ...)
	-- where t is a type,
	-- 	 ak+1...an is a suffix of a1..an, and are all tyars
	--	 ak+1...an do not occur free in t, nor in the s1..sm
	-- 	 (C s1 ... sm) is a  *partial applications* of class C 
	--			with the last parameter missing
	--	 (T a1 .. ak) matches the kind of C's last argument
	--		(and hence so does t)
	--
	-- We generate the instance
	--	 instance forall ({a1..ak} u fvs(s1..sm)).
	--		  C s1 .. sm t => C s1 .. sm (T a1...ak)
	-- where T a1...ap is the partial application of 
	-- 	 the LHS of the correct kind and p >= k
	--
	--	NB: the variables below are:
	--		tc_tvs = [a1, ..., an]
	--		tyvars_to_keep = [a1, ..., ak]
	--		rep_ty = t ak .. an
	--		deriv_tvs = fvs(s1..sm) \ tc_tvs
	--		tys = [s1, ..., sm]
	--		rep_fn' = t
	--
	-- Running example: newtype T s a = MkT (ST s a) deriving( Monad )
	-- We generate the instance
	--	instance Monad (ST s) => Monad (T s) where 

	cls_tyvars = classTyVars cls
	kind = tyVarKind (last cls_tyvars)
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
	rep_ty		      = newTyConInstRhs rep_tycon rep_tc_args
	(rep_fn, rep_ty_args) = tcSplitAppTys rep_ty

	n_tyargs_to_keep = tyConArity tycon - n_args_to_drop
	dropped_tc_args = drop n_tyargs_to_keep tc_args
	dropped_tvs     = tyVarsOfTypes dropped_tc_args

	n_args_to_keep = length rep_ty_args - n_args_to_drop
	args_to_drop   = drop n_args_to_keep rep_ty_args
	args_to_keep   = take n_args_to_keep rep_ty_args

	rep_fn'  = mkAppTys rep_fn args_to_keep
	rep_tys  = cls_tys ++ [rep_fn']
	rep_pred = mkClassPred cls rep_tys
		-- rep_pred is the representation dictionary, from where
		-- we are gong to get all the methods for the newtype
		-- dictionary 

	tc_app = mkTyConApp tycon (take n_tyargs_to_keep tc_args)

    -- Next we figure out what superclass dictionaries to use
    -- See Note [Newtype deriving superclasses] above

	inst_tys = cls_tys ++ [tc_app]
	sc_theta = substTheta (zipOpenTvSubst cls_tyvars inst_tys)
			      (classSCTheta cls)

		-- If there are no tyvars, there's no need
		-- to abstract over the dictionaries we need
		-- Example: 	newtype T = MkT Int deriving( C )
		-- We get the derived instance
		--		instance C T
		-- rather than
		--		instance C Int => C T
	dict_tvs = filterOut (`elemVarSet` dropped_tvs) tvs
	all_preds = rep_pred : sc_theta		-- NB: rep_pred comes first
	(dict_args, ntd_info) | null dict_tvs = ([], Just all_preds)
			      | otherwise     = (all_preds, Nothing)

		-- Finally! Here's where we build the dictionary Id
	mk_inst_spec dfun_name = mkLocalInstance dfun overlap_flag
	  where
	    dfun = mkDictFunId dfun_name dict_tvs dict_args cls inst_tys

	-------------------------------------------------------------------
	--  Figuring out whether we can only do this newtype-deriving thing

	right_arity = length cls_tys + 1 == classArity cls

		-- Never derive Read,Show,Typeable,Data this way 
	non_iso_classes = [readClassKey, showClassKey, typeableClassKey, dataClassKey]
	can_derive_via_isomorphism
	   =  not (getUnique cls `elem` non_iso_classes)
	   && right_arity 			-- Well kinded;
						-- eg not: newtype T ... deriving( ST )
						--	because ST needs *2* type params
	   && n_tyargs_to_keep >= 0		-- Type constructor has right kind:
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
	eta_ok = (args_to_drop `tcEqTypes` dropped_tc_args)
		-- (a) the dropped-off args are identical in the source and rep type
		--	  newtype T a b = MkT (S [a] b) deriving( Monad )
		--     Here the 'b' must be the same in the rep type (S [a] b)

	      && (tyVarsOfType rep_fn' `disjointVarSet` dropped_tvs)
		-- (b) the remaining type args do not mention any of the dropped
		--     type variables 

	      && (tyVarsOfTypes cls_tys `disjointVarSet` dropped_tvs)
		-- (c) the type class args do not mention any of the dropped type
		--     variables 

	      && all isTyVarTy dropped_tc_args
		-- (d) in case of newtype family instances, the eta-dropped
		--      arguments must be type variables (not more complex indexes)

	cant_derive_err = derivingThingErr cls cls_tys tc_app
				(vcat [ptext SLIT("even with cunning newtype deriving:"),
					if isRecursiveTyCon tycon then
					  ptext SLIT("the newtype is recursive")
					else empty,
					if not right_arity then 
					  quotes (ppr (mkClassPred cls cls_tys)) <+> ptext SLIT("does not have arity 1")
					else empty,
					if not (n_tyargs_to_keep >= 0) then 
					  ptext SLIT("the type constructor has wrong kind")
					else if not (n_args_to_keep >= 0) then
					  ptext SLIT("the representation type has wrong kind")
					else if not eta_ok then 
					  ptext SLIT("the eta-reduction property does not hold")
					else empty
				      ])

	non_std_err = derivingThingErr cls cls_tys tc_app
				(vcat [non_std_why cls,
				       ptext SLIT("Try -fglasgow-exts for GHC's newtype-deriving extension")])
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
solveDerivEqns :: OverlapFlag
	       -> [DerivEqn]
	       -> TcM [Instance]-- Solns in same order as eqns.
				-- This bunch is Absolutely minimal...

solveDerivEqns overlap_flag orig_eqns
  = do	{ traceTc (text "solveDerivEqns" <+> vcat (map pprDerivEqn orig_eqns))
	; iterateDeriv 1 initial_solutions }
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
    iterateDeriv :: Int -> [DerivSoln] -> TcM [Instance]
    iterateDeriv n current_solns
      | n > 20 	-- Looks as if we are in an infinite loop
		-- This can happen if we have -fallow-undecidable-instances
		-- (See TcSimplify.tcSimplifyDeriv.)
      = pprPanic "solveDerivEqns: probable loop" 
		 (vcat (map pprDerivEqn orig_eqns) $$ ppr current_solns)
      | otherwise
      =	let 
	    inst_specs = zipWithEqual "add_solns" mk_inst_spec 
				      orig_eqns current_solns
        in
        checkNoErrs (
		  -- Extend the inst info from the explicit instance decls
		  -- with the current set of solutions, and simplify each RHS
	    extendLocalInstEnv inst_specs $
	    mappM gen_soln orig_eqns
	)				`thenM` \ new_solns ->
	if (current_solns == new_solns) then
	    returnM inst_specs
	else
	    iterateDeriv (n+1) new_solns

    ------------------------------------------------------------------
    gen_soln :: DerivEqn -> TcM [PredType]
    gen_soln (loc, orig, _, tyvars, clas, inst_ty, deriv_rhs)
      = setSrcSpan loc	$
	do { theta <- tcSimplifyDeriv orig tyvars deriv_rhs
	   ; addErrCtxt (derivInstCtxt theta clas [inst_ty]) $ 
	do { checkNoErrs (checkValidInstance tyvars theta clas [inst_ty])
	 	-- See Note [Deriving context]
		-- If this fails, don't continue

		  -- Check for a bizarre corner case, when the derived instance decl should
		  -- have form 	instance C a b => D (T a) where ...
		  -- Note that 'b' isn't a parameter of T.  This gives rise to all sorts
		  -- of problems; in particular, it's hard to compare solutions for
		  -- equality when finding the fixpoint.  So I just rule it out for now.
 	   ; let tv_set = mkVarSet tyvars
	         weird_preds = [pred | pred <- theta, not (tyVarsOfPred pred `subVarSet` tv_set)]  
	   ; mapM_ (addErrTc . badDerivedPred) weird_preds	

		-- Claim: the result instance declaration is guaranteed valid
		-- Hence no need to call:
		--   checkValidInstance tyvars theta clas inst_tys
	   ; return (sortLe (<=) theta) } }	-- Canonicalise before returning the solution

    ------------------------------------------------------------------
    mk_inst_spec :: DerivEqn -> DerivSoln -> Instance
    mk_inst_spec (loc, orig, dfun_name, tyvars, clas, inst_ty, _) theta
	= mkLocalInstance dfun overlap_flag
	where
	  dfun = mkDictFunId dfun_name tyvars theta clas [inst_ty]

extendLocalInstEnv :: [Instance] -> TcM a -> TcM a
-- Add new locally-defined instances; don't bother to check
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
-- Generate the InstInfo for the required instance paired with the
--   *representation* tycon for that instance,
-- plus any auxiliary bindings required
--
-- Representation tycons differ from the tycon in the instance signature in
-- case of instances for indexed families.
--
genInst :: Instance -> TcM ((InstInfo, TyCon), LHsBinds RdrName)
genInst spec
  = do	{ fix_env <- getFixityEnv
	; let
	    (tyvars,_,clas,[ty])    = instanceHead spec
	    clas_nm		    = className clas
	    (visible_tycon, tyArgs) = tcSplitTyConApp ty 

          -- In case of a family instance, we need to use the representation
          -- tycon (after all, it has the data constructors)
        ; (tycon, _) <- tcLookupFamInst visible_tycon tyArgs
	; let (meth_binds, aux_binds) = genDerivBinds clas fix_env tycon

	-- Bring the right type variables into 
	-- scope, and rename the method binds
	-- It's a bit yukky that we return *renamed* InstInfo, but
	-- *non-renamed* auxiliary bindings
	; (rn_meth_binds, _fvs) <- discardWarnings $ 
				   bindLocalNames (map Var.varName tyvars) $
			 	   rnMethodBinds clas_nm (\n -> []) [] meth_binds

	-- Build the InstInfo
	; return ((InstInfo { iSpec = spec, 
		  	      iBinds = VanillaInst rn_meth_binds [] }, tycon),
		  aux_binds)
        }

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
genTaggeryBinds :: [(InstInfo, TyCon)] -> TcM (LHsBinds RdrName)
genTaggeryBinds infos
  = do	{ names_so_far <- foldlM do_con2tag []           tycons_of_interest
	; nm_alist_etc <- foldlM do_tag2con names_so_far tycons_of_interest
	; return (listToBag (map gen_tag_n_con_monobind nm_alist_etc)) }
  where
    all_CTs                 = [ (fst (simpleInstInfoClsTy info), tc) 
			      | (info, tc) <- infos]
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
derivingThingErr clas tys ty why
  = sep [hsep [ptext SLIT("Can't make a derived instance of"), 
	       quotes (ppr pred)],
	 nest 2 (parens why)]
  where
    pred = mkClassPred clas (tys ++ [ty])

standaloneCtxt :: LHsType Name -> SDoc
standaloneCtxt ty = ptext SLIT("In the stand-alone deriving instance for") <+> quotes (ppr ty)

derivInstCtxt theta clas inst_tys
  = hang (ptext SLIT("In the derived instance:"))
	 2 (pprThetaArrow theta <+> pprClassPred clas inst_tys)
-- Used for the ...Thetas variants; all top level

badDerivedPred pred
  = vcat [ptext SLIT("Can't derive instances where the instance context mentions"),
	  ptext SLIT("type variables that are not data type parameters"),
	  nest 2 (ptext SLIT("Offending constraint:") <+> ppr pred)]
\end{code}

