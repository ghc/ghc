%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
module TcMonoType ( tcHsSigType, tcHsType, tcIfaceType, tcHsTheta, 
		    UserTypeCtxt(..),

			-- Kind checking
		    kcHsTyVar, kcHsTyVars, mkTyClTyVars,
		    kcHsType, kcHsSigType, kcHsSigTypes, 
		    kcHsLiftedSigType, kcHsContext,
		    tcScopedTyVars, tcHsTyVars, mkImmutTyVars,

		    TcSigInfo(..), tcTySig, mkTcSig, maybeSig,
		    checkSigTyVars, sigCtxt, sigPatCtxt
	          ) where

#include "HsVersions.h"

import HsSyn		( HsType(..), HsTyVarBndr(..),
                          Sig(..), HsPred(..), pprParendHsType, HsTupCon(..), hsTyVarNames )
import RnHsSyn		( RenamedHsType, RenamedHsPred, RenamedContext, RenamedSig )
import TcHsSyn		( TcId )

import TcMonad
import TcEnv		( tcExtendTyVarEnv, tcLookup, tcLookupGlobal,
			  tcGetGlobalTyVars, tcEnvTcIds, tcEnvTyVars,
		 	  TyThing(..), TcTyThing(..), tcExtendKindEnv
			)
import TcMType		( newKindVar, tcInstSigVars, 
			  zonkKindEnv, zonkTcType, zonkTcTyVars, zonkTcTyVar,
			  unifyKind, unifyOpenTypeKind,
			  checkValidType, UserTypeCtxt(..), pprUserTypeCtxt
			)
import TcType		( Type, Kind, SourceType(..), ThetaType,
			  mkTyVarTy, mkTyVarTys, mkFunTy, mkSynTy,
			  tcSplitForAllTys, tcSplitRhoTy,
		 	  hoistForAllTys, allDistinctTyVars,
                          zipFunTys, 
			  mkSigmaTy, mkPredTy, mkTyConApp,
			  mkAppTys, mkRhoTy,
			  liftedTypeKind, unliftedTypeKind, mkArrowKind,
			  mkArrowKinds, tcGetTyVar_maybe, tcGetTyVar, tcSplitFunTy_maybe,
		  	  tidyOpenType, tidyOpenTypes, tidyOpenTyVar, tidyOpenTyVars,
			  tyVarsOfType, mkForAllTys
			)
import Inst		( Inst, InstOrigin(..), newMethodWithGivenTy, instToId )
import PprType		( pprType )
import Subst		( mkTopTyVarSubst, substTy )
import CoreFVs		( idFreeTyVars )
import Id		( mkLocalId, idName, idType )
import Var		( Id, Var, TyVar, mkTyVar, tyVarKind )
import VarEnv
import VarSet
import ErrUtils		( Message )
import TyCon		( TyCon, isSynTyCon, tyConArity, tyConKind )
import Class		( classTyCon )
import Name		( Name )
import TysWiredIn	( mkListTy, mkTupleTy, genUnitTyCon )
import BasicTypes	( Boxity(..) )
import SrcLoc		( SrcLoc )
import Util		( isSingleton )
import Outputable

\end{code}


%************************************************************************
%*									*
\subsection{Checking types}
%*									*
%************************************************************************

Generally speaking we now type-check types in three phases

	1.  Kind check the HsType [kcHsType]
	2.  Convert from HsType to Type, and hoist the foralls [tcHsType]
	3.  Check the validity of the resulting type [checkValidType]

Often these steps are done one after the othe (tcHsSigType).
But in mutually recursive groups of type and class decls we do
	1 kind-check the whole group
	2 build TyCons/Classes in a knot-tied wa
	3 check the validity of types in the now-unknotted TyCons/Classes

\begin{code}
tcHsSigType :: UserTypeCtxt -> RenamedHsType -> TcM Type
  -- Do kind checking, and hoist for-alls to the top
tcHsSigType ctxt ty = tcAddErrCtxt (checkTypeCtxt ctxt ty) (
			kcTypeType ty		`thenTc_`
		        tcHsType ty
		      )				`thenTc` \ ty' ->
		      checkValidType ctxt ty'	`thenTc_`
		      returnTc ty'

checkTypeCtxt ctxt ty
  = vcat [ptext SLIT("In the type:") <+> ppr ty,
	  ptext SLIT("While checking") <+> pprUserTypeCtxt ctxt ]

tcHsType    :: RenamedHsType -> TcM Type
  -- Don't do kind checking, nor validity checking, 
  -- 	but do hoist for-alls to the top
  -- This is used in type and class decls, where kinding is
  -- done in advance, and validity checking is done later
  -- [Validity checking done later because of knot-tying issues.]
tcHsType ty = tc_type ty  `thenTc` \ ty' ->  
	      returnTc (hoistForAllTys ty')

tcHsTheta :: RenamedContext -> TcM ThetaType
-- Used when we are expecting a ClassContext (i.e. no implicit params)
-- Does not do validity checking, like tcHsType
tcHsTheta hs_theta = mapTc tc_pred hs_theta

-- In interface files the type is already kinded,
-- and we definitely don't want to hoist for-alls.
-- Otherwise we'll change
--  	dmfail :: forall m:(*->*) Monad m => forall a:* => String -> m a
-- into 
-- 	dmfail :: forall m:(*->*) a:* Monad m => String -> m a
-- which definitely isn't right!
tcIfaceType ty = tc_type ty
\end{code}


%************************************************************************
%*									*
\subsection{Kind checking}
%*									*
%************************************************************************

Kind checking
~~~~~~~~~~~~~
When we come across the binding site for some type variables, we
proceed in two stages

1. Figure out what kind each tyvar has

2. Create suitably-kinded tyvars, 
   extend the envt, 
   and typecheck the body

To do step 1, we proceed thus:

1a. Bind each type variable to a kind variable
1b. Apply the kind checker
1c. Zonk the resulting kinds

The kind checker is passed to tcHsTyVars as an argument.  

For example, when we find
	(forall a m. m a -> m a)
we bind a,m to kind varibles and kind-check (m a -> m a).  This
makes a get kind *, and m get kind *->*.  Now we typecheck (m a -> m a)
in an environment that binds a and m suitably.

The kind checker passed to tcHsTyVars needs to look at enough to
establish the kind of the tyvar:
  * For a group of type and class decls, it's just the group, not
	the rest of the program
  * For a tyvar bound in a pattern type signature, its the types
	mentioned in the other type signatures in that bunch of patterns
  * For a tyvar bound in a RULE, it's the type signatures on other
	universally quantified variables in the rule

Note that this may occasionally give surprising results.  For example:

	data T a b = MkT (a b)

Here we deduce			a::*->*, b::*.
But equally valid would be
				a::(*->*)-> *, b::*->*

\begin{code}
-- tcHsTyVars is used for type variables in type signatures
--	e.g. forall a. a->a
-- They are immutable, because they scope only over the signature
-- They may or may not be explicitly-kinded
tcHsTyVars :: [HsTyVarBndr Name] 
	   -> TcM a				-- The kind checker
	   -> ([TyVar] -> TcM b)
	   -> TcM b

tcHsTyVars [] kind_check thing_inside = thing_inside []
	-- A useful short cut for a common case!
  
tcHsTyVars tv_names kind_check thing_inside
  = kcHsTyVars tv_names 				`thenNF_Tc` \ tv_names_w_kinds ->
    tcExtendKindEnv tv_names_w_kinds kind_check		`thenTc_`
    zonkKindEnv tv_names_w_kinds			`thenNF_Tc` \ tvs_w_kinds ->
    let
	tyvars = mkImmutTyVars tvs_w_kinds
    in
    tcExtendTyVarEnv tyvars (thing_inside tyvars)

-- tcScopedTyVars is used for scoped type variables
--	e.g.  \ (x::a) (y::a) -> x+y
-- They never have explicit kinds (because this is source-code only)
-- They are mutable (because they can get bound to a more specific type)
tcScopedTyVars :: [Name] 
	       -> TcM a				-- The kind checker
	       -> TcM b
	       -> TcM b
tcScopedTyVars [] kind_check thing_inside = thing_inside

tcScopedTyVars tv_names kind_check thing_inside
  = mapNF_Tc newNamedKindVar tv_names		`thenTc` \ kind_env ->
    tcExtendKindEnv kind_env kind_check		`thenTc_`
    zonkKindEnv kind_env			`thenNF_Tc` \ tvs_w_kinds ->
    listTc [tcNewMutTyVar name kind | (name, kind) <- tvs_w_kinds]	`thenNF_Tc` \ tyvars ->
    tcExtendTyVarEnv tyvars thing_inside
\end{code}
    

\begin{code}
kcHsTyVar  :: HsTyVarBndr name   -> NF_TcM (name, TcKind)
kcHsTyVars :: [HsTyVarBndr name] -> NF_TcM [(name, TcKind)]

kcHsTyVar (UserTyVar name)       = newNamedKindVar name
kcHsTyVar (IfaceTyVar name kind) = returnNF_Tc (name, kind)

kcHsTyVars tvs = mapNF_Tc kcHsTyVar tvs

newNamedKindVar name = newKindVar	`thenNF_Tc` \ kind ->
		       returnNF_Tc (name, kind)

---------------------------
kcLiftedType :: RenamedHsType -> TcM ()
	-- The type ty must be a *lifted* *type*
kcLiftedType ty
  = kcHsType ty				`thenTc` \ kind ->
    tcAddErrCtxt (typeKindCtxt ty)	$
    unifyKind liftedTypeKind kind
    
---------------------------
kcTypeType :: RenamedHsType -> TcM ()
	-- The type ty must be a *type*, but it can be lifted or unlifted.
kcTypeType ty
  = kcHsType ty				`thenTc` \ kind ->
    tcAddErrCtxt (typeKindCtxt ty)	$
    unifyOpenTypeKind kind

---------------------------
kcHsSigType, kcHsLiftedSigType :: RenamedHsType -> TcM ()
	-- Used for type signatures
kcHsSigType  	  = kcTypeType
kcHsSigTypes tys  = mapTc_ kcHsSigType tys
kcHsLiftedSigType = kcLiftedType

---------------------------
kcHsType :: RenamedHsType -> TcM TcKind
kcHsType (HsTyVar name)	      = kcTyVar name

kcHsType (HsListTy ty)
  = kcLiftedType ty		`thenTc` \ tau_ty ->
    returnTc liftedTypeKind

kcHsType (HsTupleTy (HsTupCon _ boxity _) tys)
  = mapTc kcTypeType tys	`thenTc_`
    returnTc (case boxity of
		  Boxed   -> liftedTypeKind
		  Unboxed -> unliftedTypeKind)

kcHsType (HsFunTy ty1 ty2)
  = kcTypeType ty1	`thenTc_`
    kcTypeType ty2	`thenTc_`
    returnTc liftedTypeKind

kcHsType (HsNumTy _)		-- The unit type for generics
  = returnTc liftedTypeKind

kcHsType ty@(HsOpTy ty1 op ty2)
  = kcTyVar op				`thenTc` \ op_kind ->
    kcHsType ty1			`thenTc` \ ty1_kind ->
    kcHsType ty2			`thenTc` \ ty2_kind ->
    tcAddErrCtxt (appKindCtxt (ppr ty))	$
    kcAppKind op_kind  ty1_kind		`thenTc` \ op_kind' ->
    kcAppKind op_kind' ty2_kind
   
kcHsType (HsPredTy pred)
  = kcHsPred pred		`thenTc_`
    returnTc liftedTypeKind

kcHsType ty@(HsAppTy ty1 ty2)
  = kcHsType ty1			`thenTc` \ tc_kind ->
    kcHsType ty2			`thenTc` \ arg_kind ->
    tcAddErrCtxt (appKindCtxt (ppr ty))	$
    kcAppKind tc_kind arg_kind

kcHsType (HsForAllTy (Just tv_names) context ty)
  = kcHsTyVars tv_names		`thenNF_Tc` \ kind_env ->
    tcExtendKindEnv kind_env	$
    kcHsContext context		`thenTc_`
    kcHsType ty			`thenTc_`
    returnTc liftedTypeKind

---------------------------
kcAppKind fun_kind arg_kind
  = case tcSplitFunTy_maybe fun_kind of 
	Just (arg_kind', res_kind)
		-> unifyKind arg_kind arg_kind'	`thenTc_`
		   returnTc res_kind

	Nothing -> newKindVar 						`thenNF_Tc` \ res_kind ->
		   unifyKind fun_kind (mkArrowKind arg_kind res_kind)	`thenTc_`
		   returnTc res_kind


---------------------------
kcHsContext ctxt = mapTc_ kcHsPred ctxt

kcHsPred :: RenamedHsPred -> TcM ()
kcHsPred pred@(HsIParam name ty)
  = tcAddErrCtxt (appKindCtxt (ppr pred))	$
    kcLiftedType ty

kcHsPred pred@(HsClassP cls tys)
  = tcAddErrCtxt (appKindCtxt (ppr pred))	$
    kcClass cls					`thenTc` \ kind ->
    mapTc kcHsType tys				`thenTc` \ arg_kinds ->
    unifyKind kind (mkArrowKinds arg_kinds liftedTypeKind)

 ---------------------------
kcTyVar name	-- Could be a tyvar or a tycon
  = tcLookup name	`thenTc` \ thing ->
    case thing of 
	AThing kind 	    -> returnTc kind
	ATyVar tv	    -> returnTc (tyVarKind tv)
	AGlobal (ATyCon tc) -> returnTc (tyConKind tc) 
	other		    -> failWithTc (wrongThingErr "type" thing name)

kcClass cls	-- Must be a class
  = tcLookup cls 				`thenNF_Tc` \ thing -> 
    case thing of
	AThing kind	      -> returnTc kind
	AGlobal (AClass cls)  -> returnTc (tyConKind (classTyCon cls))
	other		      -> failWithTc (wrongThingErr "class" thing cls)
\end{code}

%************************************************************************
%*									*
\subsection{tc_type}
%*									*
%************************************************************************

tc_type, the main work horse
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	-------------------
	*** BIG WARNING ***
	-------------------

tc_type is used to typecheck the types in the RHS of data
constructors.  In the case of recursive data types, that means that
the type constructors themselves are (partly) black holes.  e.g.

	data T a = MkT a [T a]

While typechecking the [T a] on the RHS, T itself is not yet fully
defined.  That in turn places restrictions on what you can check in
tcHsType; if you poke on too much you get a black hole.  I keep
forgetting this, hence this warning!

So tc_type does no validity-checking.  Instead that's all done
by TcMType.checkValidType

	--------------------------
	*** END OF BIG WARNING ***
	--------------------------


\begin{code}
tc_type :: RenamedHsType -> TcM Type

tc_type ty@(HsTyVar name)
  = tc_app ty []

tc_type (HsListTy ty)
  = tc_type ty	`thenTc` \ tau_ty ->
    returnTc (mkListTy tau_ty)

tc_type (HsTupleTy (HsTupCon _ boxity arity) tys)
  = ASSERT( arity == length tys )
    tc_types tys	`thenTc` \ tau_tys ->
    returnTc (mkTupleTy boxity arity tau_tys)

tc_type (HsFunTy ty1 ty2)
  = tc_type ty1			`thenTc` \ tau_ty1 ->
    tc_type ty2			`thenTc` \ tau_ty2 ->
    returnTc (mkFunTy tau_ty1 tau_ty2)

tc_type (HsNumTy n)
  = ASSERT(n== 1)
    returnTc (mkTyConApp genUnitTyCon [])

tc_type (HsOpTy ty1 op ty2)
  = tc_type ty1 `thenTc` \ tau_ty1 ->
    tc_type ty2 `thenTc` \ tau_ty2 ->
    tc_fun_type op [tau_ty1,tau_ty2]

tc_type (HsAppTy ty1 ty2) = tc_app ty1 [ty2]

tc_type (HsPredTy pred)
  = tc_pred pred	`thenTc` \ pred' ->
    returnTc (mkPredTy pred')

tc_type full_ty@(HsForAllTy (Just tv_names) ctxt ty)
  = let
	kind_check = kcHsContext ctxt `thenTc_` kcHsType ty
    in
    tcHsTyVars tv_names kind_check	$ \ tyvars ->
    mapTc tc_pred ctxt			`thenTc` \ theta ->
    tc_type ty				`thenTc` \ tau ->
    returnTc (mkSigmaTy tyvars theta tau)

tc_types arg_tys = mapTc tc_type arg_tys
\end{code}

Help functions for type applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
tc_app :: RenamedHsType -> [RenamedHsType] -> TcM Type
tc_app (HsAppTy ty1 ty2) tys
  = tc_app ty1 (ty2:tys)

tc_app ty tys
  = tcAddErrCtxt (appKindCtxt pp_app)	$
    tc_types tys			`thenTc` \ arg_tys ->
    case ty of
	HsTyVar fun -> tc_fun_type fun arg_tys
	other	    -> tc_type ty		`thenTc` \ fun_ty ->
		       returnNF_Tc (mkAppTys fun_ty arg_tys)
  where
    pp_app = ppr ty <+> sep (map pprParendHsType tys)

-- (tc_fun_type ty arg_tys) returns (mkAppTys ty arg_tys)
-- But not quite; for synonyms it checks the correct arity, and builds a SynTy
-- 	hence the rather strange functionality.

tc_fun_type name arg_tys
  = tcLookup name			`thenTc` \ thing ->
    case thing of
	ATyVar tv -> returnTc (mkAppTys (mkTyVarTy tv) arg_tys)

	AGlobal (ATyCon tc)
		| isSynTyCon tc ->  returnTc (mkSynTy tc arg_tys)
		| otherwise	->  returnTc (mkTyConApp tc arg_tys)

	other -> failWithTc (wrongThingErr "type constructor" thing name)
\end{code}


Contexts
~~~~~~~~
\begin{code}
tc_pred assn@(HsClassP class_name tys)
  = tcAddErrCtxt (appKindCtxt (ppr assn))	$
    tc_types tys			`thenTc` \ arg_tys ->
    tcLookupGlobal class_name			`thenTc` \ thing ->
    case thing of
	AClass clas -> returnTc (ClassP clas arg_tys)
	other 	    -> failWithTc (wrongThingErr "class" (AGlobal thing) class_name)

tc_pred assn@(HsIParam name ty)
  = tcAddErrCtxt (appKindCtxt (ppr assn))	$
    tc_type ty					`thenTc` \ arg_ty ->
    returnTc (IParam name arg_ty)
\end{code}



%************************************************************************
%*									*
\subsection{Type variables, with knot tying!}
%*									*
%************************************************************************

\begin{code}
mkImmutTyVars :: [(Name,Kind)] -> [TyVar]
mkImmutTyVars pairs = [mkTyVar name kind | (name, kind) <- pairs]

mkTyClTyVars :: Kind 			-- Kind of the tycon or class
	     -> [HsTyVarBndr Name]
	     -> [TyVar]
mkTyClTyVars kind tyvar_names
  = mkImmutTyVars tyvars_w_kinds
  where
    (tyvars_w_kinds, _) = zipFunTys (hsTyVarNames tyvar_names) kind
\end{code}


%************************************************************************
%*									*
\subsection{Signatures}
%*									*
%************************************************************************

@tcSigs@ checks the signatures for validity, and returns a list of
{\em freshly-instantiated} signatures.  That is, the types are already
split up, and have fresh type variables installed.  All non-type-signature
"RenamedSigs" are ignored.

The @TcSigInfo@ contains @TcTypes@ because they are unified with
the variable's type, and after that checked to see whether they've
been instantiated.

\begin{code}
data TcSigInfo
  = TySigInfo	    
	Name			-- N, the Name in corresponding binding

	TcId			-- *Polymorphic* binder for this value...
				-- Has name = N

	[TcTyVar]		-- tyvars
	TcThetaType		-- theta
	TcTauType		-- tau

	TcId			-- *Monomorphic* binder for this value
				-- Does *not* have name = N
				-- Has type tau

	[Inst]			-- Empty if theta is null, or
				-- (method mono_id) otherwise

	SrcLoc			-- Of the signature

instance Outputable TcSigInfo where
    ppr (TySigInfo nm id tyvars theta tau _ inst loc) =
	ppr nm <+> ptext SLIT("::") <+> ppr tyvars <+> ppr theta <+> ptext SLIT("=>") <+> ppr tau

maybeSig :: [TcSigInfo] -> Name -> Maybe (TcSigInfo)
	-- Search for a particular signature
maybeSig [] name = Nothing
maybeSig (sig@(TySigInfo sig_name _ _ _ _ _ _ _) : sigs) name
  | name == sig_name = Just sig
  | otherwise	     = maybeSig sigs name
\end{code}


\begin{code}
tcTySig :: RenamedSig -> TcM TcSigInfo

tcTySig (Sig v ty src_loc)
 = tcAddSrcLoc src_loc				$ 
   tcHsSigType (FunSigCtxt v) ty		`thenTc` \ sigma_tc_ty ->
   mkTcSig (mkLocalId v sigma_tc_ty) src_loc	`thenNF_Tc` \ sig -> 
   returnTc sig

mkTcSig :: TcId -> SrcLoc -> NF_TcM TcSigInfo
mkTcSig poly_id src_loc
  = 	-- Instantiate this type
	-- It's important to do this even though in the error-free case
	-- we could just split the sigma_tc_ty (since the tyvars don't
	-- unified with anything).  But in the case of an error, when
	-- the tyvars *do* get unified with something, we want to carry on
	-- typechecking the rest of the program with the function bound
	-- to a pristine type, namely sigma_tc_ty
   let
	(tyvars, rho) = tcSplitForAllTys (idType poly_id)
   in
   tcInstSigVars tyvars			`thenNF_Tc` \ tyvars' ->
	-- Make *signature* type variables

   let
     tyvar_tys' = mkTyVarTys tyvars'
     rho' = substTy (mkTopTyVarSubst tyvars tyvar_tys') rho
	-- mkTopTyVarSubst because the tyvars' are fresh

     (theta', tau') = tcSplitRhoTy rho'
	-- This splitRhoTy tries hard to make sure that tau' is a type synonym
	-- wherever possible, which can improve interface files.
   in
   newMethodWithGivenTy SignatureOrigin 
		poly_id
		tyvar_tys'
		theta' tau'			`thenNF_Tc` \ inst ->
	-- We make a Method even if it's not overloaded; no harm
	
   returnNF_Tc (TySigInfo name poly_id tyvars' theta' tau' (instToId inst) [inst] src_loc)
  where
    name = idName poly_id
\end{code}



%************************************************************************
%*									*
\subsection{Checking signature type variables}
%*									*
%************************************************************************

@checkSigTyVars@ is used after the type in a type signature has been unified with
the actual type found.  It then checks that the type variables of the type signature
are
	(a) Still all type variables
		eg matching signature [a] against inferred type [(p,q)]
		[then a will be unified to a non-type variable]

	(b) Still all distinct
		eg matching signature [(a,b)] against inferred type [(p,p)]
		[then a and b will be unified together]

	(c) Not mentioned in the environment
		eg the signature for f in this:

			g x = ... where
					f :: a->[a]
					f y = [x,y]

		Here, f is forced to be monorphic by the free occurence of x.

	(d) Not (unified with another type variable that is) in scope.
		eg f x :: (r->r) = (\y->y) :: forall a. a->r
	    when checking the expression type signature, we find that
	    even though there is nothing in scope whose type mentions r,
	    nevertheless the type signature for the expression isn't right.

	    Another example is in a class or instance declaration:
		class C a where
		   op :: forall b. a -> b
		   op x = x
	    Here, b gets unified with a

Before doing this, the substitution is applied to the signature type variable.

We used to have the notion of a "DontBind" type variable, which would
only be bound to itself or nothing.  Then points (a) and (b) were 
self-checking.  But it gave rise to bogus consequential error messages.
For example:

   f = (*)	-- Monomorphic

   g :: Num a => a -> a
   g x = f x x

Here, we get a complaint when checking the type signature for g,
that g isn't polymorphic enough; but then we get another one when
dealing with the (Num x) context arising from f's definition;
we try to unify x with Int (to default it), but find that x has already
been unified with the DontBind variable "a" from g's signature.
This is really a problem with side-effecting unification; we'd like to
undo g's effects when its type signature fails, but unification is done
by side effect, so we can't (easily).

So we revert to ordinary type variables for signatures, and try to
give a helpful message in checkSigTyVars.

\begin{code}
checkSigTyVars :: [TcTyVar]		-- Universally-quantified type variables in the signature
	       -> TcTyVarSet		-- Tyvars that are free in the type signature
					--	Not necessarily zonked
					-- 	These should *already* be in the free-in-env set, 
					-- 	and are used here only to improve the error message
	       -> TcM [TcTyVar]		-- Zonked signature type variables

checkSigTyVars [] free = returnTc []
checkSigTyVars sig_tyvars free_tyvars
  = zonkTcTyVars sig_tyvars		`thenNF_Tc` \ sig_tys ->
    tcGetGlobalTyVars			`thenNF_Tc` \ globals ->

    checkTcM (allDistinctTyVars sig_tys globals)
	     (complain sig_tys globals)	`thenTc_`

    returnTc (map (tcGetTyVar "checkSigTyVars") sig_tys)

  where
    complain sig_tys globals
      = -- For the in-scope ones, zonk them and construct a map
	-- from the zonked tyvar to the in-scope one
	-- If any of the in-scope tyvars zonk to a type, then ignore them;
	-- that'll be caught later when we back up to their type sig
 	tcGetEnv				`thenNF_Tc` \ env ->
 	let
 	   in_scope_tvs = tcEnvTyVars env
 	in
	zonkTcTyVars in_scope_tvs		`thenNF_Tc` \ in_scope_tys ->
	let
	    in_scope_assoc = [ (zonked_tv, in_scope_tv) 
			     | (z_ty, in_scope_tv) <- in_scope_tys `zip` in_scope_tvs,
			       Just zonked_tv <- [tcGetTyVar_maybe z_ty]
    			     ]
	    in_scope_env = mkVarEnv in_scope_assoc
	in

	-- "check" checks each sig tyvar in turn
        foldlNF_Tc check
		   (env2, in_scope_env, [])
		   (tidy_tvs `zip` tidy_tys)	`thenNF_Tc` \ (env3, _, msgs) ->

        failWithTcM (env3, main_msg $$ nest 4 (vcat msgs))
      where
	(env1, tidy_tvs) = tidyOpenTyVars emptyTidyEnv sig_tyvars
	(env2, tidy_tys) = tidyOpenTypes  env1	       sig_tys

	main_msg = ptext SLIT("Inferred type is less polymorphic than expected")

	check (tidy_env, acc, msgs) (sig_tyvar,ty)
		-- sig_tyvar is from the signature;
		-- ty is what you get if you zonk sig_tyvar and then tidy it
		--
		-- acc maps a zonked type variable back to a signature type variable
	  = case tcGetTyVar_maybe ty of {
	      Nothing ->			-- Error (a)!
			returnNF_Tc (tidy_env, acc, unify_msg sig_tyvar (quotes (ppr ty)) : msgs) ;

	      Just tv ->

	    case lookupVarEnv acc tv of {
		Just sig_tyvar' -> 	-- Error (b) or (d)!
			returnNF_Tc (tidy_env, acc, unify_msg sig_tyvar thing : msgs)
		    where
			thing = ptext SLIT("another quantified type variable") <+> quotes (ppr sig_tyvar')

	      ; Nothing ->

	    if tv `elemVarSet` globals	-- Error (c)! Type variable escapes
					-- The least comprehensible, so put it last
			-- Game plan: 
			--    a) get the local TcIds from the environment,
			-- 	 and pass them to find_globals (they might have tv free)
			--    b) similarly, find any free_tyvars that mention tv
	    then   tcGetEnv 							`thenNF_Tc` \ ve ->
        	   find_globals tv tidy_env  [] (tcEnvTcIds ve)			`thenNF_Tc` \ (tidy_env1, globs) ->
        	   find_frees   tv tidy_env1 [] (varSetElems free_tyvars)	`thenNF_Tc` \ (tidy_env2, frees) ->
		   returnNF_Tc (tidy_env2, acc, escape_msg sig_tyvar tv globs frees : msgs)

	    else 	-- All OK
	    returnNF_Tc (tidy_env, extendVarEnv acc tv sig_tyvar, msgs)
	    }}

-- find_globals looks at the value environment and finds values
-- whose types mention the offending type variable.  It has to be 
-- careful to zonk the Id's type first, so it has to be in the monad.
-- We must be careful to pass it a zonked type variable, too.

find_globals :: Var 
             -> TidyEnv 
             -> [(Name,Type)] 
             -> [Id] 
             -> NF_TcM (TidyEnv,[(Name,Type)])

find_globals tv tidy_env acc []
  = returnNF_Tc (tidy_env, acc)

find_globals tv tidy_env acc (id:ids) 
  | isEmptyVarSet (idFreeTyVars id)
  = find_globals tv tidy_env acc ids

  | otherwise
  = zonkTcType (idType id)	`thenNF_Tc` \ id_ty ->
    if tv `elemVarSet` tyVarsOfType id_ty then
    	let 
    	   (tidy_env', id_ty') = tidyOpenType tidy_env id_ty
	   acc'		       = (idName id, id_ty') : acc
    	in
    	find_globals tv tidy_env' acc' ids
    else
    	find_globals tv tidy_env  acc  ids

find_frees tv tidy_env acc []
  = returnNF_Tc (tidy_env, acc)
find_frees tv tidy_env acc (ftv:ftvs)
  = zonkTcTyVar ftv	`thenNF_Tc` \ ty ->
    if tv `elemVarSet` tyVarsOfType ty then
	let
	    (tidy_env', ftv') = tidyOpenTyVar tidy_env ftv
	in
	find_frees tv tidy_env' (ftv':acc) ftvs
    else
	find_frees tv tidy_env  acc        ftvs


escape_msg sig_tv tv globs frees
  = mk_msg sig_tv <+> ptext SLIT("escapes") $$
    if not (null globs) then
	vcat [pp_it <+> ptext SLIT("is mentioned in the environment"),
	      ptext SLIT("The following variables in the environment mention") <+> quotes (ppr tv),
	      nest 2 (vcat_first 10 [ppr name <+> dcolon <+> ppr ty | (name,ty) <- globs])
	]
     else if not (null frees) then
	vcat [ptext SLIT("It is reachable from the type variable(s)") <+> pprQuotedList frees,
	      nest 2 (ptext SLIT("which") <+> is_are <+> ptext SLIT("free in the signature"))
	]
     else
	empty	-- Sigh.  It's really hard to give a good error message
		-- all the time.   One bad case is an existential pattern match
  where
    is_are | isSingleton frees = ptext SLIT("is")
	   | otherwise         = ptext SLIT("are")
    pp_it | sig_tv /= tv = ptext SLIT("It unifies with") <+> quotes (ppr tv) <> comma <+> ptext SLIT("which")
	  | otherwise    = ptext SLIT("It")

    vcat_first :: Int -> [SDoc] -> SDoc
    vcat_first n []     = empty
    vcat_first 0 (x:xs) = text "...others omitted..."
    vcat_first n (x:xs) = x $$ vcat_first (n-1) xs

unify_msg tv thing = mk_msg tv <+> ptext SLIT("is unified with") <+> thing
mk_msg tv          = ptext SLIT("Quantified type variable") <+> quotes (ppr tv)
\end{code}

These two context are used with checkSigTyVars
    
\begin{code}
sigCtxt :: Message -> [TcTyVar] -> TcThetaType -> TcTauType
	-> TidyEnv -> NF_TcM (TidyEnv, Message)
sigCtxt when sig_tyvars sig_theta sig_tau tidy_env
  = zonkTcType sig_tau		`thenNF_Tc` \ actual_tau ->
    let
	(env1, tidy_sig_tyvars)  = tidyOpenTyVars tidy_env sig_tyvars
	(env2, tidy_sig_rho)	 = tidyOpenType env1 (mkRhoTy sig_theta sig_tau)
	(env3, tidy_actual_tau)  = tidyOpenType env2 actual_tau
	msg = vcat [ptext SLIT("Signature type:    ") <+> pprType (mkForAllTys tidy_sig_tyvars tidy_sig_rho),
		    ptext SLIT("Type to generalise:") <+> pprType tidy_actual_tau,
		    when
		   ]
    in
    returnNF_Tc (env3, msg)

sigPatCtxt bound_tvs bound_ids tidy_env
  = returnNF_Tc (env1,
		 sep [ptext SLIT("When checking a pattern that binds"),
		      nest 4 (vcat (zipWith ppr_id show_ids tidy_tys))])
  where
    show_ids = filter is_interesting bound_ids
    is_interesting id = any (`elemVarSet` idFreeTyVars id) bound_tvs

    (env1, tidy_tys) = tidyOpenTypes tidy_env (map idType show_ids)
    ppr_id id ty     = ppr id <+> dcolon <+> ppr ty
	-- Don't zonk the types so we get the separate, un-unified versions
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
typeKindCtxt :: RenamedHsType -> Message
typeKindCtxt ty = sep [ptext SLIT("When checking that"),
	  	       nest 2 (quotes (ppr ty)),
		       ptext SLIT("is a type")]

appKindCtxt :: SDoc -> Message
appKindCtxt pp = ptext SLIT("When checking kinds in") <+> quotes pp

wrongThingErr expected thing name
  = pp_thing thing <+> quotes (ppr name) <+> ptext SLIT("used as a") <+> text expected
  where
    pp_thing (AGlobal (ATyCon _)) = ptext SLIT("Type constructor")
    pp_thing (AGlobal (AClass _)) = ptext SLIT("Class")
    pp_thing (AGlobal (AnId   _)) = ptext SLIT("Identifier")
    pp_thing (ATyVar _) 	  = ptext SLIT("Type variable")
    pp_thing (ATcId _)  	  = ptext SLIT("Local identifier")
    pp_thing (AThing _) 	  = ptext SLIT("Utterly bogus")
\end{code}
