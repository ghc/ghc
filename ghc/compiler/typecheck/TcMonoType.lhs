%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
module TcMonoType ( tcHsSigType, tcHsType, tcIfaceType, tcHsTheta, tcHsPred,
		    UserTypeCtxt(..),

			-- Kind checking
		    kcHsTyVar, kcHsTyVars, mkTyClTyVars,
		    kcHsType, kcHsSigType, kcHsSigTypes, 
		    kcHsLiftedSigType, kcHsContext,
		    tcAddScopedTyVars, tcHsTyVars, mkImmutTyVars,

		    TcSigInfo(..), tcTySig, mkTcSig, maybeSig, tcSigPolyId, tcSigMonoId
	          ) where

#include "HsVersions.h"

import HsSyn		( HsType(..), HsTyVarBndr(..),
                          Sig(..), HsPred(..), pprParendHsType, HsTupCon(..), hsTyVarNames )
import RnHsSyn		( RenamedHsType, RenamedHsPred, RenamedContext, RenamedSig, extractHsTyVars )
import TcHsSyn		( TcId )

import TcMonad
import TcEnv		( tcExtendTyVarEnv, tcLookup, tcLookupGlobal,
			  tcInLocalScope,
		 	  TyThing(..), TcTyThing(..), tcExtendKindEnv
			)
import TcMType		( newKindVar, zonkKindEnv, tcInstType,
			  checkValidType, UserTypeCtxt(..), pprUserTypeCtxt
			)
import TcUnify		( unifyKind, unifyOpenTypeKind )
import TcType		( Type, Kind, SourceType(..), ThetaType, TyVarDetails(..),
			  TcTyVar, TcKind, TcThetaType, TcTauType,
			  mkTyVarTy, mkTyVarTys, mkFunTy, mkSynTy,
		 	  hoistForAllTys, zipFunTys, 
			  mkSigmaTy, mkPredTy, mkTyConApp, mkAppTys, 
			  liftedTypeKind, unliftedTypeKind, mkArrowKind,
			  mkArrowKinds, tcSplitFunTy_maybe
			)
import Inst		( Inst, InstOrigin(..), newMethodWithGivenTy, instToId )

import Id		( mkLocalId, idName, idType )
import Var		( TyVar, mkTyVar, tyVarKind )
import ErrUtils		( Message )
import TyCon		( TyCon, isSynTyCon, tyConKind )
import Class		( classTyCon )
import Name		( Name )
import NameSet
import TysWiredIn	( mkListTy, mkPArrTy, mkTupleTy, genUnitTyCon )
import BasicTypes	( Boxity(..) )
import SrcLoc		( SrcLoc )
import Util		( lengthIs )
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



tcAddScopedTyVars :: [RenamedHsType] -> TcM a -> TcM a
-- tcAddScopedTyVars is used for scoped type variables
-- added by pattern type signatures
--	e.g.  \ (x::a) (y::a) -> x+y
-- They never have explicit kinds (because this is source-code only)
-- They are mutable (because they can get bound to a more specific type)

-- Find the not-already-in-scope signature type variables,
-- kind-check them, and bring them into scope
--
-- We no longer specify that these type variables must be univerally 
-- quantified (lots of email on the subject).  If you want to put that 
-- back in, you need to
--	a) Do a checkSigTyVars after thing_inside
--	b) More insidiously, don't pass in expected_ty, else
--	   we unify with it too early and checkSigTyVars barfs
--	   Instead you have to pass in a fresh ty var, and unify
--	   it with expected_ty afterwards
tcAddScopedTyVars [] thing_inside
  = thing_inside	-- Quick get-out for the empty case

tcAddScopedTyVars sig_tys thing_inside
  = tcGetEnv					`thenNF_Tc` \ env ->
    let
	all_sig_tvs	= foldr (unionNameSets . extractHsTyVars) emptyNameSet sig_tys
	sig_tvs 	= filter not_in_scope (nameSetToList all_sig_tvs)
 	not_in_scope tv = not (tcInLocalScope env tv)
    in	      
    mapNF_Tc newNamedKindVar sig_tvs			`thenTc` \ kind_env ->
    tcExtendKindEnv kind_env (kcHsSigTypes sig_tys)	`thenTc_`
    zonkKindEnv kind_env				`thenNF_Tc` \ tvs_w_kinds ->
    listTc [ tcNewMutTyVar name kind PatSigTv
	   | (name, kind) <- tvs_w_kinds]		`thenNF_Tc` \ tyvars ->
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

kcHsType (HsKindSig ty k)
  = kcHsType ty			`thenTc` \ k' ->
    unifyKind k k' 		`thenTc_`
    returnTc k

kcHsType (HsListTy ty)
  = kcLiftedType ty		`thenTc` \ tau_ty ->
    returnTc liftedTypeKind

kcHsType (HsPArrTy ty)
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
kc_pred :: RenamedHsPred -> TcM TcKind	-- Does *not* check for a saturated
					-- application (reason: used from TcDeriv)
kc_pred pred@(HsIParam name ty)
  = kcHsType ty

kc_pred pred@(HsClassP cls tys)
  = kcClass cls				`thenTc` \ kind ->
    mapTc kcHsType tys			`thenTc` \ arg_kinds ->
    newKindVar 				`thenNF_Tc` \ kv -> 
    unifyKind kind (mkArrowKinds arg_kinds kv)	`thenTc_` 
    returnTc kv

---------------------------
kcHsContext ctxt = mapTc_ kcHsPred ctxt

kcHsPred pred		-- Checks that the result is of kind liftedType
  = tcAddErrCtxt (appKindCtxt (ppr pred))	$
    kc_pred pred				`thenTc` \ kind ->
    unifyKind liftedTypeKind kind		`thenTc_`
    returnTc ()
    

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

tc_type (HsKindSig ty k)
  = tc_type ty	-- Kind checking done already

tc_type (HsListTy ty)
  = tc_type ty	`thenTc` \ tau_ty ->
    returnTc (mkListTy tau_ty)

tc_type (HsPArrTy ty)
  = tc_type ty	`thenTc` \ tau_ty ->
    returnTc (mkPArrTy tau_ty)

tc_type (HsTupleTy (HsTupCon _ boxity arity) tys)
  = ASSERT( tys `lengthIs` arity )
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
tcHsPred pred = kc_pred pred `thenTc_`  tc_pred pred
	-- Is happy with a partial application, e.g. (ST s)
	-- Used from TcDeriv

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
    ppr (TySigInfo id tyvars theta tau _ inst loc) =
	ppr id <+> ptext SLIT("::") <+> ppr tyvars <+> ppr theta <+> ptext SLIT("=>") <+> ppr tau

tcSigPolyId :: TcSigInfo -> TcId
tcSigPolyId (TySigInfo id _ _ _ _ _ _) = id

tcSigMonoId :: TcSigInfo -> TcId
tcSigMonoId (TySigInfo _ _ _ _ id _ _) = id

maybeSig :: [TcSigInfo] -> Name -> Maybe (TcSigInfo)
	-- Search for a particular signature
maybeSig [] name = Nothing
maybeSig (sig@(TySigInfo sig_id _ _ _ _ _ _) : sigs) name
  | name == idName sig_id = Just sig
  | otherwise	     	  = maybeSig sigs name
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
   tcInstType SigTv (idType poly_id)		`thenNF_Tc` \ (tyvars', theta', tau') ->

   newMethodWithGivenTy SignatureOrigin 
			poly_id
			(mkTyVarTys tyvars')
			theta' tau'		`thenNF_Tc` \ inst ->
	-- We make a Method even if it's not overloaded; no harm
	
   returnNF_Tc (TySigInfo poly_id tyvars' theta' tau' 
			  (instToId inst) [inst] src_loc)
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
