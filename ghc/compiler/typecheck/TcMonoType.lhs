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

import HsSyn		( HsType(..), HsTyVarBndr(..), HsTyOp(..),
                          Sig(..), HsPred(..), pprParendHsType, HsTupCon(..), hsTyVarNames )
import RnHsSyn		( RenamedHsType, RenamedHsPred, RenamedContext, RenamedSig, extractHsTyVars )
import TcHsSyn		( TcId )

import TcRnMonad
import TcEnv		( tcExtendTyVarEnv, tcLookup, tcLookupGlobal,
		 	  TyThing(..), TcTyThing(..), tcExtendKindEnv,
			  getInLocalScope
			)
import TcMType		( newMutTyVar, newKindVar, zonkKindEnv, tcInstType, zonkTcType,
			  checkValidType, UserTypeCtxt(..), pprUserTypeCtxt, newOpenTypeKind
			)
import TcUnify		( unifyKind, unifyOpenTypeKind, unifyFunKind )
import TcType		( Type, Kind, SourceType(..), ThetaType, TyVarDetails(..),
			  TcTyVar, TcKind, TcThetaType, TcTauType,
			  mkTyVarTy, mkTyVarTys, mkFunTy, isTypeKind,
		 	  zipFunTys, mkForAllTys, mkFunTys, tcEqType, isPredTy,
			  mkSigmaTy, mkPredTy, mkGenTyConApp, mkTyConApp, mkAppTys, 
			  liftedTypeKind, unliftedTypeKind, mkArrowKind, eqKind,
			  mkArrowKinds, tcSplitFunTy_maybe, tcSplitForAllTys
			)
import qualified Type	( splitFunTys )
import Inst		( Inst, InstOrigin(..), newMethod, instToId )

import Id		( mkLocalId, idName, idType )
import Var		( TyVar, mkTyVar, tyVarKind )
import ErrUtils		( Message )
import TyCon		( TyCon, tyConKind )
import Class		( classTyCon )
import Name		( Name )
import NameSet
import Subst		( deShadowTy )
import TysWiredIn	( mkListTy, mkPArrTy, mkTupleTy, genUnitTyCon )
import BasicTypes	( Boxity(..) )
import SrcLoc		( SrcLoc )
import Util		( lengthIs )
import Outputable
import List		( nubBy )
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
tcHsSigType ctxt ty = addErrCtxt (checkTypeCtxt ctxt ty) (
			kcTypeType ty		`thenM_`
		        tcHsType ty
		      )				`thenM` \ ty' ->
		      checkValidType ctxt ty'	`thenM_`
		      returnM ty'

checkTypeCtxt ctxt ty
  = vcat [ptext SLIT("In the type:") <+> ppr ty,
	  ptext SLIT("While checking") <+> pprUserTypeCtxt ctxt ]

tcHsType    :: RenamedHsType -> TcM Type
  -- Don't do kind checking, nor validity checking, 
  -- 	but do hoist for-alls to the top
  -- This is used in type and class decls, where kinding is
  -- done in advance, and validity checking is done later
  -- [Validity checking done later because of knot-tying issues.]
tcHsType ty = tc_type ty  `thenM` \ ty' ->  
	      returnM (hoistForAllTys ty')

tcHsTheta :: RenamedContext -> TcM ThetaType
-- Used when we are expecting a ClassContext (i.e. no implicit params)
-- Does not do validity checking, like tcHsType
tcHsTheta hs_theta = mappM tc_pred hs_theta

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
  = kcHsTyVars tv_names 				`thenM` \ tv_names_w_kinds ->
    tcExtendKindEnv tv_names_w_kinds kind_check		`thenM_`
    zonkKindEnv tv_names_w_kinds			`thenM` \ tvs_w_kinds ->
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
  = getInLocalScope			`thenM` \ in_scope ->
    let
	all_sig_tvs	= foldr (unionNameSets . extractHsTyVars) emptyNameSet sig_tys
	sig_tvs 	= filter (not . in_scope) (nameSetToList all_sig_tvs)
    in	      
    mappM newNamedKindVar sig_tvs			`thenM` \ kind_env ->
    tcExtendKindEnv kind_env (kcHsSigTypes sig_tys)	`thenM_`
    zonkKindEnv kind_env				`thenM` \ tvs_w_kinds ->
    sequenceM [ newMutTyVar name kind PatSigTv
	      | (name, kind) <- tvs_w_kinds]		`thenM` \ tyvars ->
    tcExtendTyVarEnv tyvars thing_inside
\end{code}
    

\begin{code}
kcHsTyVar  :: HsTyVarBndr name   -> TcM (name, TcKind)
kcHsTyVars :: [HsTyVarBndr name] -> TcM [(name, TcKind)]

kcHsTyVar (UserTyVar name)       = newNamedKindVar name
kcHsTyVar (IfaceTyVar name kind) = returnM (name, kind)

kcHsTyVars tvs = mappM kcHsTyVar tvs

newNamedKindVar name = newKindVar	`thenM` \ kind ->
		       returnM (name, kind)

---------------------------
kcLiftedType :: RenamedHsType -> TcM Kind
	-- The type ty must be a *lifted* *type*
kcLiftedType ty = kcHsType ty	`thenM` \ act_kind ->
		  checkExpectedKind (ppr ty) act_kind liftedTypeKind
    
---------------------------
kcTypeType :: RenamedHsType -> TcM ()
	-- The type ty must be a *type*, but it can be lifted or unlifted.
kcTypeType ty
  = kcHsType ty			`thenM` \ kind ->
    if isTypeKind kind then
	return ()
    else
    newOpenTypeKind 				`thenM` \ exp_kind ->
    checkExpectedKind (ppr ty) kind exp_kind	`thenM_`
    returnM ()

---------------------------
kcHsSigType, kcHsLiftedSigType :: RenamedHsType -> TcM ()
	-- Used for type signatures
kcHsSigType ty 	     = kcTypeType ty
kcHsSigTypes tys     = mappM_ kcHsSigType tys
kcHsLiftedSigType ty = kcLiftedType ty `thenM_` returnM ()

---------------------------
kcHsType :: RenamedHsType -> TcM TcKind
-- kcHsType *returns* the kind of the type, rather than taking an expected
-- kind as argument as tcExpr does.  Reason: the kind of (->) is
--	forall bx1 bx2. Type bx1 -> Type bx2 -> Type Boxed
-- so we'd need to generate huge numbers of bx variables.

kcHsType (HsTyVar name)   = kcTyVar name
kcHsType (HsListTy ty)    = kcLiftedType ty
kcHsType (HsPArrTy ty)    = kcLiftedType ty
kcHsType (HsParTy ty)	  = kcHsType ty		    -- Skip parentheses markers
kcHsType (HsNumTy _)      = returnM liftedTypeKind  -- The unit type for generics
kcHsType (HsKindSig ty k) = kcHsType ty		`thenM` \ act_kind ->
			    checkExpectedKind (ppr ty) act_kind k

kcHsType (HsTupleTy (HsTupCon boxity _) tys)
  = mappM kcTypeType tys	`thenM_`
    returnM (case boxity of
		  Boxed   -> liftedTypeKind
		  Unboxed -> unliftedTypeKind)

kcHsType (HsFunTy ty1 ty2)
  = kcTypeType ty1	`thenM_`
    kcTypeType ty2	`thenM_`
    returnM liftedTypeKind

kcHsType (HsOpTy ty1 HsArrow ty2)
  = kcTypeType ty1	`thenM_`
    kcTypeType ty2	`thenM_`
    returnM liftedTypeKind

kcHsType ty@(HsOpTy ty1 op_ty@(HsTyOp op) ty2)
  = addErrCtxt (appKindCtxt (ppr ty))	$
    kcTyVar op				`thenM` \ op_kind ->
    kcApps (ppr op_ty) op_kind [ty1,ty2]

kcHsType (HsPredTy pred)
  = kcHsPred pred		`thenM_`
    returnM liftedTypeKind

kcHsType ty@(HsAppTy ty1 ty2)
  = addErrCtxt (appKindCtxt (ppr ty))	$
    kc_app ty []
  where
    kc_app (HsAppTy f a) as = kc_app f (a:as)
    kc_app f		 as = kcHsType f	`thenM` \ fk ->
			      kcApps (ppr f) fk as

kcHsType (HsForAllTy (Just tv_names) context ty)
  = kcHsTyVars tv_names		`thenM` \ kind_env ->
    tcExtendKindEnv kind_env	$
    kcHsContext context		`thenM_`
    kcLiftedType ty
	-- The body of a forall must be of kind *
	-- In principle, I suppose, we could allow unlifted types,
	-- but it seems simpler to stick to lifted types for now.

---------------------------
kcApps :: SDoc 			-- The function
       -> TcKind		-- Function kind
       -> [RenamedHsType]	-- Arg types
       -> TcM TcKind		-- Result kind
kcApps pp_fun fun_kind args
  = go fun_kind args
  where
    go fk []       = returnM fk
    go fk (ty:tys) = unifyFunKind fk	`thenM` \ mb_fk ->
		     case mb_fk of {
			Nothing       -> failWithTc too_few_args ;
			Just (ak',fk') -> 
		     kcHsType ty			`thenM` \ ak ->
		     checkExpectedKind (ppr ty) ak ak'  `thenM_`
		     go fk' tys }

    too_few_args = ptext SLIT("Kind error:") <+> quotes pp_fun <+>
			ptext SLIT("is applied to too many type arguments")

---------------------------
-- We would like to get a decent error message from
--   (a) Under-applied type constructors
--		f :: (Maybe, Maybe)
--   (b) Over-applied type constructors
--		f :: Int x -> Int x
--

checkExpectedKind :: SDoc -> TcKind -> TcKind -> TcM TcKind
-- A fancy wrapper for 'unifyKind', which tries to give 
-- decent error messages.
-- Returns the same kind that it is passed, exp_kind
checkExpectedKind pp_ty act_kind exp_kind
  | act_kind `eqKind` exp_kind -- Short cut for a very common case
  = returnM exp_kind	
  | otherwise
  = tryTc (unifyKind exp_kind act_kind)	`thenM` \ (errs, mb_r) ->
    case mb_r of {
	Just _  -> returnM exp_kind ;	-- Unification succeeded
	Nothing ->

	-- So there's definitely an error
	-- Now to find out what sort
    zonkTcType exp_kind		`thenM` \ exp_kind ->
    zonkTcType act_kind		`thenM` \ act_kind ->

    let (exp_as, _) = Type.splitFunTys exp_kind
        (act_as, _) = Type.splitFunTys act_kind
		-- Use the Type versions for kinds	
	n_exp_as = length exp_as
	n_act_as = length act_as

	err | n_exp_as < n_act_as	-- E.g. [Maybe]
	    = quotes pp_ty <+> ptext SLIT("is not applied to enough type arguments")

		-- Now n_exp_as >= n_act_as. In the next two cases, 
		-- n_exp_as == 0, and hence so is n_act_as
	    | exp_kind `eqKind` liftedTypeKind && act_kind `eqKind` unliftedTypeKind
	    = ptext SLIT("Expecting a lifted type, but") <+> quotes pp_ty 
		<+> ptext SLIT("is unlifted")

	    | exp_kind `eqKind` unliftedTypeKind && act_kind `eqKind` liftedTypeKind
	    = ptext SLIT("Expecting an unlifted type, but") <+> quotes pp_ty 
		<+> ptext SLIT("is lifted")

	    | otherwise 		-- E.g. Monad [Int]
	    = sep [ ptext SLIT("Expecting kind") <+> quotes (ppr exp_kind) <> comma,
		    ptext SLIT("but") <+> quotes pp_ty <+> 
		        ptext SLIT("has kind") <+> quotes (ppr act_kind)]
   in
   failWithTc (ptext SLIT("Kind error:") <+> err) 
   }

---------------------------
kc_pred :: RenamedHsPred -> TcM TcKind	-- Does *not* check for a saturated
					-- application (reason: used from TcDeriv)
kc_pred pred@(HsIParam name ty)
  = kcHsType ty

kc_pred pred@(HsClassP cls tys)
  = kcClass cls		`thenM` \ kind ->
    kcApps (ppr cls) kind tys

---------------------------
kcHsContext ctxt = mappM_ kcHsPred ctxt

kcHsPred pred		-- Checks that the result is of kind liftedType
  = addErrCtxt (appKindCtxt (ppr pred))	$
    kc_pred pred			`thenM` \ kind ->
    checkExpectedKind (ppr pred) kind liftedTypeKind
    

 ---------------------------
kcTyVar name	-- Could be a tyvar or a tycon
  = tcLookup name	`thenM` \ thing ->
    case thing of 
	AThing kind 	    -> returnM kind
	ATyVar tv	    -> returnM (tyVarKind tv)
	AGlobal (ATyCon tc) -> returnM (tyConKind tc) 
	other		    -> failWithTc (wrongThingErr "type" thing name)

kcClass cls	-- Must be a class
  = tcLookup cls 				`thenM` \ thing -> 
    case thing of
	AThing kind	      -> returnM kind
	AGlobal (AClass cls)  -> returnM (tyConKind (classTyCon cls))
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
  = tc_type ty	`thenM` \ tau_ty ->
    returnM (mkListTy tau_ty)

tc_type (HsPArrTy ty)
  = tc_type ty	`thenM` \ tau_ty ->
    returnM (mkPArrTy tau_ty)

tc_type (HsTupleTy (HsTupCon boxity arity) tys)
  = ASSERT( tys `lengthIs` arity )
    tc_types tys	`thenM` \ tau_tys ->
    returnM (mkTupleTy boxity arity tau_tys)

tc_type (HsFunTy ty1 ty2)
  = tc_type ty1			`thenM` \ tau_ty1 ->
    tc_type ty2			`thenM` \ tau_ty2 ->
    returnM (mkFunTy tau_ty1 tau_ty2)

tc_type (HsOpTy ty1 HsArrow ty2)
  = tc_type ty1 `thenM` \ tau_ty1 ->
    tc_type ty2 `thenM` \ tau_ty2 ->
    returnM (mkFunTy tau_ty1 tau_ty2)

tc_type (HsOpTy ty1 (HsTyOp op) ty2)
  = tc_type ty1 `thenM` \ tau_ty1 ->
    tc_type ty2 `thenM` \ tau_ty2 ->
    tc_fun_type op [tau_ty1,tau_ty2]

tc_type (HsParTy ty)		-- Remove the parentheses markers
  = tc_type ty

tc_type (HsNumTy n)
  = ASSERT(n== 1)
    returnM (mkTyConApp genUnitTyCon [])

tc_type ty@(HsAppTy ty1 ty2) 
  = addErrCtxt (appKindCtxt (ppr ty))	$
    tc_app ty1 [ty2]

tc_type (HsPredTy pred)
  = tc_pred pred	`thenM` \ pred' ->
    returnM (mkPredTy pred')

tc_type full_ty@(HsForAllTy (Just tv_names) ctxt ty)
  = let
	kind_check = kcHsContext ctxt `thenM_` kcHsType ty
    in
    tcHsTyVars tv_names kind_check	$ \ tyvars ->
    mappM tc_pred ctxt			`thenM` \ theta ->
    tc_type ty				`thenM` \ tau ->
    returnM (mkSigmaTy tyvars theta tau)

tc_types arg_tys = mappM tc_type arg_tys
\end{code}

Help functions for type applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
tc_app :: RenamedHsType -> [RenamedHsType] -> TcM Type
tc_app (HsAppTy ty1 ty2) tys
  = tc_app ty1 (ty2:tys)

tc_app ty tys
  = tc_types tys			`thenM` \ arg_tys ->
    case ty of
	HsTyVar fun -> tc_fun_type fun arg_tys
	other	    -> tc_type ty		`thenM` \ fun_ty ->
		       returnM (mkAppTys fun_ty arg_tys)

-- (tc_fun_type ty arg_tys) returns (mkAppTys ty arg_tys)
-- But not quite; for synonyms it checks the correct arity, and builds a SynTy
-- 	hence the rather strange functionality.

tc_fun_type name arg_tys
  = tcLookup name			`thenM` \ thing ->
    case thing of
	ATyVar tv -> returnM (mkAppTys (mkTyVarTy tv) arg_tys)

	AGlobal (ATyCon tc) -> returnM (mkGenTyConApp tc arg_tys)

	other -> failWithTc (wrongThingErr "type constructor" thing name)
\end{code}


Contexts
~~~~~~~~
\begin{code}
tcHsPred pred = kc_pred pred `thenM_`  tc_pred pred
	-- Is happy with a partial application, e.g. (ST s)
	-- Used from TcDeriv

tc_pred assn@(HsClassP class_name tys)
  = addErrCtxt (appKindCtxt (ppr assn))	$
    tc_types tys			`thenM` \ arg_tys ->
    tcLookupGlobal class_name			`thenM` \ thing ->
    case thing of
	AClass clas -> returnM (ClassP clas arg_tys)
	other 	    -> failWithTc (wrongThingErr "class" (AGlobal thing) class_name)

tc_pred assn@(HsIParam name ty)
  = addErrCtxt (appKindCtxt (ppr assn))	$
    tc_type ty					`thenM` \ arg_ty ->
    returnM (IParam name arg_ty)
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
 = addSrcLoc src_loc			$ 
   tcHsSigType (FunSigCtxt v) ty	`thenM` \ sigma_tc_ty ->
   mkTcSig (mkLocalId v sigma_tc_ty) 	`thenM` \ sig -> 
   returnM sig

mkTcSig :: TcId -> TcM TcSigInfo
mkTcSig poly_id
  = 	-- Instantiate this type
	-- It's important to do this even though in the error-free case
	-- we could just split the sigma_tc_ty (since the tyvars don't
	-- unified with anything).  But in the case of an error, when
	-- the tyvars *do* get unified with something, we want to carry on
	-- typechecking the rest of the program with the function bound
	-- to a pristine type, namely sigma_tc_ty
   tcInstType SigTv (idType poly_id)		`thenM` \ (tyvars', theta', tau') ->

   getInstLoc SignatureOrigin			`thenM` \ inst_loc ->
   newMethod inst_loc poly_id
	     (mkTyVarTys tyvars')
	     theta' tau'			`thenM` \ inst ->
	-- We make a Method even if it's not overloaded; no harm
	-- But do not extend the LIE!  We're just making an Id.
	
   getSrcLocM					`thenM` \ src_loc ->
   returnM (TySigInfo poly_id tyvars' theta' tau' 
			  (instToId inst) [inst] src_loc)
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************


\begin{code}
hoistForAllTys :: Type -> Type
-- Used for user-written type signatures only
-- Move all the foralls and constraints to the top
-- e.g.  T -> forall a. a        ==>   forall a. T -> a
--	 T -> (?x::Int) -> Int   ==>   (?x::Int) -> T -> Int
--
-- Also: eliminate duplicate constraints.  These can show up
-- when hoisting constraints, notably implicit parameters.
--
-- We want to 'look through' type synonyms when doing this
-- so it's better done on the Type than the HsType

hoistForAllTys ty
  = let
	no_shadow_ty = deShadowTy ty
	-- Running over ty with an empty substitution gives it the
	-- no-shadowing property.  This is important.  For example:
	--	type Foo r = forall a. a -> r
	--	foo :: Foo (Foo ())
	-- Here the hoisting should give
	--	foo :: forall a a1. a -> a1 -> ()
	--
	-- What about type vars that are lexically in scope in the envt?
	-- We simply rely on them having a different unique to any
	-- binder in 'ty'.  Otherwise we'd have to slurp the in-scope-tyvars
	-- out of the envt, which is boring and (I think) not necessary.
    in
    case hoist no_shadow_ty of 
	(tvs, theta, body) -> mkForAllTys tvs (mkFunTys (nubBy tcEqType theta) body)
		-- The 'nubBy' eliminates duplicate constraints,
		-- notably implicit parameters
  where
    hoist ty
	| (tvs1, body_ty) <- tcSplitForAllTys ty,
	  not (null tvs1)
	= case hoist body_ty of
		(tvs2,theta,tau) -> (tvs1 ++ tvs2, theta, tau)

	| Just (arg, res) <- tcSplitFunTy_maybe ty
	= let
	      arg' = hoistForAllTys arg	-- Don't forget to apply hoist recursively
	  in				-- to the argument type
	  if (isPredTy arg') then
	    case hoist res of
		(tvs,theta,tau) -> (tvs, arg':theta, tau)
	  else
	     case hoist res of
		(tvs,theta,tau) -> (tvs, theta, mkFunTy arg' tau)

	| otherwise = ([], [], ty)
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
    pp_thing (ATcId _ _)  	  = ptext SLIT("Local identifier")
    pp_thing (AThing _) 	  = ptext SLIT("Utterly bogus")
\end{code}
