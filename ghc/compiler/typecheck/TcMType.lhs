%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Monadic type operations}

This module contains monadic operations over types that contain mutable type variables

\begin{code}
module TcMType (
  TcTyVar, TcKind, TcType, TcTauType, TcThetaType, TcRhoType, TcTyVarSet,

  --------------------------------
  -- Creating new mutable type variables
  newTyVar,
  newTyVarTy,		-- Kind -> NF_TcM TcType
  newTyVarTys,		-- Int -> Kind -> NF_TcM [TcType]
  newKindVar, newKindVars, newBoxityVar,

  --------------------------------
  -- Instantiation
  tcInstTyVar, tcInstTyVars,
  tcInstSigTyVars, tcInstType,
  tcSplitRhoTyM,

  --------------------------------
  -- Checking type validity
  Rank, UserTypeCtxt(..), checkValidType, pprUserTypeCtxt,
  SourceTyCtxt(..), checkValidTheta, 
  checkValidInstHead, instTypeErr,

  --------------------------------
  -- Unification
  unifyTauTy, unifyTauTyList, unifyTauTyLists, 
  unifyFunTy, unifyListTy, unifyTupleTy,
  unifyKind, unifyKinds, unifyOpenTypeKind,

  --------------------------------
  -- Zonking
  zonkTcTyVar, zonkTcTyVars, zonkTcTyVarsAndFV, zonkTcSigTyVars,
  zonkTcType, zonkTcTypes, zonkTcClassConstraints, zonkTcThetaType,
  zonkTcPredType, zonkTcTypeToType, zonkTcTyVarToTyVar, zonkKindEnv,

  ) where

#include "HsVersions.h"


-- friends:
import TypeRep		( Type(..), SourceType(..), TyNote(..),	 -- Friend; can see representation
			  Kind, TauType, ThetaType, 
			  openKindCon, typeCon
			) 
import TcType		( TcType, TcRhoType, TcThetaType, TcTauType, TcPredType,
			  TcTyVarSet, TcKind, TcTyVar, TyVarDetails(..),
			  tcEqType, tcCmpPred,
			  tcSplitRhoTy, tcSplitPredTy_maybe, tcSplitAppTy_maybe, 
			  tcSplitTyConApp_maybe, tcSplitFunTy_maybe, tcSplitForAllTys,
			  tcGetTyVar, tcIsTyVarTy, tcSplitSigmaTy, 
			  isUnLiftedType, isIPPred, isUserTyVar, isSkolemTyVar,

			  mkAppTy, mkTyVarTy, mkTyVarTys, mkFunTy, mkTyConApp,
			  tyVarsOfPred, getClassPredTys_maybe,

			  liftedTypeKind, unliftedTypeKind, openTypeKind, defaultKind, superKind,
			  superBoxity, liftedBoxity, hasMoreBoxityInfo, typeKind,
			  tyVarsOfType, tyVarsOfTypes, tidyOpenType, tidyOpenTypes, tidyOpenTyVar,
			  eqKind, isTypeKind,

			  isFFIArgumentTy, isFFIImportResultTy
			)
import Subst		( Subst, mkTopTyVarSubst, substTy )
import Class		( classArity, className )
import TyCon		( TyCon, mkPrimTyCon, isSynTyCon, isUnboxedTupleTyCon, 
			  isTupleTyCon, tyConArity, tupleTyConBoxity, tyConName )
import PrimRep		( PrimRep(VoidRep) )
import Var		( TyVar, varName, tyVarKind, tyVarName, isTyVar, mkTyVar,
			  isMutTyVar, mutTyVarDetails )

-- others:
import TcMonad          -- TcType, amongst others
import TysWiredIn	( voidTy, listTyCon, mkListTy, mkTupleTy )
import PrelNames	( cCallableClassKey, cReturnableClassKey, hasKey )
import ForeignCall	( Safety(..) )
import FunDeps		( grow )
import PprType		( pprPred, pprSourceType, pprTheta, pprClassPred )
import Name		( Name, NamedThing(..), setNameUnique, mkSysLocalName,
			  mkLocalName, mkDerivedTyConOcc, isSystemName
			)
import VarSet
import BasicTypes	( Boxity, Arity, isBoxed )
import CmdLineOpts	( dopt, DynFlag(..) )
import Unique		( Uniquable(..) )
import SrcLoc		( noSrcLoc )
import Util		( nOfThem, isSingleton, equalLength )
import ListSetOps	( removeDups )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{New type variables}
%*									*
%************************************************************************

\begin{code}
newTyVar :: Kind -> NF_TcM TcTyVar
newTyVar kind
  = tcGetUnique 	`thenNF_Tc` \ uniq ->
    tcNewMutTyVar (mkSysLocalName uniq SLIT("t")) kind VanillaTv

newTyVarTy  :: Kind -> NF_TcM TcType
newTyVarTy kind
  = newTyVar kind	`thenNF_Tc` \ tc_tyvar ->
    returnNF_Tc (TyVarTy tc_tyvar)

newTyVarTys :: Int -> Kind -> NF_TcM [TcType]
newTyVarTys n kind = mapNF_Tc newTyVarTy (nOfThem n kind)

newKindVar :: NF_TcM TcKind
newKindVar
  = tcGetUnique 							`thenNF_Tc` \ uniq ->
    tcNewMutTyVar (mkSysLocalName uniq SLIT("k")) superKind VanillaTv	`thenNF_Tc` \ kv ->
    returnNF_Tc (TyVarTy kv)

newKindVars :: Int -> NF_TcM [TcKind]
newKindVars n = mapNF_Tc (\ _ -> newKindVar) (nOfThem n ())

newBoxityVar :: NF_TcM TcKind
newBoxityVar
  = tcGetUnique 							  `thenNF_Tc` \ uniq ->
    tcNewMutTyVar (mkSysLocalName uniq SLIT("bx")) superBoxity VanillaTv  `thenNF_Tc` \ kv ->
    returnNF_Tc (TyVarTy kv)
\end{code}


%************************************************************************
%*									*
\subsection{Type instantiation}
%*									*
%************************************************************************

I don't understand why this is needed
An old comments says "No need for tcSplitForAllTyM because a type 
	variable can't be instantiated to a for-all type"
But the same is true of rho types!

\begin{code}
tcSplitRhoTyM :: TcType -> NF_TcM (TcThetaType, TcType)
tcSplitRhoTyM t
  = go t t []
 where
	-- A type variable is never instantiated to a dictionary type,
	-- so we don't need to do a tcReadVar on the "arg".
    go syn_t (FunTy arg res) ts = case tcSplitPredTy_maybe arg of
					Just pair -> go res res (pair:ts)
					Nothing   -> returnNF_Tc (reverse ts, syn_t)
    go syn_t (NoteTy n t)    ts = go syn_t t ts
    go syn_t (TyVarTy tv)    ts = getTcTyVar tv		`thenNF_Tc` \ maybe_ty ->
				  case maybe_ty of
				    Just ty | not (tcIsTyVarTy ty) -> go syn_t ty ts
				    other			   -> returnNF_Tc (reverse ts, syn_t)
    go syn_t (UsageTy _ t)   ts = go syn_t t ts
    go syn_t t		     ts = returnNF_Tc (reverse ts, syn_t)
\end{code}


%************************************************************************
%*									*
\subsection{Type instantiation}
%*									*
%************************************************************************

Instantiating a bunch of type variables

\begin{code}
tcInstTyVars :: [TyVar] 
	     -> NF_TcM ([TcTyVar], [TcType], Subst)

tcInstTyVars tyvars
  = mapNF_Tc tcInstTyVar tyvars	`thenNF_Tc` \ tc_tyvars ->
    let
	tys = mkTyVarTys tc_tyvars
    in
    returnNF_Tc (tc_tyvars, tys, mkTopTyVarSubst tyvars tys)
		-- Since the tyvars are freshly made,
		-- they cannot possibly be captured by
		-- any existing for-alls.  Hence mkTopTyVarSubst

tcInstTyVar tyvar
  = tcGetUnique 		`thenNF_Tc` \ uniq ->
    let
	name = setNameUnique (tyVarName tyvar) uniq
	-- Note that we don't change the print-name
	-- This won't confuse the type checker but there's a chance
	-- that two different tyvars will print the same way 
	-- in an error message.  -dppr-debug will show up the difference
	-- Better watch out for this.  If worst comes to worst, just
	-- use mkSysLocalName.
    in
    tcNewMutTyVar name (tyVarKind tyvar) VanillaTv

tcInstSigTyVars :: TyVarDetails -> [TyVar] -> NF_TcM [TcTyVar]
tcInstSigTyVars details tyvars	-- Very similar to tcInstTyVar
  = tcGetUniques 	`thenNF_Tc` \ uniqs ->
    listTc [ ASSERT( not (kind `eqKind` openTypeKind) )	-- Shouldn't happen
	     tcNewMutTyVar name kind details
	   | (tyvar, uniq) <- tyvars `zip` uniqs,
	     let name = setNameUnique (tyVarName tyvar) uniq, 
	     let kind = tyVarKind tyvar
	   ]
\end{code}

@tcInstType@ instantiates the outer-level for-alls of a TcType with
fresh type variables, splits off the dictionary part, and returns the results.

\begin{code}
tcInstType :: TcType -> NF_TcM ([TcTyVar], TcThetaType, TcType)
tcInstType ty
  = case tcSplitForAllTys ty of
	([],     rho) -> 	-- There may be overloading but no type variables;
				-- 	(?x :: Int) => Int -> Int
			 let
			   (theta, tau) = tcSplitRhoTy rho	-- Used to be tcSplitRhoTyM
			 in
			 returnNF_Tc ([], theta, tau)

	(tyvars, rho) -> tcInstTyVars tyvars			`thenNF_Tc` \ (tyvars', _, tenv)  ->
			 let
			   (theta, tau) = tcSplitRhoTy (substTy tenv rho)	-- Used to be tcSplitRhoTyM
			 in
			 returnNF_Tc (tyvars', theta, tau)
\end{code}



%************************************************************************
%*									*
\subsection{Putting and getting  mutable type variables}
%*									*
%************************************************************************

\begin{code}
putTcTyVar :: TcTyVar -> TcType -> NF_TcM TcType
getTcTyVar :: TcTyVar -> NF_TcM (Maybe TcType)
\end{code}

Putting is easy:

\begin{code}
putTcTyVar tyvar ty 
  | not (isMutTyVar tyvar)
  = pprTrace "putTcTyVar" (ppr tyvar) $
    returnNF_Tc ty

  | otherwise
  = ASSERT( isMutTyVar tyvar )
    UASSERT2( not (isUTy ty), ppr tyvar <+> ppr ty )
    tcWriteMutTyVar tyvar (Just ty)	`thenNF_Tc_`
    returnNF_Tc ty
\end{code}

Getting is more interesting.  The easy thing to do is just to read, thus:

\begin{verbatim}
getTcTyVar tyvar = tcReadMutTyVar tyvar
\end{verbatim}

But it's more fun to short out indirections on the way: If this
version returns a TyVar, then that TyVar is unbound.  If it returns
any other type, then there might be bound TyVars embedded inside it.

We return Nothing iff the original box was unbound.

\begin{code}
getTcTyVar tyvar
  | not (isMutTyVar tyvar)
  = pprTrace "getTcTyVar" (ppr tyvar) $
    returnNF_Tc (Just (mkTyVarTy tyvar))

  | otherwise
  = ASSERT2( isMutTyVar tyvar, ppr tyvar )
    tcReadMutTyVar tyvar				`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty -> short_out ty				`thenNF_Tc` \ ty' ->
		   tcWriteMutTyVar tyvar (Just ty')	`thenNF_Tc_`
		   returnNF_Tc (Just ty')

	Nothing	   -> returnNF_Tc Nothing

short_out :: TcType -> NF_TcM TcType
short_out ty@(TyVarTy tyvar)
  | not (isMutTyVar tyvar)
  = returnNF_Tc ty

  | otherwise
  = tcReadMutTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> short_out ty' 			`thenNF_Tc` \ ty' ->
		    tcWriteMutTyVar tyvar (Just ty')	`thenNF_Tc_`
		    returnNF_Tc ty'

	other    -> returnNF_Tc ty

short_out other_ty = returnNF_Tc other_ty
\end{code}


%************************************************************************
%*									*
\subsection{Zonking -- the exernal interfaces}
%*									*
%************************************************************************

-----------------  Type variables

\begin{code}
zonkTcTyVars :: [TcTyVar] -> NF_TcM [TcType]
zonkTcTyVars tyvars = mapNF_Tc zonkTcTyVar tyvars

zonkTcTyVarsAndFV :: [TcTyVar] -> NF_TcM TcTyVarSet
zonkTcTyVarsAndFV tyvars = mapNF_Tc zonkTcTyVar tyvars	`thenNF_Tc` \ tys ->
			   returnNF_Tc (tyVarsOfTypes tys)

zonkTcTyVar :: TcTyVar -> NF_TcM TcType
zonkTcTyVar tyvar = zonkTyVar (\ tv -> returnNF_Tc (TyVarTy tv)) tyvar

zonkTcSigTyVars :: [TcTyVar] -> NF_TcM [TcTyVar]
-- This guy is to zonk the tyvars we're about to feed into tcSimplify
-- Usually this job is done by checkSigTyVars, but in a couple of places
-- that is overkill, so we use this simpler chap
zonkTcSigTyVars tyvars
  = zonkTcTyVars tyvars	`thenNF_Tc` \ tys ->
    returnNF_Tc (map (tcGetTyVar "zonkTcSigTyVars") tys)
\end{code}

-----------------  Types

\begin{code}
zonkTcType :: TcType -> NF_TcM TcType
zonkTcType ty = zonkType (\ tv -> returnNF_Tc (TyVarTy tv)) ty

zonkTcTypes :: [TcType] -> NF_TcM [TcType]
zonkTcTypes tys = mapNF_Tc zonkTcType tys

zonkTcClassConstraints cts = mapNF_Tc zonk cts
    where zonk (clas, tys)
	    = zonkTcTypes tys	`thenNF_Tc` \ new_tys ->
	      returnNF_Tc (clas, new_tys)

zonkTcThetaType :: TcThetaType -> NF_TcM TcThetaType
zonkTcThetaType theta = mapNF_Tc zonkTcPredType theta

zonkTcPredType :: TcPredType -> NF_TcM TcPredType
zonkTcPredType (ClassP c ts) =
    zonkTcTypes ts	`thenNF_Tc` \ new_ts ->
    returnNF_Tc (ClassP c new_ts)
zonkTcPredType (IParam n t) =
    zonkTcType t	`thenNF_Tc` \ new_t ->
    returnNF_Tc (IParam n new_t)
\end{code}

-------------------  These ...ToType, ...ToKind versions
		     are used at the end of type checking

\begin{code}
zonkKindEnv :: [(Name, TcKind)] -> NF_TcM [(Name, Kind)]
zonkKindEnv pairs 
  = mapNF_Tc zonk_it pairs
 where
    zonk_it (name, tc_kind) = zonkType zonk_unbound_kind_var tc_kind `thenNF_Tc` \ kind ->
			      returnNF_Tc (name, kind)

	-- When zonking a kind, we want to
	--	zonk a *kind* variable to (Type *)
	--	zonk a *boxity* variable to *
    zonk_unbound_kind_var kv | tyVarKind kv `eqKind` superKind   = putTcTyVar kv liftedTypeKind
			     | tyVarKind kv `eqKind` superBoxity = putTcTyVar kv liftedBoxity
			     | otherwise		 	 = pprPanic "zonkKindEnv" (ppr kv)
			
zonkTcTypeToType :: TcType -> NF_TcM Type
zonkTcTypeToType ty = zonkType zonk_unbound_tyvar ty
  where
	-- Zonk a mutable but unbound type variable to
	--	Void		if it has kind Lifted
	--	:Void		otherwise
	-- We know it's unbound even though we don't carry an environment,
	-- because at the binding site for a type variable we bind the
	-- mutable tyvar to a fresh immutable one.  So the mutable store
	-- plays the role of an environment.  If we come across a mutable
	-- type variable that isn't so bound, it must be completely free.
    zonk_unbound_tyvar tv
	| kind `eqKind` liftedTypeKind || kind `eqKind` openTypeKind
	= putTcTyVar tv voidTy	-- Just to avoid creating a new tycon in
				-- this vastly common case
	| otherwise
	= putTcTyVar tv (TyConApp (mk_void_tycon tv kind) [])
	where
	  kind = tyVarKind tv

    mk_void_tycon tv kind	-- Make a new TyCon with the same kind as the 
				-- type variable tv.  Same name too, apart from
				-- making it start with a colon (sigh)
		-- I dread to think what will happen if this gets out into an 
		-- interface file.  Catastrophe likely.  Major sigh.
	= pprTrace "Urk! Inventing strangely-kinded void TyCon" (ppr tc_name) $
	  mkPrimTyCon tc_name kind 0 [] VoidRep
	where
	  tc_name = mkLocalName (getUnique tv) (mkDerivedTyConOcc (getOccName tv)) noSrcLoc

-- zonkTcTyVarToTyVar is applied to the *binding* occurrence 
-- of a type variable, at the *end* of type checking.  It changes
-- the *mutable* type variable into an *immutable* one.
-- 
-- It does this by making an immutable version of tv and binds tv to it.
-- Now any bound occurences of the original type variable will get 
-- zonked to the immutable version.

zonkTcTyVarToTyVar :: TcTyVar -> NF_TcM TyVar
zonkTcTyVarToTyVar tv
  = let
		-- Make an immutable version, defaulting 
		-- the kind to lifted if necessary
	immut_tv    = mkTyVar (tyVarName tv) (defaultKind (tyVarKind tv))
	immut_tv_ty = mkTyVarTy immut_tv

        zap tv = putTcTyVar tv immut_tv_ty
		-- Bind the mutable version to the immutable one
    in 
	-- If the type variable is mutable, then bind it to immut_tv_ty
	-- so that all other occurrences of the tyvar will get zapped too
    zonkTyVar zap tv		`thenNF_Tc` \ ty2 ->

    WARN( not (immut_tv_ty `tcEqType` ty2), ppr tv $$ ppr immut_tv $$ ppr ty2 )

    returnNF_Tc immut_tv
\end{code}


%************************************************************************
%*									*
\subsection{Zonking -- the main work-horses: zonkType, zonkTyVar}
%*									*
%*		For internal use only!					*
%*									*
%************************************************************************

\begin{code}
-- zonkType is used for Kinds as well

-- For unbound, mutable tyvars, zonkType uses the function given to it
-- For tyvars bound at a for-all, zonkType zonks them to an immutable
--	type variable and zonks the kind too

zonkType :: (TcTyVar -> NF_TcM Type) 	-- What to do with unbound mutable type variables
					-- see zonkTcType, and zonkTcTypeToType
	 -> TcType
	 -> NF_TcM Type
zonkType unbound_var_fn ty
  = go ty
  where
    go (TyConApp tycon tys)	  = mapNF_Tc go tys	`thenNF_Tc` \ tys' ->
				    returnNF_Tc (TyConApp tycon tys')

    go (NoteTy (SynNote ty1) ty2) = go ty1		`thenNF_Tc` \ ty1' ->
				    go ty2		`thenNF_Tc` \ ty2' ->
				    returnNF_Tc (NoteTy (SynNote ty1') ty2')

    go (NoteTy (FTVNote _) ty2)   = go ty2	-- Discard free-tyvar annotations

    go (SourceTy p)		  = go_pred p		`thenNF_Tc` \ p' ->
				    returnNF_Tc (SourceTy p')

    go (FunTy arg res)      	  = go arg		`thenNF_Tc` \ arg' ->
				    go res		`thenNF_Tc` \ res' ->
				    returnNF_Tc (FunTy arg' res')
 
    go (AppTy fun arg)	 	  = go fun		`thenNF_Tc` \ fun' ->
				    go arg		`thenNF_Tc` \ arg' ->
				    returnNF_Tc (mkAppTy fun' arg')

    go (UsageTy u ty)             = go u                `thenNF_Tc` \ u'  ->
                                    go ty               `thenNF_Tc` \ ty' ->
                                    returnNF_Tc (UsageTy u' ty')

	-- The two interesting cases!
    go (TyVarTy tyvar)     = zonkTyVar unbound_var_fn tyvar

    go (ForAllTy tyvar ty) = zonkTcTyVarToTyVar tyvar	`thenNF_Tc` \ tyvar' ->
			     go ty			`thenNF_Tc` \ ty' ->
			     returnNF_Tc (ForAllTy tyvar' ty')

    go_pred (ClassP c tys) = mapNF_Tc go tys	`thenNF_Tc` \ tys' ->
			     returnNF_Tc (ClassP c tys')
    go_pred (NType tc tys) = mapNF_Tc go tys	`thenNF_Tc` \ tys' ->
			     returnNF_Tc (NType tc tys')
    go_pred (IParam n ty) = go ty		`thenNF_Tc` \ ty' ->
			    returnNF_Tc (IParam n ty')

zonkTyVar :: (TcTyVar -> NF_TcM Type)		-- What to do for an unbound mutable variable
	  -> TcTyVar -> NF_TcM TcType
zonkTyVar unbound_var_fn tyvar 
  | not (isMutTyVar tyvar)	-- Not a mutable tyvar.  This can happen when
				-- zonking a forall type, when the bound type variable
				-- needn't be mutable
  = ASSERT( isTyVar tyvar )		-- Should not be any immutable kind vars
    returnNF_Tc (TyVarTy tyvar)

  | otherwise
  =  getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
     case maybe_ty of
	  Nothing	-> unbound_var_fn tyvar			-- Mutable and unbound
	  Just other_ty	-> zonkType unbound_var_fn other_ty	-- Bound
\end{code}



%************************************************************************
%*									*
\subsection{Checking a user type}
%*									*
%************************************************************************

When dealing with a user-written type, we first translate it from an HsType
to a Type, performing kind checking, and then check various things that should 
be true about it.  We don't want to perform these checks at the same time
as the initial translation because (a) they are unnecessary for interface-file
types and (b) when checking a mutually recursive group of type and class decls,
we can't "look" at the tycons/classes yet.  Also, the checks are are rather
diverse, and used to really mess up the other code.

One thing we check for is 'rank'.  

	Rank 0: 	monotypes (no foralls)
	Rank 1:		foralls at the front only, Rank 0 inside
	Rank 2:		foralls at the front, Rank 1 on left of fn arrow,

	basic ::= tyvar | T basic ... basic

	r2  ::= forall tvs. cxt => r2a
	r2a ::= r1 -> r2a | basic
	r1  ::= forall tvs. cxt => r0
	r0  ::= r0 -> r0 | basic
	
Another thing is to check that type synonyms are saturated. 
This might not necessarily show up in kind checking.
	type A i = i
	data T k = MkT (k Int)
	f :: T A	-- BAD!

	
\begin{code}
data UserTypeCtxt 
  = FunSigCtxt Name	-- Function type signature
  | ExprSigCtxt		-- Expression type signature
  | ConArgCtxt Name	-- Data constructor argument
  | TySynCtxt Name	-- RHS of a type synonym decl
  | GenPatCtxt		-- Pattern in generic decl
			-- 	f{| a+b |} (Inl x) = ...
  | PatSigCtxt		-- Type sig in pattern
			-- 	f (x::t) = ...
  | ResSigCtxt		-- Result type sig
			-- 	f x :: t = ....
  | ForSigCtxt Name	-- Foreign inport or export signature
  | RuleSigCtxt Name 	-- Signature on a forall'd variable in a RULE

-- Notes re TySynCtxt
-- We allow type synonyms that aren't types; e.g.  type List = []
--
-- If the RHS mentions tyvars that aren't in scope, we'll 
-- quantify over them:
--	e.g. 	type T = a->a
-- will become	type T = forall a. a->a
--
-- With gla-exts that's right, but for H98 we should complain. 


pprUserTypeCtxt (FunSigCtxt n) 	= ptext SLIT("the type signature for") <+> quotes (ppr n)
pprUserTypeCtxt ExprSigCtxt    	= ptext SLIT("an expression type signature")
pprUserTypeCtxt (ConArgCtxt c) 	= ptext SLIT("the type of constructor") <+> quotes (ppr c)
pprUserTypeCtxt (TySynCtxt c)  	= ptext SLIT("the RHS of a type synonym declaration") <+> quotes (ppr c)
pprUserTypeCtxt GenPatCtxt     	= ptext SLIT("the type pattern of a generic definition")
pprUserTypeCtxt PatSigCtxt     	= ptext SLIT("a pattern type signature")
pprUserTypeCtxt ResSigCtxt     	= ptext SLIT("a result type signature")
pprUserTypeCtxt (ForSigCtxt n) 	= ptext SLIT("the foreign signature for") <+> quotes (ppr n)
pprUserTypeCtxt (RuleSigCtxt n) = ptext SLIT("the type signature on") <+> quotes (ppr n)
\end{code}

\begin{code}
checkValidType :: UserTypeCtxt -> Type -> TcM ()
-- Checks that the type is valid for the given context
checkValidType ctxt ty
  = doptsTc Opt_GlasgowExts	`thenNF_Tc` \ gla_exts ->
    let 
	rank = case ctxt of
		 GenPatCtxt		  -> 0
		 PatSigCtxt		  -> 0
		 ResSigCtxt		  -> 0
		 ExprSigCtxt 	          -> 1
		 FunSigCtxt _ | gla_exts  -> 2
			      | otherwise -> 1
		 ConArgCtxt _ | gla_exts  -> 2	-- We are given the type of the entire
			      | otherwise -> 1	-- constructor; hence rank 1 is ok
		 TySynCtxt _  | gla_exts  -> 1
			      | otherwise -> 0
		 ForSigCtxt _		  -> 1
		 RuleSigCtxt _		  -> 1

	actual_kind = typeKind ty

	actual_kind_is_lifted = actual_kind `eqKind` liftedTypeKind

	kind_ok = case ctxt of
			TySynCtxt _  -> True	-- Any kind will do
			GenPatCtxt   -> actual_kind_is_lifted
			ForSigCtxt _ -> actual_kind_is_lifted
			other	     -> isTypeKind actual_kind
    in
    tcAddErrCtxt (checkTypeCtxt ctxt ty)	$

	-- Check that the thing has kind Type, and is lifted if necessary
    checkTc kind_ok (kindErr actual_kind)	`thenTc_`

	-- Check the internal validity of the type itself
    check_poly_type rank ty


checkTypeCtxt ctxt ty
  = vcat [ptext SLIT("In the type:") <+> ppr_ty ty,
	  ptext SLIT("While checking") <+> pprUserTypeCtxt ctxt ]

	-- Hack alert.  If there are no tyvars, (ppr sigma_ty) will print
	-- something strange like {Eq k} -> k -> k, because there is no
	-- ForAll at the top of the type.  Since this is going to the user
	-- we want it to look like a proper Haskell type even then; hence the hack
	-- 
	-- This shows up in the complaint about
	--	case C a where
	--	  op :: Eq a => a -> a
ppr_ty ty | null forall_tvs && not (null theta) = pprTheta theta <+> ptext SLIT("=>") <+> ppr tau
          | otherwise	 		     = ppr ty
          where
	    (forall_tvs, theta, tau) = tcSplitSigmaTy ty
\end{code}


\begin{code}
type Rank = Int
check_poly_type :: Rank -> Type -> TcM ()
check_poly_type rank ty 
  | rank == 0 
  = check_tau_type 0 False ty
  | otherwise	-- rank > 0
  = let
	(tvs, theta, tau) = tcSplitSigmaTy ty
    in
    check_valid_theta SigmaCtxt theta	`thenTc_`
    check_tau_type (rank-1) False tau	`thenTc_`
    checkAmbiguity tvs theta tau

----------------------------------------
check_arg_type :: Type -> TcM ()
-- The sort of type that can instantiate a type variable,
-- or be the argument of a type constructor.
-- Not an unboxed tuple, not a forall.
-- Other unboxed types are very occasionally allowed as type
-- arguments depending on the kind of the type constructor
-- 
-- For example, we want to reject things like:
--
--	instance Ord a => Ord (forall s. T s a)
-- and
--	g :: T s (forall b.b)
--
-- NB: unboxed tuples can have polymorphic or unboxed args.
--     This happens in the workers for functions returning
--     product types with polymorphic components.
--     But not in user code
-- 
-- Question: what about nested unboxed tuples?
-- 	     Currently rejected.
check_arg_type ty 
  = check_tau_type 0 False ty	`thenTc_` 
    checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty)

----------------------------------------
check_tau_type :: Rank -> Bool -> Type -> TcM ()
-- Rank is allowed rank for function args
-- No foralls otherwise
-- Bool is True iff unboxed tuple are allowed here

check_tau_type rank ubx_tup_ok ty@(UsageTy _ _)  = failWithTc (usageTyErr ty)
check_tau_type rank ubx_tup_ok ty@(ForAllTy _ _) = failWithTc (forAllTyErr ty)
check_tau_type rank ubx_tup_ok (SourceTy sty)    = getDOptsTc		`thenNF_Tc` \ dflags ->
						   check_source_ty dflags TypeCtxt sty
check_tau_type rank ubx_tup_ok (TyVarTy _)       = returnTc ()
check_tau_type rank ubx_tup_ok ty@(FunTy arg_ty res_ty)
  = check_poly_type rank      arg_ty	`thenTc_`
    check_tau_type  rank True res_ty

check_tau_type rank ubx_tup_ok (AppTy ty1 ty2)
  = check_arg_type ty1 `thenTc_` check_arg_type ty2

check_tau_type rank ubx_tup_ok (NoteTy note ty)
  = check_note note `thenTc_` check_tau_type rank ubx_tup_ok ty

check_tau_type rank ubx_tup_ok ty@(TyConApp tc tys)
  | isSynTyCon tc
  = checkTc syn_arity_ok arity_msg	`thenTc_`
    mapTc_ check_arg_type tys
    
  | isUnboxedTupleTyCon tc
  = checkTc ubx_tup_ok ubx_tup_msg	`thenTc_`
    mapTc_ (check_tau_type 0 True) tys		-- Args are allowed to be unlifted, or
						-- more unboxed tuples, so can't use check_arg_ty

  | otherwise
  = mapTc_ check_arg_type tys

  where
    syn_arity_ok = tc_arity <= n_args
		-- It's OK to have an *over-applied* type synonym
		--	data Tree a b = ...
		--	type Foo a = Tree [a]
		--	f :: Foo a b -> ...
    n_args    = length tys
    tc_arity  = tyConArity tc

    arity_msg   = arityErr "Type synonym" (tyConName tc) tc_arity n_args
    ubx_tup_msg = ubxArgTyErr ty

----------------------------------------
check_note (FTVNote _)  = returnTc ()
check_note (SynNote ty) = check_tau_type 0 False ty
\end{code}

Check for ambiguity
~~~~~~~~~~~~~~~~~~~
	  forall V. P => tau
is ambiguous if P contains generic variables
(i.e. one of the Vs) that are not mentioned in tau

However, we need to take account of functional dependencies
when we speak of 'mentioned in tau'.  Example:
	class C a b | a -> b where ...
Then the type
	forall x y. (C x y) => x
is not ambiguous because x is mentioned and x determines y

NOTE: In addition, GHC insists that at least one type variable
in each constraint is in V.  So we disallow a type like
	forall a. Eq b => b -> b
even in a scope where b is in scope.
This is the is_free test below.

NB; the ambiguity check is only used for *user* types, not for types
coming from inteface files.  The latter can legitimately have
ambiguous types. Example

   class S a where s :: a -> (Int,Int)
   instance S Char where s _ = (1,1)
   f:: S a => [a] -> Int -> (Int,Int)
   f (_::[a]) x = (a*x,b)
	where (a,b) = s (undefined::a)

Here the worker for f gets the type
	fw :: forall a. S a => Int -> (# Int, Int #)

If the list of tv_names is empty, we have a monotype, and then we
don't need to check for ambiguity either, because the test can't fail
(see is_ambig).

\begin{code}
checkAmbiguity :: [TyVar] -> ThetaType -> TauType -> TcM ()
checkAmbiguity forall_tyvars theta tau
  = mapTc_ check_pred theta	`thenTc_`
    returnTc ()
  where
    tau_vars	      = tyVarsOfType tau
    extended_tau_vars = grow theta tau_vars

    is_ambig ct_var   = (ct_var `elem` forall_tyvars) &&
		        not (ct_var `elemVarSet` extended_tau_vars)
    is_free ct_var    = not (ct_var `elem` forall_tyvars)
    
    check_pred pred = checkTc (not any_ambig)                 (ambigErr pred) `thenTc_`
	    	      checkTc (isIPPred pred || not all_free) (freeErr  pred)
             where 
	    	ct_vars	  = varSetElems (tyVarsOfPred pred)
	    	all_free  = all is_free ct_vars
	    	any_ambig = any is_ambig ct_vars
\end{code}

\begin{code}
ambigErr pred
  = sep [ptext SLIT("Ambiguous constraint") <+> quotes (pprPred pred),
	 nest 4 (ptext SLIT("At least one of the forall'd type variables mentioned by the constraint") $$
		 ptext SLIT("must be reachable from the type after the =>"))]


freeErr pred
  = sep [ptext SLIT("All of the type variables in the constraint") <+> quotes (pprPred pred) <+>
		   ptext SLIT("are already in scope"),
	 nest 4 (ptext SLIT("At least one must be universally quantified here"))
    ]

forAllTyErr     ty = ptext SLIT("Illegal polymorphic type:") <+> ppr_ty ty
usageTyErr      ty = ptext SLIT("Illegal usage type:") <+> ppr_ty ty
unliftedArgErr  ty = ptext SLIT("Illegal unlifted type argument:") <+> ppr_ty ty
ubxArgTyErr     ty = ptext SLIT("Illegal unboxed tuple type as function argument:") <+> ppr_ty ty
kindErr kind       = ptext SLIT("Expecting an ordinary type, but found a type of kind") <+> ppr kind
\end{code}

%************************************************************************
%*									*
\subsection{Checking a theta or source type}
%*									*
%************************************************************************

\begin{code}
data SourceTyCtxt
  = ClassSCCtxt Name	-- Superclasses of clas
  | SigmaCtxt		-- Context of a normal for-all type
  | DataTyCtxt Name	-- Context of a data decl
  | TypeCtxt 		-- Source type in an ordinary type
  | InstThetaCtxt	-- Context of an instance decl
  | InstHeadCtxt	-- Head of an instance decl
		
pprSourceTyCtxt (ClassSCCtxt c) = ptext SLIT("the super-classes of class") <+> quotes (ppr c)
pprSourceTyCtxt SigmaCtxt       = ptext SLIT("the context of a polymorphic type")
pprSourceTyCtxt (DataTyCtxt tc) = ptext SLIT("the context of the data type declaration for") <+> quotes (ppr tc)
pprSourceTyCtxt InstThetaCtxt   = ptext SLIT("the context of an instance declaration")
pprSourceTyCtxt InstHeadCtxt    = ptext SLIT("the head of an instance declaration")
pprSourceTyCtxt TypeCtxt        = ptext SLIT("the context of a type")
\end{code}

\begin{code}
checkValidTheta :: SourceTyCtxt -> ThetaType -> TcM ()
checkValidTheta ctxt theta 
  = tcAddErrCtxt (checkThetaCtxt ctxt theta) (check_valid_theta ctxt theta)

-------------------------
check_valid_theta ctxt []
  = returnTc ()
check_valid_theta ctxt theta
  = getDOptsTc					`thenNF_Tc` \ dflags ->
    warnTc (not (null dups)) (dupPredWarn dups)	`thenNF_Tc_`
    mapTc_ (check_source_ty dflags ctxt) theta
  where
    (_,dups) = removeDups tcCmpPred theta

-------------------------
check_source_ty dflags ctxt pred@(ClassP cls tys)
  = 	-- Class predicates are valid in all contexts
    mapTc_ check_arg_type tys			`thenTc_`
    checkTc (arity == n_tys) arity_err		`thenTc_`
    checkTc (all tyvar_head tys || arby_preds_ok) (predTyVarErr pred)

  where
    class_name = className cls
    arity      = classArity cls
    n_tys      = length tys
    arity_err  = arityErr "Class" class_name arity n_tys

    arby_preds_ok = case ctxt of
			InstHeadCtxt  -> True	-- We check for instance-head formation
						-- in checkValidInstHead
			InstThetaCtxt -> dopt Opt_AllowUndecidableInstances dflags
			other	      -> dopt Opt_GlasgowExts               dflags

check_source_ty dflags SigmaCtxt (IParam name ty) = check_arg_type ty
	-- Implicit parameters only allows in type
	-- signatures; not in instance decls, superclasses etc
	-- The reason for not allowing implicit params in instances is a bit subtle
	-- If we allowed	instance (?x::Int, Eq a) => Foo [a] where ...
	-- then when we saw (e :: (?x::Int) => t) it would be unclear how to 
	-- discharge all the potential usas of the ?x in e.   For example, a
	-- constraint Foo [Int] might come out of e,and applying the
	-- instance decl would show up two uses of ?x.

check_source_ty dflags TypeCtxt  (NType tc tys)   = mapTc_ check_arg_type tys

-- Catch-all
check_source_ty dflags ctxt sty = failWithTc (badSourceTyErr sty)

-------------------------
tyvar_head ty			-- Haskell 98 allows predicates of form 
  | tcIsTyVarTy ty = True	-- 	C (a ty1 .. tyn)
  | otherwise			-- where a is a type variable
  = case tcSplitAppTy_maybe ty of
	Just (ty, _) -> tyvar_head ty
	Nothing	     -> False
\end{code}

\begin{code}
badSourceTyErr sty = ptext SLIT("Illegal constraint") <+> pprSourceType sty
predTyVarErr pred  = ptext SLIT("Non-type variables in constraint:") <+> pprPred pred
dupPredWarn dups   = ptext SLIT("Duplicate constraint(s):") <+> pprWithCommas pprPred (map head dups)

checkThetaCtxt ctxt theta
  = vcat [ptext SLIT("In the context:") <+> pprTheta theta,
	  ptext SLIT("While checking") <+> pprSourceTyCtxt ctxt ]
\end{code}


%************************************************************************
%*									*
\subsection{Checking for a decent instance head type}
%*									*
%************************************************************************

@checkValidInstHead@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere). In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.

\begin{code}
checkValidInstHead :: Type -> TcM ()

checkValidInstHead ty	-- Should be a source type
  = case tcSplitPredTy_maybe ty of {
	Nothing -> failWithTc (instTypeErr (ppr ty) empty) ;
	Just pred -> 

    case getClassPredTys_maybe pred of {
	Nothing -> failWithTc (instTypeErr (pprPred pred) empty) ;
        Just (clas,tys) ->

    getDOptsTc					`thenNF_Tc` \ dflags ->
    mapTc_ check_arg_type tys			`thenTc_`
    check_inst_head dflags clas tys
    }}

check_inst_head dflags clas tys
  |	-- CCALL CHECK
	-- A user declaration of a CCallable/CReturnable instance
	-- must be for a "boxed primitive" type.
        (clas `hasKey` cCallableClassKey   
            && not (ccallable_type first_ty)) 
  ||    (clas `hasKey` cReturnableClassKey 
            && not (creturnable_type first_ty))
  = failWithTc (nonBoxedPrimCCallErr clas first_ty)

	-- If GlasgowExts then check at least one isn't a type variable
  | dopt Opt_GlasgowExts dflags
  = check_tyvars dflags clas tys

	-- WITH HASKELL 1.4, MUST HAVE C (T a b c)
  | isSingleton tys,
    Just (tycon, arg_tys) <- tcSplitTyConApp_maybe first_ty,
    not (isSynTyCon tycon), 		-- ...but not a synonym
    all tcIsTyVarTy arg_tys,  		-- Applied to type variables
    equalLength (varSetElems (tyVarsOfTypes arg_tys)) arg_tys
          -- This last condition checks that all the type variables are distinct
  = returnTc ()

  | otherwise
  = failWithTc (instTypeErr (pprClassPred clas tys) head_shape_msg)

  where
    (first_ty : _)       = tys

    ccallable_type   ty = isFFIArgumentTy dflags PlayRisky ty
    creturnable_type ty = isFFIImportResultTy dflags ty
	
    head_shape_msg = parens (text "The instance type must be of form (T a b c)" $$
			     text "where T is not a synonym, and a,b,c are distinct type variables")

check_tyvars dflags clas tys
   	-- Check that at least one isn't a type variable
	-- unless -fallow-undecideable-instances
  | dopt Opt_AllowUndecidableInstances dflags = returnTc ()
  | not (all tcIsTyVarTy tys)	  	      = returnTc ()
  | otherwise 				      = failWithTc (instTypeErr (pprClassPred clas tys) msg)
  where
    msg =  parens (ptext SLIT("There must be at least one non-type-variable in the instance head")
	        $$ ptext SLIT("Use -fallow-undecidable-instances to lift this restriction"))
\end{code}

\begin{code}
instTypeErr pp_ty msg
  = sep [ptext SLIT("Illegal instance declaration for") <+> quotes pp_ty, 
	 nest 4 msg]

nonBoxedPrimCCallErr clas inst_ty
  = hang (ptext SLIT("Unacceptable instance type for ccall-ish class"))
	 4 (pprClassPred clas [inst_ty])
\end{code}


%************************************************************************
%*									*
\subsection{Kind unification}
%*									*
%************************************************************************

\begin{code}
unifyKind :: TcKind		    -- Expected
	  -> TcKind		    -- Actual
	  -> TcM ()
unifyKind k1 k2 
  = tcAddErrCtxtM (unifyCtxt "kind" k1 k2) $
    uTys k1 k1 k2 k2

unifyKinds :: [TcKind] -> [TcKind] -> TcM ()
unifyKinds []       []       = returnTc ()
unifyKinds (k1:ks1) (k2:ks2) = unifyKind k1 k2 	`thenTc_`
			       unifyKinds ks1 ks2
unifyKinds _ _ = panic "unifyKinds: length mis-match"
\end{code}

\begin{code}
unifyOpenTypeKind :: TcKind -> TcM ()	
-- Ensures that the argument kind is of the form (Type bx)
-- for some boxity bx

unifyOpenTypeKind ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyOpenTypeKind ty'
	other	 -> unify_open_kind_help ty

unifyOpenTypeKind ty
  | isTypeKind ty = returnTc ()
  | otherwise     = unify_open_kind_help ty

unify_open_kind_help ty	-- Revert to ordinary unification
  = newBoxityVar 	`thenNF_Tc` \ boxity ->
    unifyKind ty (mkTyConApp typeCon [boxity])
\end{code}


%************************************************************************
%*									*
\subsection[Unify-exported]{Exported unification functions}
%*									*
%************************************************************************

The exported functions are all defined as versions of some
non-exported generic functions.

Unify two @TauType@s.  Dead straightforward.

\begin{code}
unifyTauTy :: TcTauType -> TcTauType -> TcM ()
unifyTauTy ty1 ty2 	-- ty1 expected, ty2 inferred
  = tcAddErrCtxtM (unifyCtxt "type" ty1 ty2) $
    uTys ty1 ty1 ty2 ty2
\end{code}

@unifyTauTyList@ unifies corresponding elements of two lists of
@TauType@s.  It uses @uTys@ to do the real work.  The lists should be
of equal length.  We charge down the list explicitly so that we can
complain if their lengths differ.

\begin{code}
unifyTauTyLists :: [TcTauType] -> [TcTauType] ->  TcM ()
unifyTauTyLists [] 	     []	        = returnTc ()
unifyTauTyLists (ty1:tys1) (ty2:tys2) = uTys ty1 ty1 ty2 ty2   `thenTc_`
					unifyTauTyLists tys1 tys2
unifyTauTyLists ty1s ty2s = panic "Unify.unifyTauTyLists: mismatched type lists!"
\end{code}

@unifyTauTyList@ takes a single list of @TauType@s and unifies them
all together.  It is used, for example, when typechecking explicit
lists, when all the elts should be of the same type.

\begin{code}
unifyTauTyList :: [TcTauType] -> TcM ()
unifyTauTyList []		 = returnTc ()
unifyTauTyList [ty]		 = returnTc ()
unifyTauTyList (ty1:tys@(ty2:_)) = unifyTauTy ty1 ty2	`thenTc_`
				   unifyTauTyList tys
\end{code}

%************************************************************************
%*									*
\subsection[Unify-uTys]{@uTys@: getting down to business}
%*									*
%************************************************************************

@uTys@ is the heart of the unifier.  Each arg happens twice, because
we want to report errors in terms of synomyms if poss.  The first of
the pair is used in error messages only; it is always the same as the
second, except that if the first is a synonym then the second may be a
de-synonym'd version.  This way we get better error messages.

We call the first one \tr{ps_ty1}, \tr{ps_ty2} for ``possible synomym''.

\begin{code}
uTys :: TcTauType -> TcTauType	-- Error reporting ty1 and real ty1
				-- ty1 is the *expected* type

     -> TcTauType -> TcTauType	-- Error reporting ty2 and real ty2
				-- ty2 is the *actual* type
     -> TcM ()

	-- Always expand synonyms (see notes at end)
        -- (this also throws away FTVs)
uTys ps_ty1 (NoteTy n1 ty1) ps_ty2 ty2 = uTys ps_ty1 ty1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (NoteTy n2 ty2) = uTys ps_ty1 ty1 ps_ty2 ty2

	-- Ignore usage annotations inside typechecker
uTys ps_ty1 (UsageTy _ ty1) ps_ty2 ty2 = uTys ps_ty1 ty1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (UsageTy _ ty2) = uTys ps_ty1 ty1 ps_ty2 ty2

	-- Variables; go for uVar
uTys ps_ty1 (TyVarTy tyvar1) ps_ty2 ty2 = uVar False tyvar1 ps_ty2 ty2
uTys ps_ty1 ty1 ps_ty2 (TyVarTy tyvar2) = uVar True  tyvar2 ps_ty1 ty1
					-- "True" means args swapped

	-- Predicates
uTys _ (SourceTy (IParam n1 t1)) _ (SourceTy (IParam n2 t2))
  | n1 == n2 = uTys t1 t1 t2 t2
uTys _ (SourceTy (ClassP c1 tys1)) _ (SourceTy (ClassP c2 tys2))
  | c1 == c2 = unifyTauTyLists tys1 tys2
uTys _ (SourceTy (NType tc1 tys1)) _ (SourceTy (NType tc2 tys2))
  | tc1 == tc2 = unifyTauTyLists tys1 tys2

	-- Functions; just check the two parts
uTys _ (FunTy fun1 arg1) _ (FunTy fun2 arg2)
  = uTys fun1 fun1 fun2 fun2	`thenTc_`    uTys arg1 arg1 arg2 arg2

	-- Type constructors must match
uTys ps_ty1 (TyConApp con1 tys1) ps_ty2 (TyConApp con2 tys2)
  | con1 == con2 && equalLength tys1 tys2
  = unifyTauTyLists tys1 tys2

  | con1 == openKindCon
	-- When we are doing kind checking, we might match a kind '?' 
	-- against a kind '*' or '#'.  Notably, CCallable :: ? -> *, and
	-- (CCallable Int) and (CCallable Int#) are both OK
  = unifyOpenTypeKind ps_ty2

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
uTys ps_ty1 (AppTy s1 t1) ps_ty2 ty2
  = case tcSplitAppTy_maybe ty2 of
	Just (s2,t2) -> uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2
	Nothing      -> unifyMisMatch ps_ty1 ps_ty2

	-- Now the same, but the other way round
	-- Don't swap the types, because the error messages get worse
uTys ps_ty1 ty1 ps_ty2 (AppTy s2 t2)
  = case tcSplitAppTy_maybe ty1 of
	Just (s1,t1) -> uTys s1 s1 s2 s2	`thenTc_`    uTys t1 t1 t2 t2
	Nothing      -> unifyMisMatch ps_ty1 ps_ty2

	-- Not expecting for-alls in unification
	-- ... but the error message from the unifyMisMatch more informative
	-- than a panic message!

	-- Anything else fails
uTys ps_ty1 ty1 ps_ty2 ty2  = unifyMisMatch ps_ty1 ps_ty2
\end{code}


Notes on synonyms
~~~~~~~~~~~~~~~~~
If you are tempted to make a short cut on synonyms, as in this
pseudocode...

\begin{verbatim}
-- NO	uTys (SynTy con1 args1 ty1) (SynTy con2 args2 ty2)
-- NO     = if (con1 == con2) then
-- NO	-- Good news!  Same synonym constructors, so we can shortcut
-- NO	-- by unifying their arguments and ignoring their expansions.
-- NO	unifyTauTypeLists args1 args2
-- NO    else
-- NO	-- Never mind.  Just expand them and try again
-- NO	uTys ty1 ty2
\end{verbatim}

then THINK AGAIN.  Here is the whole story, as detected and reported
by Chris Okasaki \tr{<Chris_Okasaki@loch.mess.cs.cmu.edu>}:
\begin{quotation}
Here's a test program that should detect the problem:

\begin{verbatim}
	type Bogus a = Int
	x = (1 :: Bogus Char) :: Bogus Bool
\end{verbatim}

The problem with [the attempted shortcut code] is that
\begin{verbatim}
	con1 == con2
\end{verbatim}
is not a sufficient condition to be able to use the shortcut!
You also need to know that the type synonym actually USES all
its arguments.  For example, consider the following type synonym
which does not use all its arguments.
\begin{verbatim}
	type Bogus a = Int
\end{verbatim}

If you ever tried unifying, say, \tr{Bogus Char} with \tr{Bogus Bool},
the unifier would blithely try to unify \tr{Char} with \tr{Bool} and
would fail, even though the expanded forms (both \tr{Int}) should
match.

Similarly, unifying \tr{Bogus Char} with \tr{Bogus t} would
unnecessarily bind \tr{t} to \tr{Char}.

... You could explicitly test for the problem synonyms and mark them
somehow as needing expansion, perhaps also issuing a warning to the
user.
\end{quotation}


%************************************************************************
%*									*
\subsection[Unify-uVar]{@uVar@: unifying with a type variable}
%*									*
%************************************************************************

@uVar@ is called when at least one of the types being unified is a
variable.  It does {\em not} assume that the variable is a fixed point
of the substitution; rather, notice that @uVar@ (defined below) nips
back into @uTys@ if it turns out that the variable is already bound.

\begin{code}
uVar :: Bool		-- False => tyvar is the "expected"
			-- True  => ty    is the "expected" thing
     -> TcTyVar
     -> TcTauType -> TcTauType	-- printing and real versions
     -> TcM ()

uVar swapped tv1 ps_ty2 ty2
  = getTcTyVar tv1	`thenNF_Tc` \ maybe_ty1 ->
    case maybe_ty1 of
	Just ty1 | swapped   -> uTys ps_ty2 ty2 ty1 ty1	-- Swap back
		 | otherwise -> uTys ty1 ty1 ps_ty2 ty2	-- Same order
	other       -> uUnboundVar swapped tv1 maybe_ty1 ps_ty2 ty2

	-- Expand synonyms; ignore FTVs
uUnboundVar swapped tv1 maybe_ty1 ps_ty2 (NoteTy n2 ty2)
  = uUnboundVar swapped tv1 maybe_ty1 ps_ty2 ty2


	-- The both-type-variable case
uUnboundVar swapped tv1 maybe_ty1 ps_ty2 ty2@(TyVarTy tv2)

	-- Same type variable => no-op
  | tv1 == tv2
  = returnTc ()

	-- Distinct type variables
	-- ASSERT maybe_ty1 /= Just
  | otherwise
  = getTcTyVar tv2	`thenNF_Tc` \ maybe_ty2 ->
    case maybe_ty2 of
	Just ty2' -> uUnboundVar swapped tv1 maybe_ty1 ty2' ty2'

	Nothing | update_tv2

		-> WARN( not (k1 `hasMoreBoxityInfo` k2), (ppr tv1 <+> ppr k1) $$ (ppr tv2 <+> ppr k2) )
		   putTcTyVar tv2 (TyVarTy tv1)		`thenNF_Tc_`
		   returnTc ()
		|  otherwise

		-> WARN( not (k2 `hasMoreBoxityInfo` k1), (ppr tv2 <+> ppr k2) $$ (ppr tv1 <+> ppr k1) )
                   (putTcTyVar tv1 ps_ty2		`thenNF_Tc_`
	  	    returnTc ())
  where
    k1 = tyVarKind tv1
    k2 = tyVarKind tv2
    update_tv2 = (k2 `eqKind` openTypeKind) || (not (k1 `eqKind` openTypeKind) && nicer_to_update_tv2)
			-- Try to get rid of open type variables as soon as poss

    nicer_to_update_tv2 =  isUserTyVar (mutTyVarDetails tv1)
				-- Don't unify a signature type variable if poss
			|| isSystemName (varName tv2)
				-- Try to update sys-y type variables in preference to sig-y ones

	-- Second one isn't a type variable
uUnboundVar swapped tv1 maybe_ty1 ps_ty2 non_var_ty2
  = 	-- Check that the kinds match
    checkKinds swapped tv1 non_var_ty2			`thenTc_`

	-- Check that tv1 isn't a type-signature type variable
    checkTcM (not (isSkolemTyVar (mutTyVarDetails tv1)))
	     (failWithTcM (unifyWithSigErr tv1 ps_ty2))	`thenTc_`

	-- Check that we aren't losing boxity info (shouldn't happen)
    warnTc (not (typeKind non_var_ty2 `hasMoreBoxityInfo` tyVarKind tv1))
	   ((ppr tv1 <+> ppr (tyVarKind tv1)) $$ 
	     (ppr non_var_ty2 <+> ppr (typeKind non_var_ty2)))		`thenNF_Tc_` 

	-- Occurs check
	-- Basically we want to update     tv1 := ps_ty2
	-- because ps_ty2 has type-synonym info, which improves later error messages
	-- 
	-- But consider 
	--	type A a = ()
	--
	--	f :: (A a -> a -> ()) -> ()
	--	f = \ _ -> ()
	--
	-- 	x :: ()
	-- 	x = f (\ x p -> p x)
	--
	-- In the application (p x), we try to match "t" with "A t".  If we go
	-- ahead and bind t to A t (= ps_ty2), we'll lead the type checker into 
	-- an infinite loop later.
 	-- But we should not reject the program, because A t = ().
	-- Rather, we should bind t to () (= non_var_ty2).
	-- 
	-- That's why we have this two-state occurs-check
    zonkTcType ps_ty2					`thenNF_Tc` \ ps_ty2' ->
    if not (tv1 `elemVarSet` tyVarsOfType ps_ty2') then
	putTcTyVar tv1 ps_ty2'				`thenNF_Tc_`
	returnTc ()
    else
    zonkTcType non_var_ty2				`thenNF_Tc` \ non_var_ty2' ->
    if not (tv1 `elemVarSet` tyVarsOfType non_var_ty2') then
	-- This branch rarely succeeds, except in strange cases
	-- like that in the example above
	putTcTyVar tv1 non_var_ty2'			`thenNF_Tc_`
	returnTc ()
    else
    failWithTcM (unifyOccurCheck tv1 ps_ty2')


checkKinds swapped tv1 ty2
-- We're about to unify a type variable tv1 with a non-tyvar-type ty2.
-- We need to check that we don't unify a lifted type variable with an
-- unlifted type: e.g.  (id 3#) is illegal
  | tk1 `eqKind` liftedTypeKind && tk2 `eqKind` unliftedTypeKind
  = tcAddErrCtxtM (unifyKindCtxt swapped tv1 ty2)	$
    unifyMisMatch k1 k2
  | otherwise
  = returnTc ()
  where
    (k1,k2) | swapped   = (tk2,tk1)
	    | otherwise = (tk1,tk2)
    tk1 = tyVarKind tv1
    tk2 = typeKind ty2
\end{code}


%************************************************************************
%*									*
\subsection[Unify-fun]{@unifyFunTy@}
%*									*
%************************************************************************

@unifyFunTy@ is used to avoid the fruitless creation of type variables.

\begin{code}
unifyFunTy :: TcType	 			-- Fail if ty isn't a function type
	   -> TcM (TcType, TcType)	-- otherwise return arg and result types

unifyFunTy ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyFunTy ty'
	other	    -> unify_fun_ty_help ty

unifyFunTy ty
  = case tcSplitFunTy_maybe ty of
	Just arg_and_res -> returnTc arg_and_res
	Nothing 	 -> unify_fun_ty_help ty

unify_fun_ty_help ty	-- Special cases failed, so revert to ordinary unification
  = newTyVarTy openTypeKind	`thenNF_Tc` \ arg ->
    newTyVarTy openTypeKind	`thenNF_Tc` \ res ->
    unifyTauTy ty (mkFunTy arg res)	`thenTc_`
    returnTc (arg,res)
\end{code}

\begin{code}
unifyListTy :: TcType              -- expected list type
	    -> TcM TcType      -- list element type

unifyListTy ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyListTy ty'
	other	 -> unify_list_ty_help ty

unifyListTy ty
  = case tcSplitTyConApp_maybe ty of
	Just (tycon, [arg_ty]) | tycon == listTyCon -> returnTc arg_ty
	other					    -> unify_list_ty_help ty

unify_list_ty_help ty	-- Revert to ordinary unification
  = newTyVarTy liftedTypeKind		`thenNF_Tc` \ elt_ty ->
    unifyTauTy ty (mkListTy elt_ty)	`thenTc_`
    returnTc elt_ty
\end{code}

\begin{code}
unifyTupleTy :: Boxity -> Arity -> TcType -> TcM [TcType]
unifyTupleTy boxity arity ty@(TyVarTy tyvar)
  = getTcTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> unifyTupleTy boxity arity ty'
	other	 -> unify_tuple_ty_help boxity arity ty

unifyTupleTy boxity arity ty
  = case tcSplitTyConApp_maybe ty of
	Just (tycon, arg_tys)
		|  isTupleTyCon tycon 
		&& tyConArity tycon == arity
		&& tupleTyConBoxity tycon == boxity
		-> returnTc arg_tys
	other -> unify_tuple_ty_help boxity arity ty

unify_tuple_ty_help boxity arity ty
  = newTyVarTys arity kind				`thenNF_Tc` \ arg_tys ->
    unifyTauTy ty (mkTupleTy boxity arity arg_tys)	`thenTc_`
    returnTc arg_tys
  where
    kind | isBoxed boxity = liftedTypeKind
	 | otherwise      = openTypeKind
\end{code}


%************************************************************************
%*									*
\subsection[Unify-context]{Errors and contexts}
%*									*
%************************************************************************

Errors
~~~~~~

\begin{code}
unifyCtxt s ty1 ty2 tidy_env	-- ty1 expected, ty2 inferred
  = zonkTcType ty1	`thenNF_Tc` \ ty1' ->
    zonkTcType ty2	`thenNF_Tc` \ ty2' ->
    returnNF_Tc (err ty1' ty2')
  where
    err ty1 ty2 = (env1, 
		   nest 4 
			(vcat [
			   text "Expected" <+> text s <> colon <+> ppr tidy_ty1,
			   text "Inferred" <+> text s <> colon <+> ppr tidy_ty2
		        ]))
		  where
		    (env1, [tidy_ty1,tidy_ty2]) = tidyOpenTypes tidy_env [ty1,ty2]

unifyKindCtxt swapped tv1 ty2 tidy_env	-- not swapped => tv1 expected, ty2 inferred
	-- tv1 is zonked already
  = zonkTcType ty2	`thenNF_Tc` \ ty2' ->
    returnNF_Tc (err ty2')
  where
    err ty2 = (env2, ptext SLIT("When matching types") <+> 
		     sep [quotes pp_expected, ptext SLIT("and"), quotes pp_actual])
	    where
	      (pp_expected, pp_actual) | swapped   = (pp2, pp1)
				       | otherwise = (pp1, pp2)
	      (env1, tv1') = tidyOpenTyVar tidy_env tv1
	      (env2, ty2') = tidyOpenType  env1 ty2
	      pp1 = ppr tv1'
	      pp2 = ppr ty2'

unifyMisMatch ty1 ty2
  = zonkTcType ty1	`thenNF_Tc` \ ty1' ->
    zonkTcType ty2	`thenNF_Tc` \ ty2' ->
    let
    	(env, [tidy_ty1, tidy_ty2]) = tidyOpenTypes emptyTidyEnv [ty1',ty2']
	msg = hang (ptext SLIT("Couldn't match"))
		   4 (sep [quotes (ppr tidy_ty1), 
			   ptext SLIT("against"), 
			   quotes (ppr tidy_ty2)])
    in
    failWithTcM (env, msg)

unifyWithSigErr tyvar ty
  = (env2, hang (ptext SLIT("Cannot unify the type-signature variable") <+> quotes (ppr tidy_tyvar))
	      4 (ptext SLIT("with the type") <+> quotes (ppr tidy_ty)))
  where
    (env1, tidy_tyvar) = tidyOpenTyVar emptyTidyEnv tyvar
    (env2, tidy_ty)    = tidyOpenType  env1         ty

unifyOccurCheck tyvar ty
  = (env2, hang (ptext SLIT("Occurs check: cannot construct the infinite type:"))
	      4 (sep [ppr tidy_tyvar, char '=', ppr tidy_ty]))
  where
    (env1, tidy_tyvar) = tidyOpenTyVar emptyTidyEnv tyvar
    (env2, tidy_ty)    = tidyOpenType  env1         ty
\end{code}
