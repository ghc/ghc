%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Monadic type operations}

This module contains monadic operations over types that contain mutable type variables

\begin{code}
module TcMType (
  TcTyVar, TcKind, TcType, TcTauType, TcThetaType, TcTyVarSet,

  --------------------------------
  -- Creating new mutable type variables
  newTyVar, newHoleTyVarTy,
  newTyVarTy,		-- Kind -> NF_TcM TcType
  newTyVarTys,		-- Int -> Kind -> NF_TcM [TcType]
  newKindVar, newKindVars, newBoxityVar,
  putTcTyVar, getTcTyVar,

  --------------------------------
  -- Instantiation
  tcInstTyVar, tcInstTyVars,
  tcInstSigTyVars, tcInstType, tcInstSigType,
  tcSplitRhoTyM,

  --------------------------------
  -- Checking type validity
  Rank, UserTypeCtxt(..), checkValidType, pprUserTypeCtxt,
  SourceTyCtxt(..), checkValidTheta, 
  checkValidInstHead, instTypeErr, checkAmbiguity,

  --------------------------------
  -- Zonking
  zonkTcTyVar, zonkTcTyVars, zonkTcTyVarsAndFV, 
  zonkTcType, zonkTcTypes, zonkTcClassConstraints, zonkTcThetaType,
  zonkTcPredType, zonkTcTypeToType, zonkTcTyVarToTyVar, zonkKindEnv,

  ) where

#include "HsVersions.h"


-- friends:
import TypeRep		( Type(..), SourceType(..), TyNote(..),	 -- Friend; can see representation
			  Kind, ThetaType
			) 
import TcType		( TcType, TcThetaType, TcTauType, TcPredType,
			  TcTyVarSet, TcKind, TcTyVar, TyVarDetails(..),
			  tcEqType, tcCmpPred,
			  tcSplitRhoTy, tcSplitPredTy_maybe, tcSplitAppTy_maybe, 
			  tcSplitTyConApp_maybe, tcSplitForAllTys,
			  tcIsTyVarTy, tcSplitSigmaTy, 
			  isUnLiftedType, isIPPred, 

			  mkAppTy, mkTyVarTy, mkTyVarTys, 
			  tyVarsOfPred, getClassPredTys_maybe,

			  liftedTypeKind, openTypeKind, defaultKind, superKind,
			  superBoxity, liftedBoxity, typeKind,
			  tyVarsOfType, tyVarsOfTypes, 
			  eqKind, isTypeKind, isAnyTypeKind,

			  isFFIArgumentTy, isFFIImportResultTy
			)
import qualified Type	( splitFunTys )
import Subst		( Subst, mkTopTyVarSubst, substTy )
import Class		( Class, classArity, className )
import TyCon		( TyCon, mkPrimTyCon, isSynTyCon, isUnboxedTupleTyCon, 
			  tyConArity, tyConName, tyConKind )
import PrimRep		( PrimRep(VoidRep) )
import Var		( TyVar, tyVarKind, tyVarName, isTyVar, mkTyVar, isMutTyVar )

-- others:
import TcMonad          -- TcType, amongst others
import TysWiredIn	( voidTy, listTyCon, tupleTyCon )
import PrelNames	( cCallableClassKey, cReturnableClassKey, hasKey )
import ForeignCall	( Safety(..) )
import FunDeps		( grow )
import PprType		( pprPred, pprSourceType, pprTheta, pprClassPred )
import Name		( Name, NamedThing(..), setNameUnique, mkSysLocalName,
			  mkLocalName, mkDerivedTyConOcc
			)
import VarSet
import BasicTypes	( Boxity(Boxed) )
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
    tcNewMutTyVar (mkSysLocalName uniq FSLIT("t")) kind VanillaTv

newTyVarTy  :: Kind -> NF_TcM TcType
newTyVarTy kind
  = newTyVar kind	`thenNF_Tc` \ tc_tyvar ->
    returnNF_Tc (TyVarTy tc_tyvar)

newHoleTyVarTy :: NF_TcM TcType
  = tcGetUnique 	`thenNF_Tc` \ uniq ->
    tcNewMutTyVar (mkSysLocalName uniq FSLIT("h")) openTypeKind HoleTv	`thenNF_Tc` \ tv ->
    returnNF_Tc (TyVarTy tv)

newTyVarTys :: Int -> Kind -> NF_TcM [TcType]
newTyVarTys n kind = mapNF_Tc newTyVarTy (nOfThem n kind)

newKindVar :: NF_TcM TcKind
newKindVar
  = tcGetUnique 							`thenNF_Tc` \ uniq ->
    tcNewMutTyVar (mkSysLocalName uniq FSLIT("k")) superKind VanillaTv	`thenNF_Tc` \ kv ->
    returnNF_Tc (TyVarTy kv)

newKindVars :: Int -> NF_TcM [TcKind]
newKindVars n = mapNF_Tc (\ _ -> newKindVar) (nOfThem n ())

newBoxityVar :: NF_TcM TcKind
newBoxityVar
  = tcGetUnique 							  `thenNF_Tc` \ uniq ->
    tcNewMutTyVar (mkSysLocalName uniq FSLIT("bx")) superBoxity VanillaTv  `thenNF_Tc` \ kv ->
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


tcInstSigType :: TyVarDetails -> Type -> NF_TcM ([TcTyVar], TcThetaType, TcType)
-- Very similar to tcInstSigType, but uses signature type variables
-- Also, somewhat arbitrarily, don't deal with the monomorphic case so efficiently
tcInstSigType tv_details poly_ty
 = let
	(tyvars, rho) = tcSplitForAllTys poly_ty
   in
   tcInstSigTyVars tv_details tyvars		`thenNF_Tc` \ tyvars' ->
	-- Make *signature* type variables

   let
     tyvar_tys' = mkTyVarTys tyvars'
     rho' = substTy (mkTopTyVarSubst tyvars tyvar_tys') rho
	-- mkTopTyVarSubst because the tyvars' are fresh

     (theta', tau') = tcSplitRhoTy rho'
	-- This splitRhoTy tries hard to make sure that tau' is a type synonym
	-- wherever possible, which can improve interface files.
   in
   returnNF_Tc (tyvars', theta', tau')
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
zonkTcPredType (ClassP c ts)
  = zonkTcTypes ts	`thenNF_Tc` \ new_ts ->
    returnNF_Tc (ClassP c new_ts)
zonkTcPredType (IParam n t)
  = zonkTcType t	`thenNF_Tc` \ new_t ->
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
	-- Zonk a mutable but unbound type variable to an arbitrary type
	-- We know it's unbound even though we don't carry an environment,
	-- because at the binding site for a type variable we bind the
	-- mutable tyvar to a fresh immutable one.  So the mutable store
	-- plays the role of an environment.  If we come across a mutable
	-- type variable that isn't so bound, it must be completely free.
    zonk_unbound_tyvar tv = putTcTyVar tv (mkArbitraryType tv)


-- When the type checker finds a type variable with no binding,
-- which means it can be instantiated with an arbitrary type, it
-- usually instantiates it to Void.  Eg.
-- 
-- 	length []
-- ===>
-- 	length Void (Nil Void)
-- 
-- But in really obscure programs, the type variable might have
-- a kind other than *, so we need to invent a suitably-kinded type.
-- 
-- This commit uses
-- 	Void for kind *
-- 	List for kind *->*
-- 	Tuple for kind *->...*->*
-- 
-- which deals with most cases.  (Previously, it only dealt with
-- kind *.)   
-- 
-- In the other cases, it just makes up a TyCon with a suitable
-- kind.  If this gets into an interface file, anyone reading that
-- file won't understand it.  This is fixable (by making the client
-- of the interface file make up a TyCon too) but it is tiresome and
-- never happens, so I am leaving it 

mkArbitraryType :: TcTyVar -> Type
-- Make up an arbitrary type whose kind is the same as the tyvar.
-- We'll use this to instantiate the (unbound) tyvar.
mkArbitraryType tv 
  | isAnyTypeKind kind = voidTy		-- The vastly common case
  | otherwise	       = TyConApp tycon []
  where
    kind       = tyVarKind tv
    (args,res) = Type.splitFunTys kind	-- Kinds are simple; use Type.splitFunTys

    tycon | kind `eqKind` tyConKind listTyCon 	-- *->*
	  = listTyCon				-- No tuples this size

	  | all isTypeKind args && isTypeKind res
	  = tupleTyCon Boxed (length args)	-- *-> ... ->*->*

	  | otherwise
	  = pprTrace "Urk! Inventing strangely-kinded void TyCon" (ppr tc_name) $
	    mkPrimTyCon tc_name kind 0 [] VoidRep
		-- Same name as the tyvar, apart from making it start with a colon (sigh)
		-- I dread to think what will happen if this gets out into an 
		-- interface file.  Catastrophe likely.  Major sigh.

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

	-- The two interesting cases!
    go (TyVarTy tyvar)     = zonkTyVar unbound_var_fn tyvar

    go (ForAllTy tyvar ty) = zonkTcTyVarToTyVar tyvar	`thenNF_Tc` \ tyvar' ->
			     go ty			`thenNF_Tc` \ ty' ->
			     returnNF_Tc (ForAllTy tyvar' ty')

    go_pred (ClassP c tys) = mapNF_Tc go tys	`thenNF_Tc` \ tys' ->
			     returnNF_Tc (ClassP c tys')
    go_pred (NType tc tys) = mapNF_Tc go tys	`thenNF_Tc` \ tys' ->
			     returnNF_Tc (NType tc tys')
    go_pred (IParam n ty)  = go ty		`thenNF_Tc` \ ty' ->
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
	rank | gla_exts = Arbitrary
	     | otherwise
	     = case ctxt of	-- Haskell 98
		 GenPatCtxt	-> Rank 0
		 PatSigCtxt	-> Rank 0
		 ResSigCtxt	-> Rank 0
		 TySynCtxt _    -> Rank 0
		 ExprSigCtxt 	-> Rank 1
		 FunSigCtxt _   -> Rank 1
		 ConArgCtxt _   -> Rank 1	-- We are given the type of the entire
						-- constructor, hence rank 1
		 ForSigCtxt _	-> Rank 1
		 RuleSigCtxt _	-> Rank 1

	actual_kind = typeKind ty

	actual_kind_is_lifted = actual_kind `eqKind` liftedTypeKind

	kind_ok = case ctxt of
			TySynCtxt _  -> True	-- Any kind will do
			GenPatCtxt   -> actual_kind_is_lifted
			ForSigCtxt _ -> actual_kind_is_lifted
			other	     -> isTypeKind actual_kind
	
	ubx_tup | not gla_exts = UT_NotOk
		| otherwise    = case ctxt of
				   TySynCtxt _ -> UT_Ok
				   other       -> UT_NotOk
		-- Unboxed tuples ok in function results,
		-- but for type synonyms we allow them even at
		-- top level
    in
    tcAddErrCtxt (checkTypeCtxt ctxt ty)	$

	-- Check that the thing has kind Type, and is lifted if necessary
    checkTc kind_ok (kindErr actual_kind)	`thenTc_`

	-- Check the internal validity of the type itself
    check_poly_type rank ubx_tup ty


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
data Rank = Rank Int | Arbitrary

decRank :: Rank -> Rank
decRank Arbitrary = Arbitrary
decRank (Rank n)  = Rank (n-1)

----------------------------------------
data UbxTupFlag = UT_Ok	| UT_NotOk
	-- The "Ok" version means "ok if -fglasgow-exts is on"

----------------------------------------
check_poly_type :: Rank -> UbxTupFlag -> Type -> TcM ()
check_poly_type (Rank 0) ubx_tup ty 
  = check_tau_type (Rank 0) ubx_tup ty

check_poly_type rank ubx_tup ty 
  = let
	(tvs, theta, tau) = tcSplitSigmaTy ty
    in
    check_valid_theta SigmaCtxt theta		`thenTc_`
    check_tau_type (decRank rank) ubx_tup tau	`thenTc_`
    checkFreeness tvs theta 			`thenTc_`
    checkAmbiguity tvs theta (tyVarsOfType tau)

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
--     But not in user code.
-- Anyway, they are dealt with by a special case in check_tau_type

check_arg_type ty 
  = check_tau_type (Rank 0) UT_NotOk ty		`thenTc_` 
    checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty)

----------------------------------------
check_tau_type :: Rank -> UbxTupFlag -> Type -> TcM ()
-- Rank is allowed rank for function args
-- No foralls otherwise

check_tau_type rank ubx_tup ty@(ForAllTy _ _) = failWithTc (forAllTyErr ty)
check_tau_type rank ubx_tup (SourceTy sty)    = getDOptsTc		`thenNF_Tc` \ dflags ->
						check_source_ty dflags TypeCtxt sty
check_tau_type rank ubx_tup (TyVarTy _)       = returnTc ()
check_tau_type rank ubx_tup ty@(FunTy arg_ty res_ty)
  = check_poly_type rank UT_NotOk arg_ty	`thenTc_`
    check_tau_type  rank UT_Ok    res_ty

check_tau_type rank ubx_tup (AppTy ty1 ty2)
  = check_arg_type ty1 `thenTc_` check_arg_type ty2

check_tau_type rank ubx_tup (NoteTy note ty)
  = check_tau_type rank ubx_tup ty
	-- Synonym notes are built only when the synonym is 
	-- saturated (see Type.mkSynTy)
	-- Not checking the 'note' part allows us to instantiate a synonym
	-- defn with a for-all type, but that seems OK too

check_tau_type rank ubx_tup ty@(TyConApp tc tys)
  | isSynTyCon tc	
  = 	-- NB: Type.mkSynTy builds a TyConApp (not a NoteTy) for an unsaturated
	-- synonym application, leaving it to checkValidType (i.e. right here
	-- to find the error
    checkTc syn_arity_ok arity_msg	`thenTc_`
    mapTc_ check_arg_type tys
    
  | isUnboxedTupleTyCon tc
  = doptsTc Opt_GlasgowExts			`thenNF_Tc` \ gla_exts ->
    checkTc (ubx_tup_ok gla_exts) ubx_tup_msg	`thenTc_`
    mapTc_ (check_tau_type (Rank 0) UT_Ok) tys	
			-- Args are allowed to be unlifted, or
			-- more unboxed tuples, so can't use check_arg_ty

  | otherwise
  = mapTc_ check_arg_type tys

  where
    ubx_tup_ok gla_exts = case ubx_tup of { UT_Ok -> gla_exts; other -> False }

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
forAllTyErr     ty = ptext SLIT("Illegal polymorphic type:") <+> ppr_ty ty
unliftedArgErr  ty = ptext SLIT("Illegal unlifted type argument:") <+> ppr_ty ty
ubxArgTyErr     ty = ptext SLIT("Illegal unboxed tuple type as function argument:") <+> ppr_ty ty
kindErr kind       = ptext SLIT("Expecting an ordinary type, but found a type of kind") <+> ppr kind
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
checkAmbiguity :: [TyVar] -> ThetaType -> TyVarSet -> TcM ()
checkAmbiguity forall_tyvars theta tau_tyvars
  = mapTc_ complain (filter is_ambig theta)
  where
    complain pred     = addErrTc (ambigErr pred)
    extended_tau_vars = grow theta tau_tyvars
    is_ambig pred     = any ambig_var (varSetElems (tyVarsOfPred pred))

    ambig_var ct_var  = (ct_var `elem` forall_tyvars) &&
		        not (ct_var `elemVarSet` extended_tau_vars)

    is_free ct_var    = not (ct_var `elem` forall_tyvars)

ambigErr pred
  = sep [ptext SLIT("Ambiguous constraint") <+> quotes (pprPred pred),
	 nest 4 (ptext SLIT("At least one of the forall'd type variables mentioned by the constraint") $$
		 ptext SLIT("must be reachable from the type after the '=>'"))]
\end{code}
    
In addition, GHC insists that at least one type variable
in each constraint is in V.  So we disallow a type like
	forall a. Eq b => b -> b
even in a scope where b is in scope.

\begin{code}
checkFreeness forall_tyvars theta
  = mapTc_ complain (filter is_free theta)
  where    
    is_free pred     =  not (isIPPred pred)
		     && not (any bound_var (varSetElems (tyVarsOfPred pred)))
    bound_var ct_var = ct_var `elem` forall_tyvars
    complain pred    = addErrTc (freeErr pred)

freeErr pred
  = sep [ptext SLIT("All of the type variables in the constraint") <+> quotes (pprPred pred) <+>
		   ptext SLIT("are already in scope"),
	 nest 4 (ptext SLIT("At least one must be universally quantified here"))
    ]
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
    mapTc_ check_arg_type tys		`thenTc_`
    checkTc (arity == n_tys) arity_err		`thenTc_`
    checkTc (all tyvar_head tys || arby_preds_ok)
	    (predTyVarErr pred $$ how_to_allow)

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

    how_to_allow = case ctxt of
		     InstHeadCtxt  -> empty	-- Should not happen
		     InstThetaCtxt -> parens undecidableMsg
		     other	   -> parens (ptext SLIT("Use -fglasgow-exts to permit this"))

check_source_ty dflags SigmaCtxt (IParam _ ty) = check_arg_type ty
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
checkValidInstHead :: Type -> TcM (Class, [TcType])

checkValidInstHead ty	-- Should be a source type
  = case tcSplitPredTy_maybe ty of {
	Nothing -> failWithTc (instTypeErr (ppr ty) empty) ;
	Just pred -> 

    case getClassPredTys_maybe pred of {
	Nothing -> failWithTc (instTypeErr (pprPred pred) empty) ;
        Just (clas,tys) ->

    getDOptsTc					`thenNF_Tc` \ dflags ->
    mapTc_ check_arg_type tys			`thenTc_`
    check_inst_head dflags clas tys		`thenTc_`
    returnTc (clas, tys)
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
		   $$ undecidableMsg)

undecidableMsg = ptext SLIT("Use -fallow-undecidable-instances to permit this")
\end{code}

\begin{code}
instTypeErr pp_ty msg
  = sep [ptext SLIT("Illegal instance declaration for") <+> quotes pp_ty, 
	 nest 4 msg]

nonBoxedPrimCCallErr clas inst_ty
  = hang (ptext SLIT("Unacceptable instance type for ccall-ish class"))
	 4 (pprClassPred clas [inst_ty])
\end{code}
