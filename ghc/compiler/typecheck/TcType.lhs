%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcType]{Types used in the typechecker}

\begin{code}
module TcType (
  
  TcTyVar,
  TcTyVarSet,
  newTyVar,
  newTyVarTy,		-- Kind -> NF_TcM TcType
  newTyVarTys,		-- Int -> Kind -> NF_TcM [TcType]

  -----------------------------------------
  TcType, TcTauType, TcThetaType, TcRhoType,

	-- Find the type to which a type variable is bound
  tcPutTyVar,		-- :: TcTyVar -> TcType -> NF_TcM TcType
  tcGetTyVar,		-- :: TcTyVar -> NF_TcM (Maybe TcType)	does shorting out


  tcSplitRhoTy,

  tcInstTyVar, tcInstTyVars,
  tcInstSigVar,
  tcInstType,

  --------------------------------
  TcKind,
  newKindVar, newKindVars, newBoxityVar,

  --------------------------------
  zonkTcTyVar, zonkTcTyVars, zonkTcTyVarsAndFV, zonkTcSigTyVars,
  zonkTcType, zonkTcTypes, zonkTcClassConstraints, zonkTcThetaType,
  zonkTcPredType,

  zonkTcTypeToType, zonkTcTyVarToTyVar, zonkKindEnv

  ) where

#include "HsVersions.h"


-- friends:
import TypeRep		( Type(..), Kind, TyNote(..) )  -- friend
import Type		( PredType(..),
			  getTyVar, mkAppTy, mkUTy,
			  splitPredTy_maybe, splitForAllTys, 
			  isTyVarTy, mkTyVarTy, mkTyVarTys, 
			  openTypeKind, liftedTypeKind, 
			  superKind, superBoxity, tyVarsOfTypes,
			  defaultKind, liftedBoxity
			)
import Subst		( Subst, mkTopTyVarSubst, substTy )
import TyCon		( mkPrimTyCon )
import PrimRep		( PrimRep(VoidRep) )
import Var		( TyVar, tyVarKind, tyVarName, isTyVar, isMutTyVar, mkTyVar )

-- others:
import TcMonad          -- TcType, amongst others
import TysWiredIn	( voidTy )

import Name		( Name, NamedThing(..), setNameUnique, mkSysLocalName,
			  mkLocalName, mkDerivedTyConOcc
			)
import Unique		( Uniquable(..) )
import SrcLoc		( noSrcLoc )
import Util		( nOfThem )
import Outputable
\end{code}


Utility functions
~~~~~~~~~~~~~~~~~
These tcSplit functions are like their non-Tc analogues, but they
follow through bound type variables.

No need for tcSplitForAllTy because a type variable can't be instantiated
to a for-all type.

\begin{code}
tcSplitRhoTy :: TcType -> NF_TcM (TcThetaType, TcType)
tcSplitRhoTy t
  = go t t []
 where
	-- A type variable is never instantiated to a dictionary type,
	-- so we don't need to do a tcReadVar on the "arg".
    go syn_t (FunTy arg res) ts = case splitPredTy_maybe arg of
					Just pair -> go res res (pair:ts)
					Nothing   -> returnNF_Tc (reverse ts, syn_t)
    go syn_t (NoteTy _ t)    ts = go syn_t t ts
    go syn_t (TyVarTy tv)    ts = tcGetTyVar tv		`thenNF_Tc` \ maybe_ty ->
				  case maybe_ty of
				    Just ty | not (isTyVarTy ty) -> go syn_t ty ts
				    other			 -> returnNF_Tc (reverse ts, syn_t)
    go syn_t (UsageTy _ t)   ts = go syn_t t ts
    go syn_t t		     ts = returnNF_Tc (reverse ts, syn_t)
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
    tcNewMutTyVar (mkSysLocalName uniq SLIT("t")) kind

newTyVarTy  :: Kind -> NF_TcM TcType
newTyVarTy kind
  = newTyVar kind	`thenNF_Tc` \ tc_tyvar ->
    returnNF_Tc (TyVarTy tc_tyvar)

newTyVarTys :: Int -> Kind -> NF_TcM [TcType]
newTyVarTys n kind = mapNF_Tc newTyVarTy (nOfThem n kind)

newKindVar :: NF_TcM TcKind
newKindVar
  = tcGetUnique 						`thenNF_Tc` \ uniq ->
    tcNewMutTyVar (mkSysLocalName uniq SLIT("k")) superKind	`thenNF_Tc` \ kv ->
    returnNF_Tc (TyVarTy kv)

newKindVars :: Int -> NF_TcM [TcKind]
newKindVars n = mapNF_Tc (\ _ -> newKindVar) (nOfThem n ())

newBoxityVar :: NF_TcM TcKind
newBoxityVar
  = tcGetUnique 						`thenNF_Tc` \ uniq ->
    tcNewMutTyVar (mkSysLocalName uniq SLIT("bx")) superBoxity	`thenNF_Tc` \ kv ->
    returnNF_Tc (TyVarTy kv)
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
    tcNewMutTyVar name (tyVarKind tyvar)

tcInstSigVar tyvar	-- Very similar to tcInstTyVar
  = tcGetUnique 	`thenNF_Tc` \ uniq ->
    let 
	name = setNameUnique (tyVarName tyvar) uniq
	kind = tyVarKind tyvar
    in
    ASSERT( not (kind == openTypeKind) )	-- Shouldn't happen
    tcNewSigTyVar name kind
\end{code}

@tcInstType@ instantiates the outer-level for-alls of a TcType with
fresh type variables, splits off the dictionary part, and returns the results.

\begin{code}
tcInstType :: TcType -> NF_TcM ([TcTyVar], TcThetaType, TcType)
tcInstType ty
  = case splitForAllTys ty of
	([],     _)   -> returnNF_Tc ([], [], ty) 	 -- Nothing to do
	(tyvars, rho) -> tcInstTyVars tyvars			`thenNF_Tc` \ (tyvars', _, tenv)  ->
			 tcSplitRhoTy (substTy tenv rho)	`thenNF_Tc` \ (theta, tau) ->
			 returnNF_Tc (tyvars', theta, tau)
\end{code}



%************************************************************************
%*									*
\subsection{Putting and getting  mutable type variables}
%*									*
%************************************************************************

\begin{code}
tcPutTyVar :: TcTyVar -> TcType -> NF_TcM TcType
tcGetTyVar :: TcTyVar -> NF_TcM (Maybe TcType)
\end{code}

Putting is easy:

\begin{code}
tcPutTyVar tyvar ty 
  | not (isMutTyVar tyvar)
  = pprTrace "tcPutTyVar" (ppr tyvar) $
    returnNF_Tc ty

  | otherwise
  = ASSERT( isMutTyVar tyvar )
    UASSERT2( not (isUTy ty), ppr tyvar <+> ppr ty )
    tcWriteMutTyVar tyvar (Just ty)	`thenNF_Tc_`
    returnNF_Tc ty
\end{code}

Getting is more interesting.  The easy thing to do is just to read, thus:

\begin{verbatim}
tcGetTyVar tyvar = tcReadMutTyVar tyvar
\end{verbatim}

But it's more fun to short out indirections on the way: If this
version returns a TyVar, then that TyVar is unbound.  If it returns
any other type, then there might be bound TyVars embedded inside it.

We return Nothing iff the original box was unbound.

\begin{code}
tcGetTyVar tyvar
  | not (isMutTyVar tyvar)
  = pprTrace "tcGetTyVar" (ppr tyvar) $
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
    returnNF_Tc (map (getTyVar "zonkTcSigTyVars") tys)
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
    zonk_unbound_kind_var kv | tyVarKind kv == superKind   = tcPutTyVar kv liftedTypeKind
			     | tyVarKind kv == superBoxity = tcPutTyVar kv liftedBoxity
			     | otherwise		   = pprPanic "zonkKindEnv" (ppr kv)
			
zonkTcTypeToType :: TcType -> NF_TcM Type
zonkTcTypeToType ty = zonkType zonk_unbound_tyvar ty
  where
	-- Zonk a mutable but unbound type variable to
	--	Void		if it has kind Lifted
	--	:Void		otherwise
    zonk_unbound_tyvar tv
	| kind == liftedTypeKind || kind == openTypeKind
	= tcPutTyVar tv voidTy	-- Just to avoid creating a new tycon in
				-- this vastly common case
	| otherwise
	= tcPutTyVar tv (TyConApp (mk_void_tycon tv kind) [])
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

        zap tv = tcPutTyVar tv immut_tv_ty
		-- Bind the mutable version to the immutable one
    in 
	-- If the type variable is mutable, then bind it to immut_tv_ty
	-- so that all other occurrences of the tyvar will get zapped too
    zonkTyVar zap tv		`thenNF_Tc` \ ty2 ->

    WARN( immut_tv_ty /= ty2, ppr tv $$ ppr immut_tv $$ ppr ty2 )

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

    go (PredTy p)		  = go_pred p		`thenNF_Tc` \ p' ->
				    returnNF_Tc (PredTy p')

    go (FunTy arg res)      	  = go arg		`thenNF_Tc` \ arg' ->
				    go res		`thenNF_Tc` \ res' ->
				    returnNF_Tc (FunTy arg' res')
 
    go (AppTy fun arg)	 	  = go fun		`thenNF_Tc` \ fun' ->
				    go arg		`thenNF_Tc` \ arg' ->
				    returnNF_Tc (mkAppTy fun' arg')

    go (UsageTy u ty)             = go u                `thenNF_Tc` \ u'  ->
                                    go ty               `thenNF_Tc` \ ty' ->
                                    returnNF_Tc (mkUTy u' ty')

	-- The two interesting cases!
    go (TyVarTy tyvar)     = zonkTyVar unbound_var_fn tyvar

    go (ForAllTy tyvar ty) = zonkTcTyVarToTyVar tyvar	`thenNF_Tc` \ tyvar' ->
			     go ty			`thenNF_Tc` \ ty' ->
			     returnNF_Tc (ForAllTy tyvar' ty')

    go_pred (ClassP c tys) = mapNF_Tc go tys	`thenNF_Tc` \ tys' ->
			     returnNF_Tc (ClassP c tys')
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
  =  tcGetTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
     case maybe_ty of
	  Nothing	-> unbound_var_fn tyvar			-- Mutable and unbound
	  Just other_ty	-> zonkType unbound_var_fn other_ty	-- Bound
\end{code}

