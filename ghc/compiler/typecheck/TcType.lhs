%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcType]{Types used in the typechecker}

\begin{code}
module TcType (
  
  TcTyVar,
  TcTyVarSet,
  newTyVar,
  newTyVarTy,		-- Kind -> NF_TcM s TcType
  newTyVarTys,		-- Int -> Kind -> NF_TcM s [TcType]

  newTyVarTy_OpenKind,	-- NF_TcM s TcType
  newOpenTypeKind,	-- NF_TcM s TcKind

  -----------------------------------------
  TcType, TcTauType, TcThetaType, TcRhoType,

	-- Find the type to which a type variable is bound
  tcPutTyVar,		-- :: TcTyVar -> TcType -> NF_TcM TcType
  tcGetTyVar,		-- :: TcTyVar -> NF_TcM (Maybe TcType)	does shorting out


  tcSplitRhoTy,

  tcInstTyVars,
  tcInstTcType,

  typeToTcType,

  tcTypeKind,		-- :: TcType -> NF_TcM s TcKind
  --------------------------------
  TcKind,
  newKindVar, newKindVars,
  kindToTcKind,
  zonkTcKind,

  --------------------------------
  zonkTcTyVar, zonkTcTyVars, zonkTcTyVarBndr,
  zonkTcType, zonkTcTypes, zonkTcThetaType,

  zonkTcTypeToType, zonkTcTyVarToTyVar,
  zonkTcKindToKind

  ) where

#include "HsVersions.h"


-- friends:
import PprType		( pprType )
import Type		( Type(..), Kind, ThetaType, TyNote(..), 
			  mkAppTy, mkTyConApp,
			  splitDictTy_maybe, splitForAllTys,
			  isTyVarTy, mkTyVarTy, mkTyVarTys, 
			  fullSubstTy, substTopTy, 
			  typeCon, openTypeKind, boxedTypeKind, boxedKind, superKind, superBoxity
			)
import TyCon		( tyConKind )
import VarEnv
import VarSet		( emptyVarSet )
import Var		( TyVar, tyVarKind, tyVarName, isTyVar, isMutTyVar, mkTyVar )

-- others:
import TcMonad
import TysWiredIn	( voidTy )

import Name		( NamedThing(..), setNameUnique, mkSysLocalName )
import Unique		( Unique )
import Util		( nOfThem )
import Outputable
\end{code}



Coercions
~~~~~~~~~~
Type definitions are in TcMonad.lhs

\begin{code}
typeToTcType :: Type -> TcType
typeToTcType ty =  ty

kindToTcKind :: Kind -> TcKind
kindToTcKind kind = kind
\end{code}

Utility functions
~~~~~~~~~~~~~~~~~
These tcSplit functions are like their non-Tc analogues, but they
follow through bound type variables.

No need for tcSplitForAllTy because a type variable can't be instantiated
to a for-all type.

\begin{code}
tcSplitRhoTy :: TcType -> NF_TcM s (TcThetaType, TcType)
tcSplitRhoTy t
  = go t t []
 where
	-- A type variable is never instantiated to a dictionary type,
	-- so we don't need to do a tcReadVar on the "arg".
    go syn_t (FunTy arg res) ts = case splitDictTy_maybe arg of
					Just pair -> go res res (pair:ts)
					Nothing   -> returnNF_Tc (reverse ts, syn_t)
    go syn_t (NoteTy _ t)    ts = go syn_t t ts
    go syn_t (TyVarTy tv)    ts = tcGetTyVar tv		`thenNF_Tc` \ maybe_ty ->
				  case maybe_ty of
				    Just ty | not (isTyVarTy ty) -> go syn_t ty ts
				    other			 -> returnNF_Tc (reverse ts, syn_t)
    go syn_t t		     ts = returnNF_Tc (reverse ts, syn_t)
\end{code}


%************************************************************************
%*									*
\subsection{New type variables}
%*									*
%************************************************************************

\begin{code}
newTyVar :: Kind -> NF_TcM s TcTyVar
newTyVar kind
  = tcGetUnique 	`thenNF_Tc` \ uniq ->
    tcNewMutTyVar (mkSysLocalName uniq SLIT("t")) kind

newTyVarTy  :: Kind -> NF_TcM s TcType
newTyVarTy kind
  = newTyVar kind	`thenNF_Tc` \ tc_tyvar ->
    returnNF_Tc (TyVarTy tc_tyvar)

newTyVarTys :: Int -> Kind -> NF_TcM s [TcType]
newTyVarTys n kind = mapNF_Tc newTyVarTy (nOfThem n kind)

newKindVar :: NF_TcM s TcKind
newKindVar
  = tcGetUnique 						`thenNF_Tc` \ uniq ->
    tcNewMutTyVar (mkSysLocalName uniq SLIT("k")) superKind	`thenNF_Tc` \ kv ->
    returnNF_Tc (TyVarTy kv)

newKindVars :: Int -> NF_TcM s [TcKind]
newKindVars n = mapNF_Tc (\ _ -> newKindVar) (nOfThem n ())

-- Returns a type variable of kind (Type bv) where bv is a new boxity var
-- Used when you need a type variable that's definitely a , but you don't know
-- what kind of type (boxed or unboxed).
newTyVarTy_OpenKind :: NF_TcM s TcType
newTyVarTy_OpenKind = newOpenTypeKind	`thenNF_Tc` \ kind -> 
		      newTyVarTy kind

newOpenTypeKind :: NF_TcM s TcKind
newOpenTypeKind = newTyVarTy superBoxity	`thenNF_Tc` \ bv ->
	   	  returnNF_Tc (mkTyConApp typeCon [bv])
\end{code}


%************************************************************************
%*									*
\subsection{Type instantiation}
%*									*
%************************************************************************

Instantiating a bunch of type variables

\begin{code}
tcInstTyVars :: [TyVar] 
	     -> NF_TcM s ([TcTyVar], [TcType], TyVarEnv TcType)

tcInstTyVars tyvars
  = mapNF_Tc inst_tyvar tyvars	`thenNF_Tc` \ tc_tyvars ->
    let
	tys = mkTyVarTys tc_tyvars
    in
    returnNF_Tc (tc_tyvars, tys, zipVarEnv tyvars tys)

inst_tyvar tyvar	-- Could use the name from the tyvar?
  = tcGetUnique 		`thenNF_Tc` \ uniq ->
    let
	kind = tyVarKind tyvar
	name = setNameUnique (tyVarName tyvar) uniq
	-- Note that we don't change the print-name
	-- This won't confuse the type checker but there's a chance
	-- that two different tyvars will print the same way 
	-- in an error message.  -dppr-debug will show up the difference
	-- Better watch out for this.  If worst comes to worst, just
	-- use mkSysLocalName.
    in
    tcNewMutTyVar name kind
\end{code}

@tcInstTcType@ instantiates the outer-level for-alls of a TcType with
fresh type variables, returning them and the instantiated body of the for-all.

\begin{code}
tcInstTcType :: TcType -> NF_TcM s ([TcTyVar], TcType)
tcInstTcType ty
  = case splitForAllTys ty of
	([], _)       -> returnNF_Tc ([], ty)	-- Nothing to do
	(tyvars, rho) -> tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', _, tenv)  ->
			 returnNF_Tc (tyvars', fullSubstTy tenv emptyVarSet rho)
					-- Since the tyvars are freshly made,
					-- they cannot possibly be captured by
					-- any existing for-alls.  Hence emptyVarSet
\end{code}



%************************************************************************
%*									*
\subsection{Putting and getting  mutable type variables}
%*									*
%************************************************************************

\begin{code}
tcPutTyVar :: TcTyVar -> TcType -> NF_TcM s TcType
tcGetTyVar :: TcTyVar -> NF_TcM s (Maybe TcType)
\end{code}

Putting is easy:

\begin{code}
tcPutTyVar tyvar ty = tcWriteMutTyVar tyvar (Just ty)	`thenNF_Tc_`
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
  = ASSERT2( isMutTyVar tyvar, ppr tyvar )
    tcReadMutTyVar tyvar				`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	Just ty -> short_out ty				`thenNF_Tc` \ ty' ->
		   tcWriteMutTyVar tyvar (Just ty')	`thenNF_Tc_`
		   returnNF_Tc (Just ty')

	Nothing	   -> returnNF_Tc Nothing

short_out :: TcType -> NF_TcM s TcType
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
zonkTcTyVars :: [TcTyVar] -> NF_TcM s [TcType]
zonkTcTyVars tyvars = mapNF_Tc zonkTcTyVar tyvars

zonkTcTyVarBndr :: TcTyVar -> NF_TcM s TcTyVar
zonkTcTyVarBndr tyvar
  = zonkTcTyVar tyvar	`thenNF_Tc` \ (TyVarTy tyvar') ->
    returnNF_Tc tyvar'
	
zonkTcTyVar :: TcTyVar -> NF_TcM s TcType
zonkTcTyVar tyvar = zonkTyVar (\ tv -> returnNF_Tc (TyVarTy tv)) tyvar
\end{code}

-----------------  Types

\begin{code}
zonkTcType :: TcType -> NF_TcM s TcType
zonkTcType ty = zonkType (\ tv -> returnNF_Tc (TyVarTy tv)) ty

zonkTcTypes :: [TcType] -> NF_TcM s [TcType]
zonkTcTypes tys = mapNF_Tc zonkTcType tys

zonkTcThetaType :: TcThetaType -> NF_TcM s TcThetaType
zonkTcThetaType theta = mapNF_Tc zonk theta
		    where
		        zonk (c,ts) = zonkTcTypes ts	`thenNF_Tc` \ new_ts ->
				      returnNF_Tc (c, new_ts)

zonkTcKind :: TcKind -> NF_TcM s TcKind
zonkTcKind = zonkTcType
\end{code}

-------------------  These ...ToType, ...ToKind versions
		     are used at the end of type checking

\begin{code}
zonkTcKindToKind :: TcKind -> NF_TcM s Kind
zonkTcKindToKind kind = zonkType zonk_unbound_kind_var kind
  where
	-- Zonk a mutable but unbound kind variable to
	--	(Type Boxed) 	if it has kind superKind
	--	Boxed		if it has kind superBoxity
    zonk_unbound_kind_var kv
	| super_kind == superKind = tcPutTyVar kv boxedTypeKind
	| otherwise 		  = ASSERT( super_kind == superBoxity )
				    tcPutTyVar kv boxedKind
	where
	  super_kind = tyVarKind kv
			

zonkTcTypeToType :: TcType -> NF_TcM s Type
zonkTcTypeToType ty = zonkType zonk_unbound_tyvar ty
  where
	-- Zonk a mutable but unbound type variable to
	--	Void		if it has kind (Type Boxed)
	--	Voidxxx		otherwise
    zonk_unbound_tyvar tv
	= zonkTcKindToKind (tyVarKind tv)	`thenNF_Tc` \ kind ->
	  if kind == boxedTypeKind then
		tcPutTyVar tv voidTy	-- Just to avoid creating a new tycon in
					-- this vastly common case
	  else
		tcPutTyVar tv (TyConApp (mk_void_tycon tv) [])

    mk_void_tycon tv	-- Make a new TyCon with the same kind as the 
			-- type variable tv.  Same name too, apart from
			-- making it start with a capital letter (sigh)
			-- I can't quite bring myself to write the Name-fiddling
			-- code yet.  ToDo.  SLPJ Nov 98
	= pprPanic "zonkTcTypeToType: free type variable with non-* type:" (ppr tv)


-- zonkTcTyVarToTyVar is applied to the *binding* occurrence 
-- of a type variable, at the *end* of type checking.
-- It zonks the type variable, to get a mutable, but unbound, tyvar, tv;
-- zonks its kind, and then makes an immutable version of tv and binds tv to it.
-- Now any bound occurences of the original type variable will get 
-- zonked to the immutable version.

zonkTcTyVarToTyVar :: TcTyVar -> NF_TcM s TyVar
zonkTcTyVarToTyVar tv
  = zonkTcKindToKind (tyVarKind tv)	`thenNF_Tc` \ kind ->
    let
		-- Make an immutable version
	immut_tv    = mkTyVar (tyVarName tv) kind
	immut_tv_ty = mkTyVarTy immut_tv

        zap tv = tcPutTyVar tv immut_tv_ty
		-- Bind the mutable version to the immutable one
    in 
	-- If the type variable is mutable, then bind it to immut_tv_ty
	-- so that all other occurrences of the tyvar will get zapped too
    zonkTyVar zap tv		`thenNF_Tc` \ ty2 ->
    ASSERT2( immut_tv_ty == ty2, ppr tv $$ ppr immut_tv $$ ppr ty2 )

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

zonkType :: (TcTyVar -> NF_TcM s Type) 	-- What to do with unbound mutable type variables
					-- see zonkTcType, and zonkTcTypeToType
	 -> TcType
	 -> NF_TcM s Type
zonkType unbound_var_fn ty
  = go ty
  where
    go (TyConApp tycon tys)	  = mapNF_Tc go tys	`thenNF_Tc` \ tys' ->
				    returnNF_Tc (TyConApp tycon tys')

    go (NoteTy (SynNote ty1) ty2) = go ty1		`thenNF_Tc` \ ty1' ->
				    go ty2		`thenNF_Tc` \ ty2' ->
				    returnNF_Tc (NoteTy (SynNote ty1') ty2')

    go (NoteTy (FTVNote _) ty2)   = go ty2	-- Discard free-tyvar annotations

    go (FunTy arg res)      	  = go arg		`thenNF_Tc` \ arg' ->
				    go res		`thenNF_Tc` \ res' ->
				    returnNF_Tc (FunTy arg' res')
 
    go (AppTy fun arg)	 	  = go fun		`thenNF_Tc` \ fun' ->
				    go arg		`thenNF_Tc` \ arg' ->
				    returnNF_Tc (mkAppTy fun' arg')

	-- The two interesting cases!
    go (TyVarTy tyvar)  	  = zonkTyVar unbound_var_fn tyvar

    go (ForAllTy tyvar ty)
	= zonkTcTyVarToTyVar tyvar	`thenNF_Tc` \ tyvar' ->
	  go ty				`thenNF_Tc` \ ty' ->
	  returnNF_Tc (ForAllTy tyvar' ty')


zonkTyVar :: (TcTyVar -> NF_TcM s Type)		-- What to do for an unbound mutable variable
	  -> TcTyVar -> NF_TcM s TcType
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

%************************************************************************
%*									*
\subsection{tcTypeKind}
%*									*
%************************************************************************

Sadly, we need a Tc version of typeKind, that looks though mutable
kind variables.  See the notes with Type.typeKind for the typeKindF nonsense

This is pretty gruesome.

\begin{code}
tcTypeKind :: TcType -> NF_TcM s TcKind

tcTypeKind (TyVarTy tyvar)	= returnNF_Tc (tyVarKind tyvar)
tcTypeKind (TyConApp tycon tys)	= foldlTc (\k _ -> tcFunResultTy k) (tyConKind tycon) tys
tcTypeKind (NoteTy _ ty)	= tcTypeKind ty
tcTypeKind (AppTy fun arg)	= tcTypeKind fun	`thenNF_Tc` \ fun_kind ->
				  tcFunResultTy fun_kind
tcTypeKind (FunTy fun arg)	= tcTypeKindF arg
tcTypeKind (ForAllTy _ ty)	= tcTypeKindF ty

tcTypeKindF :: TcType -> NF_TcM s TcKind
tcTypeKindF (NoteTy _ ty)   = tcTypeKindF ty
tcTypeKindF (FunTy _ ty)    = tcTypeKindF ty
tcTypeKindF (ForAllTy _ ty) = tcTypeKindF ty
tcTypeKindF other	    = tcTypeKind other	`thenNF_Tc` \ kind ->
			      fix_up kind
  where
    fix_up (TyConApp kc _) | kc == typeCon = returnNF_Tc boxedTypeKind
		-- Functions at the type level are always boxed
    fix_up (NoteTy _ kind)   = fix_up kind
    fix_up kind@(TyVarTy tv) = tcGetTyVar tv	`thenNF_Tc` \ maybe_ty ->
			       case maybe_ty of
				  Just kind' -> fix_up kind'
				  Nothing  -> returnNF_Tc kind
    fix_up kind              = returnNF_Tc kind

tcFunResultTy (NoteTy _ ty)   = tcFunResultTy ty
tcFunResultTy (FunTy arg res) = returnNF_Tc res
tcFunResultTy (TyVarTy tv)    = tcGetTyVar tv	`thenNF_Tc` \ maybe_ty ->
			        case maybe_ty of
				  Just ty' -> tcFunResultTy ty'
	-- The Nothing case, and the other cases for tcFunResultTy
	-- should never happen... pattern match failure
\end{code}
