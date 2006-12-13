%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Monadic type operations

This module contains monadic operations over types that contain
mutable type variables

\begin{code}
module TcMType (
  TcTyVar, TcKind, TcType, TcTauType, TcThetaType, TcTyVarSet,

  --------------------------------
  -- Creating new mutable type variables
  newFlexiTyVar,
  newFlexiTyVarTy,		-- Kind -> TcM TcType
  newFlexiTyVarTys,		-- Int -> Kind -> TcM [TcType]
  newKindVar, newKindVars, 
  lookupTcTyVar, LookupTyVarResult(..),
  newMetaTyVar, readMetaTyVar, writeMetaTyVar, 

  --------------------------------
  -- Boxy type variables
  newBoxyTyVar, newBoxyTyVars, newBoxyTyVarTys, readFilledBox, 

  --------------------------------
  -- Creating new coercion variables
  newCoVars,

  --------------------------------
  -- Instantiation
  tcInstTyVar, tcInstType, tcInstTyVars, tcInstBoxyTyVar,
  tcInstSigTyVars, zonkSigTyVar,
  tcInstSkolTyVar, tcInstSkolTyVars, tcInstSkolType, 
  tcSkolSigType, tcSkolSigTyVars,

  --------------------------------
  -- Checking type validity
  Rank, UserTypeCtxt(..), checkValidType, 
  SourceTyCtxt(..), checkValidTheta, checkFreeness,
  checkValidInstHead, checkValidInstance, checkAmbiguity,
  checkInstTermination,
  arityErr, 

  --------------------------------
  -- Zonking
  zonkType, zonkTcPredType, 
  zonkTcTyVar, zonkTcTyVars, zonkTcTyVarsAndFV, 
  zonkQuantifiedTyVar, zonkQuantifiedTyVars,
  zonkTcType, zonkTcTypes, zonkTcClassConstraints, zonkTcThetaType,
  zonkTcKindToKind, zonkTcKind, zonkTopTyVar,

  readKindVar, writeKindVar

  ) where

#include "HsVersions.h"

-- friends:
import TypeRep
import TcType
import Type
import Coercion
import Class
import TyCon
import Var

-- others:
import TcRnMonad          -- TcType, amongst others
import FunDeps
import Name
import VarSet
import ErrUtils
import DynFlags
import Util
import Maybes
import ListSetOps
import UniqSupply
import SrcLoc
import Outputable

import Control.Monad	( when )
import Data.List	( (\\) )
\end{code}


%************************************************************************
%*									*
	Instantiation in general
%*									*
%************************************************************************

\begin{code}
tcInstType :: ([TyVar] -> TcM [TcTyVar]) 		-- How to instantiate the type variables
	   -> TcType 					-- Type to instantiate
	   -> TcM ([TcTyVar], TcThetaType, TcType)	-- Result
tcInstType inst_tyvars ty
  = case tcSplitForAllTys ty of
	([],     rho) -> let	-- There may be overloading despite no type variables;
				-- 	(?x :: Int) => Int -> Int
			   (theta, tau) = tcSplitPhiTy rho
			 in
			 return ([], theta, tau)

	(tyvars, rho) -> do { tyvars' <- inst_tyvars tyvars

			    ; let  tenv = zipTopTvSubst tyvars (mkTyVarTys tyvars')
				-- Either the tyvars are freshly made, by inst_tyvars,
				-- or (in the call from tcSkolSigType) any nested foralls
				-- have different binders.  Either way, zipTopTvSubst is ok

			    ; let  (theta, tau) = tcSplitPhiTy (substTy tenv rho)
			    ; return (tyvars', theta, tau) }
\end{code}


%************************************************************************
%*									*
	Kind variables
%*									*
%************************************************************************

\begin{code}
newCoVars :: [(TcType,TcType)] -> TcM [CoVar]
newCoVars spec
  = do	{ us <- newUniqueSupply 
	; return [ mkCoVar (mkSysTvName uniq FSLIT("co"))
			   (mkCoKind ty1 ty2)
		 | ((ty1,ty2), uniq) <- spec `zip` uniqsFromSupply us] }

newKindVar :: TcM TcKind
newKindVar = do	{ uniq <- newUnique
		; ref <- newMutVar Flexi
		; return (mkTyVarTy (mkKindVar uniq ref)) }

newKindVars :: Int -> TcM [TcKind]
newKindVars n = mappM (\ _ -> newKindVar) (nOfThem n ())
\end{code}


%************************************************************************
%*									*
	SkolemTvs (immutable)
%*									*
%************************************************************************

\begin{code}
mkSkolTyVar :: Name -> Kind -> SkolemInfo -> TcTyVar
mkSkolTyVar name kind info = mkTcTyVar name kind (SkolemTv info)

tcSkolSigType :: SkolemInfo -> Type -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type signature with skolem constants, but 
-- do *not* give them fresh names, because we want the name to
-- be in the type environment -- it is lexically scoped.
tcSkolSigType info ty = tcInstType (\tvs -> return (tcSkolSigTyVars info tvs)) ty

tcSkolSigTyVars :: SkolemInfo -> [TyVar] -> [TcTyVar]
-- Make skolem constants, but do *not* give them new names, as above
tcSkolSigTyVars info tyvars = [ mkSkolTyVar (tyVarName tv) (tyVarKind tv) info
			      | tv <- tyvars ]

tcInstSkolTyVar :: SkolemInfo -> Maybe SrcLoc -> TyVar -> TcM TcTyVar
-- Instantiate the tyvar, using 
--	* the occ-name and kind of the supplied tyvar, 
--	* the unique from the monad,
--	* the location either from the tyvar (mb_loc = Nothing)
--	  or from mb_loc (Just loc)
tcInstSkolTyVar info mb_loc tyvar
  = do	{ uniq <- newUnique
	; let old_name = tyVarName tyvar
	      kind     = tyVarKind tyvar
	      loc      = mb_loc `orElse` getSrcLoc old_name
	      new_name = mkInternalName uniq (nameOccName old_name) loc
	; return (mkSkolTyVar new_name kind info) }

tcInstSkolTyVars :: SkolemInfo -> [TyVar] -> TcM [TcTyVar]
-- Get the location from the monad
tcInstSkolTyVars info tyvars 
  = do 	{ span <- getSrcSpanM
	; mapM (tcInstSkolTyVar info (Just (srcSpanStart span))) tyvars }

tcInstSkolType :: SkolemInfo -> TcType -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type with fresh skolem constants
-- Binding location comes from the monad
tcInstSkolType info ty = tcInstType (tcInstSkolTyVars info) ty
\end{code}


%************************************************************************
%*									*
	MetaTvs (meta type variables; mutable)
%*									*
%************************************************************************

\begin{code}
newMetaTyVar :: BoxInfo -> Kind -> TcM TcTyVar
-- Make a new meta tyvar out of thin air
newMetaTyVar box_info kind
  = do	{ uniq <- newUnique
 	; ref <- newMutVar Flexi ;
	; let name = mkSysTvName uniq fs 
	      fs = case box_info of
			BoxTv   -> FSLIT("t")
			TauTv   -> FSLIT("t")
			SigTv _ -> FSLIT("a")
		-- We give BoxTv and TauTv the same string, because
		-- otherwise we get user-visible differences in error
		-- messages, which are confusing.  If you want to see
		-- the box_info of each tyvar, use -dppr-debug
	; return (mkTcTyVar name kind (MetaTv box_info ref)) }

instMetaTyVar :: BoxInfo -> TyVar -> TcM TcTyVar
-- Make a new meta tyvar whose Name and Kind 
-- come from an existing TyVar
instMetaTyVar box_info tyvar
  = do	{ uniq <- newUnique
 	; ref <- newMutVar Flexi ;
	; let name = setNameUnique (tyVarName tyvar) uniq
	      kind = tyVarKind tyvar
	; return (mkTcTyVar name kind (MetaTv box_info ref)) }

readMetaTyVar :: TyVar -> TcM MetaDetails
readMetaTyVar tyvar = ASSERT2( isMetaTyVar tyvar, ppr tyvar )
		      readMutVar (metaTvRef tyvar)

writeMetaTyVar :: TcTyVar -> TcType -> TcM ()
#ifndef DEBUG
writeMetaTyVar tyvar ty = writeMutVar (metaTvRef tyvar) (Indirect ty)
#else
writeMetaTyVar tyvar ty
  | not (isMetaTyVar tyvar)
  = pprTrace "writeMetaTyVar" (ppr tyvar) $
    returnM ()

  | otherwise
  = ASSERT( isMetaTyVar tyvar )
    ASSERT2( k2 `isSubKind` k1, (ppr tyvar <+> ppr k1) $$ (ppr ty <+> ppr k2) )
    do	{ ASSERTM2( do { details <- readMetaTyVar tyvar; return (isFlexi details) }, ppr tyvar )
	; writeMutVar (metaTvRef tyvar) (Indirect ty) }
  where
    k1 = tyVarKind tyvar
    k2 = typeKind ty
#endif
\end{code}


%************************************************************************
%*									*
	MetaTvs: TauTvs
%*									*
%************************************************************************

\begin{code}
newFlexiTyVar :: Kind -> TcM TcTyVar
newFlexiTyVar kind = newMetaTyVar TauTv kind

newFlexiTyVarTy  :: Kind -> TcM TcType
newFlexiTyVarTy kind
  = newFlexiTyVar kind	`thenM` \ tc_tyvar ->
    returnM (TyVarTy tc_tyvar)

newFlexiTyVarTys :: Int -> Kind -> TcM [TcType]
newFlexiTyVarTys n kind = mappM newFlexiTyVarTy (nOfThem n kind)

tcInstTyVar :: TyVar -> TcM TcTyVar
-- Instantiate with a META type variable
tcInstTyVar tyvar = instMetaTyVar TauTv tyvar

tcInstTyVars :: [TyVar] -> TcM ([TcTyVar], [TcType], TvSubst)
-- Instantiate with META type variables
tcInstTyVars tyvars
  = do	{ tc_tvs <- mapM tcInstTyVar tyvars
	; let tys = mkTyVarTys tc_tvs
	; returnM (tc_tvs, tys, zipTopTvSubst tyvars tys) }
		-- Since the tyvars are freshly made,
		-- they cannot possibly be captured by
		-- any existing for-alls.  Hence zipTopTvSubst
\end{code}


%************************************************************************
%*									*
	MetaTvs: SigTvs
%*									*
%************************************************************************

\begin{code}
tcInstSigTyVars :: Bool -> SkolemInfo -> [TyVar] -> TcM [TcTyVar]
-- Instantiate with skolems or meta SigTvs; depending on use_skols
-- Always take location info from the supplied tyvars
tcInstSigTyVars use_skols skol_info tyvars 
  | use_skols
  = mapM (tcInstSkolTyVar skol_info Nothing) tyvars

  | otherwise
  = mapM (instMetaTyVar (SigTv skol_info)) tyvars

zonkSigTyVar :: TcTyVar -> TcM TcTyVar
zonkSigTyVar sig_tv 
  | isSkolemTyVar sig_tv 
  = return sig_tv	-- Happens in the call in TcBinds.checkDistinctTyVars
  | otherwise
  = ASSERT( isSigTyVar sig_tv )
    do { ty <- zonkTcTyVar sig_tv
       ; return (tcGetTyVar "zonkSigTyVar" ty) }
	-- 'ty' is bound to be a type variable, because SigTvs
	-- can only be unified with type variables
\end{code}


%************************************************************************
%*									*
	MetaTvs: BoxTvs
%*									*
%************************************************************************

\begin{code}
newBoxyTyVar :: Kind -> TcM BoxyTyVar
newBoxyTyVar kind = newMetaTyVar BoxTv kind

newBoxyTyVars :: [Kind] -> TcM [BoxyTyVar]
newBoxyTyVars kinds = mapM newBoxyTyVar kinds

newBoxyTyVarTys :: [Kind] -> TcM [BoxyType]
newBoxyTyVarTys kinds = do { tvs <- mapM newBoxyTyVar kinds; return (mkTyVarTys tvs) }

readFilledBox :: BoxyTyVar -> TcM TcType
-- Read the contents of the box, which should be filled in by now
readFilledBox box_tv = ASSERT( isBoxyTyVar box_tv )
		       do { cts <- readMetaTyVar box_tv
		 	  ; case cts of
				Flexi 	    -> pprPanic "readFilledBox" (ppr box_tv)
				Indirect ty -> return ty }

tcInstBoxyTyVar :: TyVar -> TcM BoxyTyVar
-- Instantiate with a BOXY type variable
tcInstBoxyTyVar tyvar = instMetaTyVar BoxTv tyvar
\end{code}


%************************************************************************
%*									*
\subsection{Putting and getting  mutable type variables}
%*									*
%************************************************************************

But it's more fun to short out indirections on the way: If this
version returns a TyVar, then that TyVar is unbound.  If it returns
any other type, then there might be bound TyVars embedded inside it.

We return Nothing iff the original box was unbound.

\begin{code}
data LookupTyVarResult	-- The result of a lookupTcTyVar call
  = DoneTv TcTyVarDetails	-- SkolemTv or virgin MetaTv
  | IndirectTv TcType

lookupTcTyVar :: TcTyVar -> TcM LookupTyVarResult
lookupTcTyVar tyvar 
  = ASSERT2( isTcTyVar tyvar, ppr tyvar )
    case details of
      SkolemTv _   -> return (DoneTv details)
      MetaTv _ ref -> do { meta_details <- readMutVar ref
			 ; case meta_details of
			    Indirect ty -> return (IndirectTv ty)
			    Flexi       -> return (DoneTv details) }
  where
    details =  tcTyVarDetails tyvar

{- 
-- gaw 2004 We aren't shorting anything out anymore, at least for now
getTcTyVar tyvar
  | not (isTcTyVar tyvar)
  = pprTrace "getTcTyVar" (ppr tyvar) $
    returnM (Just (mkTyVarTy tyvar))

  | otherwise
  = ASSERT2( isTcTyVar tyvar, ppr tyvar )
    readMetaTyVar tyvar				`thenM` \ maybe_ty ->
    case maybe_ty of
	Just ty -> short_out ty				`thenM` \ ty' ->
		   writeMetaTyVar tyvar (Just ty')	`thenM_`
		   returnM (Just ty')

	Nothing	   -> returnM Nothing

short_out :: TcType -> TcM TcType
short_out ty@(TyVarTy tyvar)
  | not (isTcTyVar tyvar)
  = returnM ty

  | otherwise
  = readMetaTyVar tyvar	`thenM` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> short_out ty' 			`thenM` \ ty' ->
		    writeMetaTyVar tyvar (Just ty')	`thenM_`
		    returnM ty'

	other    -> returnM ty

short_out other_ty = returnM other_ty
-}
\end{code}


%************************************************************************
%*									*
\subsection{Zonking -- the exernal interfaces}
%*									*
%************************************************************************

-----------------  Type variables

\begin{code}
zonkTcTyVars :: [TcTyVar] -> TcM [TcType]
zonkTcTyVars tyvars = mappM zonkTcTyVar tyvars

zonkTcTyVarsAndFV :: [TcTyVar] -> TcM TcTyVarSet
zonkTcTyVarsAndFV tyvars = mappM zonkTcTyVar tyvars	`thenM` \ tys ->
			   returnM (tyVarsOfTypes tys)

zonkTcTyVar :: TcTyVar -> TcM TcType
zonkTcTyVar tyvar = ASSERT( isTcTyVar tyvar )
		    zonk_tc_tyvar (\ tv -> returnM (TyVarTy tv)) tyvar
\end{code}

-----------------  Types

\begin{code}
zonkTcType :: TcType -> TcM TcType
zonkTcType ty = zonkType (\ tv -> returnM (TyVarTy tv)) ty

zonkTcTypes :: [TcType] -> TcM [TcType]
zonkTcTypes tys = mappM zonkTcType tys

zonkTcClassConstraints cts = mappM zonk cts
    where zonk (clas, tys)
	    = zonkTcTypes tys	`thenM` \ new_tys ->
	      returnM (clas, new_tys)

zonkTcThetaType :: TcThetaType -> TcM TcThetaType
zonkTcThetaType theta = mappM zonkTcPredType theta

zonkTcPredType :: TcPredType -> TcM TcPredType
zonkTcPredType (ClassP c ts)
  = zonkTcTypes ts	`thenM` \ new_ts ->
    returnM (ClassP c new_ts)
zonkTcPredType (IParam n t)
  = zonkTcType t	`thenM` \ new_t ->
    returnM (IParam n new_t)
zonkTcPredType (EqPred t1 t2)
  = zonkTcType t1	`thenM` \ new_t1 ->
    zonkTcType t2	`thenM` \ new_t2 ->
    returnM (EqPred new_t1 new_t2)
\end{code}

-------------------  These ...ToType, ...ToKind versions
		     are used at the end of type checking

\begin{code}
zonkTopTyVar :: TcTyVar -> TcM TcTyVar
-- zonkTopTyVar is used, at the top level, on any un-instantiated meta type variables
-- to default the kind of ? and ?? etc to *.  This is important to ensure that
-- instance declarations match.  For example consider
--	instance Show (a->b)
--	foo x = show (\_ -> True)
-- Then we'll get a constraint (Show (p ->q)) where p has argTypeKind (printed ??), 
-- and that won't match the typeKind (*) in the instance decl.
--
-- Because we are at top level, no further constraints are going to affect these
-- type variables, so it's time to do it by hand.  However we aren't ready
-- to default them fully to () or whatever, because the type-class defaulting
-- rules have yet to run.

zonkTopTyVar tv
  | k `eqKind` default_k = return tv
  | otherwise
  = do	{ tv' <- newFlexiTyVar default_k
	; writeMetaTyVar tv (mkTyVarTy tv') 
	; return tv' }
  where
    k = tyVarKind tv
    default_k = defaultKind k

zonkQuantifiedTyVars :: [TcTyVar] -> TcM [TyVar]
zonkQuantifiedTyVars = mappM zonkQuantifiedTyVar

zonkQuantifiedTyVar :: TcTyVar -> TcM TyVar
-- zonkQuantifiedTyVar is applied to the a TcTyVar when quantifying over it.
--
-- The quantified type variables often include meta type variables
-- we want to freeze them into ordinary type variables, and
-- default their kind (e.g. from OpenTypeKind to TypeKind)
-- 			-- see notes with Kind.defaultKind
-- The meta tyvar is updated to point to the new regular TyVar.  Now any 
-- bound occurences of the original type variable will get zonked to 
-- the immutable version.
--
-- We leave skolem TyVars alone; they are immutable.
zonkQuantifiedTyVar tv
  | ASSERT( isTcTyVar tv ) 
    isSkolemTyVar tv = return tv
	-- It might be a skolem type variable, 
	-- for example from a user type signature

  | otherwise	-- It's a meta-type-variable
  = do	{ details <- readMetaTyVar tv

	-- Create the new, frozen, regular type variable
	; let final_kind = defaultKind (tyVarKind tv)
	      final_tv   = mkTyVar (tyVarName tv) final_kind

	-- Bind the meta tyvar to the new tyvar
	; case details of
	    Indirect ty -> WARN( True, ppr tv $$ ppr ty ) 
			   return ()
		-- [Sept 04] I don't think this should happen
		-- See note [Silly Type Synonym]

	    Flexi -> writeMetaTyVar tv (mkTyVarTy final_tv)

	-- Return the new tyvar
	; return final_tv }
\end{code}

[Silly Type Synonyms]

Consider this:
	type C u a = u	-- Note 'a' unused

	foo :: (forall a. C u a -> C u a) -> u
	foo x = ...

	bar :: Num u => u
	bar = foo (\t -> t + t)

* From the (\t -> t+t) we get type  {Num d} =>  d -> d
  where d is fresh.

* Now unify with type of foo's arg, and we get:
	{Num (C d a)} =>  C d a -> C d a
  where a is fresh.

* Now abstract over the 'a', but float out the Num (C d a) constraint
  because it does not 'really' mention a.  (see exactTyVarsOfType)
  The arg to foo becomes
	/\a -> \t -> t+t

* So we get a dict binding for Num (C d a), which is zonked to give
	a = ()
  [Note Sept 04: now that we are zonking quantified type variables
  on construction, the 'a' will be frozen as a regular tyvar on
  quantification, so the floated dict will still have type (C d a).
  Which renders this whole note moot; happily!]

* Then the /\a abstraction has a zonked 'a' in it.

All very silly.   I think its harmless to ignore the problem.  We'll end up with
a /\a in the final result but all the occurrences of a will be zonked to ()


%************************************************************************
%*									*
\subsection{Zonking -- the main work-horses: zonkType, zonkTyVar}
%*									*
%*		For internal use only!					*
%*									*
%************************************************************************

\begin{code}
-- For unbound, mutable tyvars, zonkType uses the function given to it
-- For tyvars bound at a for-all, zonkType zonks them to an immutable
--	type variable and zonks the kind too

zonkType :: (TcTyVar -> TcM Type) 	-- What to do with unbound mutable type variables
					-- see zonkTcType, and zonkTcTypeToType
         -> TcType
	 -> TcM Type
zonkType unbound_var_fn ty
  = go ty
  where
    go (NoteTy _ ty2) 	 = go ty2	-- Discard free-tyvar annotations
			 
    go (TyConApp tc tys) = mappM go tys	`thenM` \ tys' ->
			   returnM (TyConApp tc tys')
			    
    go (PredTy p)	 = go_pred p		`thenM` \ p' ->
			   returnM (PredTy p')
			 
    go (FunTy arg res)   = go arg		`thenM` \ arg' ->
			   go res		`thenM` \ res' ->
			   returnM (FunTy arg' res')
 			 
    go (AppTy fun arg)	 = go fun		`thenM` \ fun' ->
			   go arg		`thenM` \ arg' ->
			   returnM (mkAppTy fun' arg')
		-- NB the mkAppTy; we might have instantiated a
		-- type variable to a type constructor, so we need
		-- to pull the TyConApp to the top.

	-- The two interesting cases!
    go (TyVarTy tyvar) | isTcTyVar tyvar = zonk_tc_tyvar unbound_var_fn tyvar
		       | otherwise	 = return (TyVarTy tyvar)
		-- Ordinary (non Tc) tyvars occur inside quantified types

    go (ForAllTy tyvar ty) = ASSERT( isImmutableTyVar tyvar )
			     go ty		`thenM` \ ty' ->
			     returnM (ForAllTy tyvar ty')

    go_pred (ClassP c tys)   = mappM go tys	`thenM` \ tys' ->
			       returnM (ClassP c tys')
    go_pred (IParam n ty)    = go ty		`thenM` \ ty' ->
			       returnM (IParam n ty')
    go_pred (EqPred ty1 ty2) = go ty1		`thenM` \ ty1' ->
			       go ty2		`thenM` \ ty2' ->
			       returnM (EqPred ty1' ty2')

zonk_tc_tyvar :: (TcTyVar -> TcM Type)		-- What to do for an unbound mutable variable
 	      -> TcTyVar -> TcM TcType
zonk_tc_tyvar unbound_var_fn tyvar 
  | not (isMetaTyVar tyvar)	-- Skolems
  = returnM (TyVarTy tyvar)

  | otherwise			-- Mutables
  = do	{ cts <- readMetaTyVar tyvar
	; case cts of
	    Flexi       -> unbound_var_fn tyvar    -- Unbound meta type variable
	    Indirect ty -> zonkType unbound_var_fn ty  }
\end{code}



%************************************************************************
%*									*
			Zonking kinds
%*									*
%************************************************************************

\begin{code}
readKindVar  :: KindVar -> TcM (MetaDetails)
writeKindVar :: KindVar -> TcKind -> TcM ()
readKindVar  kv = readMutVar (kindVarRef kv)
writeKindVar kv val = writeMutVar (kindVarRef kv) (Indirect val)

-------------
zonkTcKind :: TcKind -> TcM TcKind
zonkTcKind k = zonkTcType k

-------------
zonkTcKindToKind :: TcKind -> TcM Kind
-- When zonking a TcKind to a kind, we need to instantiate kind variables,
-- Haskell specifies that * is to be used, so we follow that.
zonkTcKindToKind k = zonkType (\ _ -> return liftedTypeKind) k
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
checkValidType :: UserTypeCtxt -> Type -> TcM ()
-- Checks that the type is valid for the given context
checkValidType ctxt ty
  = traceTc (text "checkValidType" <+> ppr ty)	`thenM_`
    doptM Opt_GlasgowExts	`thenM` \ gla_exts ->
    let 
	rank | gla_exts = Arbitrary
	     | otherwise
	     = case ctxt of	-- Haskell 98
		 GenPatCtxt	-> Rank 0
		 LamPatSigCtxt	-> Rank 0
		 BindPatSigCtxt	-> Rank 0
		 DefaultDeclCtxt-> Rank 0
		 ResSigCtxt	-> Rank 0
		 TySynCtxt _    -> Rank 0
		 ExprSigCtxt 	-> Rank 1
		 FunSigCtxt _   -> Rank 1
		 ConArgCtxt _   -> Rank 1	-- We are given the type of the entire
						-- constructor, hence rank 1
		 ForSigCtxt _	-> Rank 1
		 SpecInstCtxt   -> Rank 1

	actual_kind = typeKind ty

	kind_ok = case ctxt of
			TySynCtxt _  -> True	-- Any kind will do
			ResSigCtxt   -> isSubOpenTypeKind 	 actual_kind
			ExprSigCtxt  -> isSubOpenTypeKind 	 actual_kind
			GenPatCtxt   -> isLiftedTypeKind actual_kind
			ForSigCtxt _ -> isLiftedTypeKind actual_kind
			other	     -> isSubArgTypeKind    actual_kind
	
	ubx_tup | not gla_exts = UT_NotOk
		| otherwise    = case ctxt of
				   TySynCtxt _ -> UT_Ok
				   ExprSigCtxt -> UT_Ok
				   other       -> UT_NotOk
		-- Unboxed tuples ok in function results,
		-- but for type synonyms we allow them even at
		-- top level
    in
	-- Check that the thing has kind Type, and is lifted if necessary
    checkTc kind_ok (kindErr actual_kind)	`thenM_`

	-- Check the internal validity of the type itself
    check_poly_type rank ubx_tup ty		`thenM_`

    traceTc (text "checkValidType done" <+> ppr ty)
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
  | null tvs && null theta
  = check_tau_type rank ubx_tup ty
  | otherwise
  = do	{ check_valid_theta SigmaCtxt theta
	; check_poly_type rank ubx_tup tau	-- Allow foralls to right of arrow
	; checkFreeness tvs theta
	; checkAmbiguity tvs theta (tyVarsOfType tau) }
  where
    (tvs, theta, tau) = tcSplitSigmaTy ty
   
----------------------------------------
check_arg_type :: Type -> TcM ()
-- The sort of type that can instantiate a type variable,
-- or be the argument of a type constructor.
-- Not an unboxed tuple, but now *can* be a forall (since impredicativity)
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
  = check_poly_type Arbitrary UT_NotOk ty	`thenM_` 
    checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty)

----------------------------------------
check_tau_type :: Rank -> UbxTupFlag -> Type -> TcM ()
-- Rank is allowed rank for function args
-- No foralls otherwise

check_tau_type rank ubx_tup ty@(ForAllTy _ _)       = failWithTc (forAllTyErr ty)
check_tau_type rank ubx_tup ty@(FunTy (PredTy _) _) = failWithTc (forAllTyErr ty)
	-- Reject e.g. (Maybe (?x::Int => Int)), with a decent error message

-- Naked PredTys don't usually show up, but they can as a result of
--	{-# SPECIALISE instance Ord Char #-}
-- The Right Thing would be to fix the way that SPECIALISE instance pragmas
-- are handled, but the quick thing is just to permit PredTys here.
check_tau_type rank ubx_tup (PredTy sty) = getDOpts		`thenM` \ dflags ->
					   check_pred_ty dflags TypeCtxt sty

check_tau_type rank ubx_tup (TyVarTy _)       = returnM ()
check_tau_type rank ubx_tup ty@(FunTy arg_ty res_ty)
  = check_poly_type (decRank rank) UT_NotOk arg_ty	`thenM_`
    check_poly_type rank 	   UT_Ok    res_ty

check_tau_type rank ubx_tup (AppTy ty1 ty2)
  = check_arg_type ty1 `thenM_` check_arg_type ty2

check_tau_type rank ubx_tup (NoteTy other_note ty)
  = check_tau_type rank ubx_tup ty

check_tau_type rank ubx_tup ty@(TyConApp tc tys)
  | isSynTyCon tc	
  = do	{ 	-- It's OK to have an *over-applied* type synonym
		--	data Tree a b = ...
		--	type Foo a = Tree [a]
		--	f :: Foo a b -> ...
	; case tcView ty of
	     Just ty' -> check_tau_type rank ubx_tup ty'	-- Check expansion
	     Nothing  -> failWithTc arity_msg

	; gla_exts <- doptM Opt_GlasgowExts
	; if gla_exts then
	-- If -fglasgow-exts then don't check the type arguments
	-- This allows us to instantiate a synonym defn with a 
	-- for-all type, or with a partially-applied type synonym.
	-- 	e.g.   type T a b = a
	--	       type S m   = m ()
	--	       f :: S (T Int)
	-- Here, T is partially applied, so it's illegal in H98.
	-- But if you expand S first, then T we get just 
	--	       f :: Int
	-- which is fine.
		returnM ()
	  else
		-- For H98, do check the type args
		mappM_ check_arg_type tys
	}
    
  | isUnboxedTupleTyCon tc
  = doptM Opt_GlasgowExts			`thenM` \ gla_exts ->
    checkTc (ubx_tup_ok gla_exts) ubx_tup_msg	`thenM_`
    mappM_ (check_tau_type (Rank 0) UT_Ok) tys	
		-- Args are allowed to be unlifted, or
		-- more unboxed tuples, so can't use check_arg_ty

  | otherwise
  = mappM_ check_arg_type tys

  where
    ubx_tup_ok gla_exts = case ubx_tup of { UT_Ok -> gla_exts; other -> False }

    n_args    = length tys
    tc_arity  = tyConArity tc

    arity_msg   = arityErr "Type synonym" (tyConName tc) tc_arity n_args
    ubx_tup_msg = ubxArgTyErr ty

----------------------------------------
forAllTyErr     ty = ptext SLIT("Illegal polymorphic or qualified type:") <+> ppr ty
unliftedArgErr  ty = ptext SLIT("Illegal unlifted type argument:") <+> ppr ty
ubxArgTyErr     ty = ptext SLIT("Illegal unboxed tuple type as function argument:") <+> ppr ty
kindErr kind       = ptext SLIT("Expecting an ordinary type, but found a type of kind") <+> ppr kind
\end{code}



%************************************************************************
%*									*
\subsection{Checking a theta or source type}
%*									*
%************************************************************************

\begin{code}
-- Enumerate the contexts in which a "source type", <S>, can occur
--	Eq a 
-- or 	?x::Int
-- or 	r <: {x::Int}
-- or 	(N a) where N is a newtype

data SourceTyCtxt
  = ClassSCCtxt Name	-- Superclasses of clas
			-- 	class <S> => C a where ...
  | SigmaCtxt		-- Theta part of a normal for-all type
			--	f :: <S> => a -> a
  | DataTyCtxt Name	-- Theta part of a data decl
			--	data <S> => T a = MkT a
  | TypeCtxt 		-- Source type in an ordinary type
			-- 	f :: N a -> N a
  | InstThetaCtxt	-- Context of an instance decl
			--	instance <S> => C [a] where ...
		
pprSourceTyCtxt (ClassSCCtxt c) = ptext SLIT("the super-classes of class") <+> quotes (ppr c)
pprSourceTyCtxt SigmaCtxt       = ptext SLIT("the context of a polymorphic type")
pprSourceTyCtxt (DataTyCtxt tc) = ptext SLIT("the context of the data type declaration for") <+> quotes (ppr tc)
pprSourceTyCtxt InstThetaCtxt   = ptext SLIT("the context of an instance declaration")
pprSourceTyCtxt TypeCtxt        = ptext SLIT("the context of a type")
\end{code}

\begin{code}
checkValidTheta :: SourceTyCtxt -> ThetaType -> TcM ()
checkValidTheta ctxt theta 
  = addErrCtxt (checkThetaCtxt ctxt theta) (check_valid_theta ctxt theta)

-------------------------
check_valid_theta ctxt []
  = returnM ()
check_valid_theta ctxt theta
  = getDOpts					`thenM` \ dflags ->
    warnTc (notNull dups) (dupPredWarn dups)	`thenM_`
    mappM_ (check_pred_ty dflags ctxt) theta
  where
    (_,dups) = removeDups tcCmpPred theta

-------------------------
check_pred_ty dflags ctxt pred@(ClassP cls tys)
  = 	-- Class predicates are valid in all contexts
    checkTc (arity == n_tys) arity_err		`thenM_`

	-- Check the form of the argument types
    mappM_ check_arg_type tys				`thenM_`
    checkTc (check_class_pred_tys dflags ctxt tys)
	    (predTyVarErr pred $$ how_to_allow)

  where
    class_name = className cls
    arity      = classArity cls
    n_tys      = length tys
    arity_err  = arityErr "Class" class_name arity n_tys
    how_to_allow = parens (ptext SLIT("Use -fglasgow-exts to permit this"))

check_pred_ty dflags SigmaCtxt (IParam _ ty) = check_arg_type ty
	-- Implicit parameters only allows in type
	-- signatures; not in instance decls, superclasses etc
	-- The reason for not allowing implicit params in instances is a bit subtle
	-- If we allowed	instance (?x::Int, Eq a) => Foo [a] where ...
	-- then when we saw (e :: (?x::Int) => t) it would be unclear how to 
	-- discharge all the potential usas of the ?x in e.   For example, a
	-- constraint Foo [Int] might come out of e,and applying the
	-- instance decl would show up two uses of ?x.

-- Catch-all
check_pred_ty dflags ctxt sty = failWithTc (badPredTyErr sty)

-------------------------
check_class_pred_tys dflags ctxt tys 
  = case ctxt of
	TypeCtxt      -> True	-- {-# SPECIALISE instance Eq (T Int) #-} is fine
	InstThetaCtxt -> gla_exts || undecidable_ok || all tcIsTyVarTy tys
				-- Further checks on head and theta in
				-- checkInstTermination
	other	      -> gla_exts || all tyvar_head tys
  where
    gla_exts       = dopt Opt_GlasgowExts dflags
    undecidable_ok = dopt Opt_AllowUndecidableInstances dflags

-------------------------
tyvar_head ty			-- Haskell 98 allows predicates of form 
  | tcIsTyVarTy ty = True	-- 	C (a ty1 .. tyn)
  | otherwise			-- where a is a type variable
  = case tcSplitAppTy_maybe ty of
	Just (ty, _) -> tyvar_head ty
	Nothing	     -> False
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
  = mappM_ complain (filter is_ambig theta)
  where
    complain pred     = addErrTc (ambigErr pred)
    extended_tau_vars = grow theta tau_tyvars

	-- Only a *class* predicate can give rise to ambiguity
	-- An *implicit parameter* cannot.  For example:
	--	foo :: (?x :: [a]) => Int
	--	foo = length ?x
	-- is fine.  The call site will suppply a particular 'x'
    is_ambig pred     = isClassPred  pred &&
			any ambig_var (varSetElems (tyVarsOfPred pred))

    ambig_var ct_var  = (ct_var `elem` forall_tyvars) &&
		        not (ct_var `elemVarSet` extended_tau_vars)

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
  = do	{ gla_exts <- doptM Opt_GlasgowExts
	; if gla_exts then return ()	-- New!  Oct06
	  else mappM_ complain (filter is_free theta) }
  where    
    is_free pred     =  not (isIPPred pred)
		     && not (any bound_var (varSetElems (tyVarsOfPred pred)))
    bound_var ct_var = ct_var `elem` forall_tyvars
    complain pred    = addErrTc (freeErr pred)

freeErr pred
  = sep [ptext SLIT("All of the type variables in the constraint") <+> quotes (pprPred pred) <+>
		   ptext SLIT("are already in scope"),
	 nest 4 (ptext SLIT("(at least one must be universally quantified here)"))
    ]
\end{code}

\begin{code}
checkThetaCtxt ctxt theta
  = vcat [ptext SLIT("In the context:") <+> pprTheta theta,
	  ptext SLIT("While checking") <+> pprSourceTyCtxt ctxt ]

badPredTyErr sty = ptext SLIT("Illegal constraint") <+> pprPred sty
predTyVarErr pred  = sep [ptext SLIT("Non type-variable argument"),
			  nest 2 (ptext SLIT("in the constraint:") <+> pprPred pred)]
dupPredWarn dups   = ptext SLIT("Duplicate constraint(s):") <+> pprWithCommas pprPred (map head dups)

arityErr kind name n m
  = hsep [ text kind, quotes (ppr name), ptext SLIT("should have"),
	   n_arguments <> comma, text "but has been given", int m]
    where
	n_arguments | n == 0 = ptext SLIT("no arguments")
		    | n == 1 = ptext SLIT("1 argument")
		    | True   = hsep [int n, ptext SLIT("arguments")]
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

    getDOpts					`thenM` \ dflags ->
    mappM_ check_arg_type tys			`thenM_`
    check_inst_head dflags clas tys		`thenM_`
    returnM (clas, tys)
    }}

check_inst_head dflags clas tys
	-- If GlasgowExts then check at least one isn't a type variable
  | dopt Opt_GlasgowExts dflags
  = mapM_ check_one tys

	-- WITH HASKELL 98, MUST HAVE C (T a b c)
  | otherwise
  = checkTc (isSingleton tys && tcValidInstHeadTy first_ty)
	    (instTypeErr (pprClassPred clas tys) head_shape_msg)

  where
    (first_ty : _) = tys

    head_shape_msg = parens (text "The instance type must be of form (T a b c)" $$
			     text "where T is not a synonym, and a,b,c are distinct type variables")

	-- For now, I only allow tau-types (not polytypes) in 
	-- the head of an instance decl.  
	-- 	E.g.  instance C (forall a. a->a) is rejected
	-- One could imagine generalising that, but I'm not sure
	-- what all the consequences might be
    check_one ty = do { check_tau_type (Rank 0) UT_NotOk ty
		      ; checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty) }

instTypeErr pp_ty msg
  = sep [ptext SLIT("Illegal instance declaration for") <+> quotes pp_ty, 
	 nest 4 msg]
\end{code}


%************************************************************************
%*									*
\subsection{Checking instance for termination}
%*									*
%************************************************************************


\begin{code}
checkValidInstance :: [TyVar] -> ThetaType -> Class -> [TcType] -> TcM ()
checkValidInstance tyvars theta clas inst_tys
  = do	{ gla_exts <- doptM Opt_GlasgowExts
	; undecidable_ok <- doptM Opt_AllowUndecidableInstances

	; checkValidTheta InstThetaCtxt theta
	; checkAmbiguity tyvars theta (tyVarsOfTypes inst_tys)

	-- Check that instance inference will terminate (if we care)
	-- For Haskell 98, checkValidTheta has already done that
	; when (gla_exts && not undecidable_ok) $
	  mapM_ failWithTc (checkInstTermination inst_tys theta)
	
	-- The Coverage Condition
	; checkTc (undecidable_ok || checkInstCoverage clas inst_tys)
	  	  (instTypeErr (pprClassPred clas inst_tys) msg)
	}
  where
    msg  = parens (vcat [ptext SLIT("the Coverage Condition fails for one of the functional dependencies;"),
			 undecidableMsg])
\end{code}

Termination test: each assertion in the context satisfies
 (1) no variable has more occurrences in the assertion than in the head, and
 (2) the assertion has fewer constructors and variables (taken together
     and counting repetitions) than the head.
This is only needed with -fglasgow-exts, as Haskell 98 restrictions
(which have already been checked) guarantee termination. 

The underlying idea is that 

    for any ground substitution, each assertion in the
    context has fewer type constructors than the head.


\begin{code}
checkInstTermination :: [TcType] -> ThetaType -> [Message]
checkInstTermination tys theta
  = mapCatMaybes check theta
  where
   fvs  = fvTypes tys
   size = sizeTypes tys
   check pred 
      | not (null (fvPred pred \\ fvs)) 
      = Just (predUndecErr pred nomoreMsg $$ parens undecidableMsg)
      | sizePred pred >= size
      = Just (predUndecErr pred smallerMsg $$ parens undecidableMsg)
      | otherwise
      = Nothing

predUndecErr pred msg = sep [msg,
			nest 2 (ptext SLIT("in the constraint:") <+> pprPred pred)]

nomoreMsg = ptext SLIT("Variable occurs more often in a constraint than in the instance head")
smallerMsg = ptext SLIT("Constraint is no smaller than the instance head")
undecidableMsg = ptext SLIT("Use -fallow-undecidable-instances to permit this")

-- Free variables of a type, retaining repetitions, and expanding synonyms
fvType :: Type -> [TyVar]
fvType ty | Just exp_ty <- tcView ty = fvType exp_ty
fvType (TyVarTy tv)        = [tv]
fvType (TyConApp _ tys)    = fvTypes tys
fvType (NoteTy _ ty)       = fvType ty
fvType (PredTy pred)       = fvPred pred
fvType (FunTy arg res)     = fvType arg ++ fvType res
fvType (AppTy fun arg)     = fvType fun ++ fvType arg
fvType (ForAllTy tyvar ty) = filter (/= tyvar) (fvType ty)

fvTypes :: [Type] -> [TyVar]
fvTypes tys                = concat (map fvType tys)

fvPred :: PredType -> [TyVar]
fvPred (ClassP _ tys')     = fvTypes tys'
fvPred (IParam _ ty)       = fvType ty
fvPred (EqPred ty1 ty2)    = fvType ty1 ++ fvType ty2

-- Size of a type: the number of variables and constructors
sizeType :: Type -> Int
sizeType ty | Just exp_ty <- tcView ty = sizeType exp_ty
sizeType (TyVarTy _)       = 1
sizeType (TyConApp _ tys)  = sizeTypes tys + 1
sizeType (NoteTy _ ty)     = sizeType ty
sizeType (PredTy pred)     = sizePred pred
sizeType (FunTy arg res)   = sizeType arg + sizeType res + 1
sizeType (AppTy fun arg)   = sizeType fun + sizeType arg
sizeType (ForAllTy _ ty)   = sizeType ty

sizeTypes :: [Type] -> Int
sizeTypes xs               = sum (map sizeType xs)

sizePred :: PredType -> Int
sizePred (ClassP _ tys')   = sizeTypes tys'
sizePred (IParam _ ty)     = sizeType ty
sizePred (EqPred ty1 ty2)  = sizeType ty1 + sizeType ty2
\end{code}
