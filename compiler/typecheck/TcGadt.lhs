%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

%************************************************************************
%*									*
		Type refinement for GADTs
%*									*
%************************************************************************

\begin{code}
module TcGadt (
	Refinement, emptyRefinement, isEmptyRefinement, 
	matchRefine, 
	refineType, refinePred, refineResType,
	tcUnifyTys, BindFlag(..)
  ) where

#include "HsVersions.h"

import HsSyn
import Coercion
import Type

import TypeRep
import Var
import VarEnv
import VarSet
import ErrUtils
import Maybes
import Control.Monad
import Outputable
import TcType
import UniqFM
import FastString
\end{code}


%************************************************************************
%*									*
		What a refinement is
%*									*
%************************************************************************

\begin{code}
data Refinement = Reft InScopeSet InternalReft 

type InternalReft = TyVarEnv (Coercion, Type)
-- INVARIANT:   a->(co,ty)   then   co :: (a:=:ty)
-- Not necessarily idemopotent

instance Outputable Refinement where
  ppr (Reft _in_scope env)
    = ptext SLIT("Refinement") <+>
        braces (ppr env)

emptyRefinement :: Refinement
emptyRefinement = (Reft emptyInScopeSet emptyVarEnv)

isEmptyRefinement :: Refinement -> Bool
isEmptyRefinement (Reft _ env) = isEmptyVarEnv env

refineType :: Refinement -> Type -> Maybe (Coercion, Type)
-- Apply the refinement to the type.
-- If (refineType r ty) = (co, ty')
-- Then co :: ty:=:ty'
-- Nothing => the refinement does nothing to this type
refineType (Reft in_scope env) ty
  | not (isEmptyVarEnv env),		-- Common case
    any (`elemVarEnv` env) (varSetElems (tyVarsOfType ty))
  = Just (substTy co_subst ty, substTy tv_subst ty)
  | otherwise
  = Nothing	-- The type doesn't mention any refined type variables
  where
    tv_subst = mkTvSubst in_scope (mapVarEnv snd env)
    co_subst = mkTvSubst in_scope (mapVarEnv fst env)
 
refinePred :: Refinement -> PredType -> Maybe (Coercion, PredType)
refinePred (Reft in_scope env) pred
  | not (isEmptyVarEnv env),		-- Common case
    any (`elemVarEnv` env) (varSetElems (tyVarsOfPred pred))
  = Just (mkPredTy (substPred co_subst pred), substPred tv_subst pred)
  | otherwise
  = Nothing	-- The type doesn't mention any refined type variables
  where
    tv_subst = mkTvSubst in_scope (mapVarEnv snd env)
    co_subst = mkTvSubst in_scope (mapVarEnv fst env)
 
refineResType :: Refinement -> Type -> (HsWrapper, Type)
-- Like refineType, but returns the 'sym' coercion
-- If (refineResType r ty) = (co, ty')
-- Then co :: ty':=:ty
-- It's convenient to return a HsWrapper here
refineResType reft ty
  = case refineType reft ty of
	Just (co, ty1) -> (WpCo (mkSymCoercion co), ty1)
	Nothing	       -> (idHsWrapper, 	    ty)
\end{code}


%************************************************************************
%*									*
		Simple generation of a type refinement
%*									*
%************************************************************************

\begin{code}
matchRefine :: [CoVar] -> Refinement
\end{code}

Given a list of coercions, where for each coercion c::(ty1~ty2), the type ty2
is a specialisation of ty1, produce a type refinement that maps the variables
of ty1 to the corresponding sub-terms of ty2 using appropriate coercions; eg,

  matchRefine (co :: [(a, b)] ~ [(c, Maybe d)])
    = { right (left (right co)) :: a ~ c
      , right (right co)        :: b ~ Maybe d
      }

Precondition: The rhs types must indeed be a specialisation of the lhs types;
  i.e., some free variables of the lhs are replaced with either distinct free 
  variables or proper type terms to obtain the rhs.  (We don't perform full
  unification or type matching here!)

NB: matchRefine does *not* expand the type synonyms.

\begin{code}
matchRefine co_vars 
  = Reft in_scope (foldr plusVarEnv emptyVarEnv (map refineOne co_vars))
  where
    in_scope = foldr extend emptyInScopeSet co_vars

	-- For each co_var, add it *and* the tyvars it mentions, to in_scope
    extend co_var in_scope
      = extendInScopeSetSet in_scope $
	  extendVarSet (tyVarsOfType (tyVarKind co_var)) co_var

    refineOne co_var = refine (TyVarTy co_var) ty1 ty2
      where
        (ty1, ty2) = splitCoercionKind (tyVarKind co_var)

    refine co (TyVarTy tv) ty                     = unitVarEnv tv (co, ty)
    refine co (TyConApp _ tys) (TyConApp _ tys')  = refineArgs co tys tys'
    refine co (NoteTy _ ty) ty'                   = refine co ty ty'
    refine co ty (NoteTy _ ty')                   = refine co ty ty'
    refine _  (PredTy _) (PredTy _)               = 
      error "TcGadt.matchRefine: PredTy"
    refine co (FunTy arg res) (FunTy arg' res')   =
      refine (mkRightCoercion (mkLeftCoercion co)) arg arg' 
      `plusVarEnv` 
      refine (mkRightCoercion co) res res'
    refine co (AppTy fun arg) (AppTy fun' arg')   = 
      refine (mkLeftCoercion co) fun fun' 
      `plusVarEnv`
      refine (mkRightCoercion co) arg arg'
    refine co (ForAllTy tv ty) (ForAllTy _tv ty') =
      refine (mkForAllCoercion tv co) ty ty' `delVarEnv` tv
    refine _ _ _ = error "RcGadt.matchRefine: mismatch"

    refineArgs :: Coercion -> [Type] -> [Type] -> InternalReft
    refineArgs co tys tys' = 
      fst $ foldr refineArg (emptyVarEnv, id) (zip tys tys')
      where
        refineArg (ty, ty') (reft, coWrapper) 
          = (refine (mkRightCoercion (coWrapper co)) ty ty' `plusVarEnv` reft, 
             mkLeftCoercion . coWrapper)
\end{code}


%************************************************************************
%*									*
		Unification
%*									*
%************************************************************************

\begin{code}
tcUnifyTys :: (TyVar -> BindFlag)
	   -> [Type] -> [Type]
	   -> Maybe TvSubst	-- A regular one-shot substitution
-- The two types may have common type variables, and indeed do so in the
-- second call to tcUnifyTys in FunDeps.checkClsFD
--
-- We implement tcUnifyTys using the evidence-generating 'unify' function
-- in this module, even though we don't need to generate any evidence.
-- This is simply to avoid replicating all all the code for unify
tcUnifyTys bind_fn tys1 tys2
  = maybeErrToMaybe $ initUM bind_fn $
    do { reft <- unifyList emptyInternalReft cos tys1 tys2

	-- Find the fixed point of the resulting non-idempotent substitution
	; let in_scope = mkInScopeSet (tvs1 `unionVarSet` tvs2)
	      tv_env   = fixTvSubstEnv in_scope (mapVarEnv snd reft)

	; return (mkTvSubst in_scope tv_env) }
  where
    tvs1 = tyVarsOfTypes tys1
    tvs2 = tyVarsOfTypes tys2
    cos  = zipWith mkUnsafeCoercion tys1 tys2


----------------------------
-- XXX Can we do this more nicely, by exploiting laziness?
-- Or avoid needing it in the first place?
fixTvSubstEnv :: InScopeSet -> TvSubstEnv -> TvSubstEnv
fixTvSubstEnv in_scope env = f env
  where
    f e = let e' = mapUFM (substTy (mkTvSubst in_scope e)) e
          in if and $ eltsUFM $ intersectUFM_C tcEqType e e'
             then e
             else f e'

\end{code}


%************************************************************************
%*									*
		The workhorse
%*									*
%************************************************************************

\begin{code}
emptyInternalReft :: InternalReft
emptyInternalReft = emptyVarEnv

unify :: InternalReft		-- An existing substitution to extend
      -> Coercion 	-- Witness of their equality 
      -> Type -> Type 	-- Types to be unified, and witness of their equality
      -> UM InternalReft		-- Just the extended substitution, 
				-- Nothing if unification failed
-- We do not require the incoming substitution to be idempotent,
-- nor guarantee that the outgoing one is.  That's fixed up by
-- the wrappers.

-- PRE-CONDITION: in the call (unify r co ty1 ty2), we know that
--			co :: (ty1:=:ty2)

-- Respects newtypes, PredTypes

unify subst co ty1 ty2 = -- pprTrace "unify" (ppr subst <+> pprParendType ty1 <+> pprParendType ty2) $
		         unify_ subst co ty1 ty2

-- in unify_, any NewTcApps/Preds should be taken at face value
unify_ subst co (TyVarTy tv1) ty2  = uVar False subst co tv1 ty2
unify_ subst co ty1 (TyVarTy tv2)  = uVar True  subst co tv2 ty1

unify_ subst co ty1 ty2 | Just ty1' <- tcView ty1 = unify subst co ty1' ty2
unify_ subst co ty1 ty2 | Just ty2' <- tcView ty2 = unify subst co ty1 ty2'

unify_ subst co (PredTy p1) (PredTy p2) = unify_pred subst co p1 p2

unify_ subst co t1@(TyConApp tyc1 tys1) t2@(TyConApp tyc2 tys2) 
  | tyc1 == tyc2 = unify_tys subst co tys1 tys2

unify_ subst co (FunTy ty1a ty1b) (FunTy ty2a ty2b) 
  = do	{ let [co1,co2] = decomposeCo 2 co
	; subst' <- unify subst co1 ty1a ty2a
	; unify subst' co2 ty1b ty2b }

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
unify_ subst co (AppTy ty1a ty1b) ty2
  | Just (ty2a, ty2b) <- repSplitAppTy_maybe ty2
  = do	{ subst' <- unify subst (mkLeftCoercion co) ty1a ty2a
        ; unify subst' (mkRightCoercion co) ty1b ty2b }

unify_ subst co ty1 (AppTy ty2a ty2b)
  | Just (ty1a, ty1b) <- repSplitAppTy_maybe ty1
  = do	{ subst' <- unify subst (mkLeftCoercion co) ty1a ty2a
        ; unify subst' (mkRightCoercion co) ty1b ty2b }

unify_ subst co ty1 ty2 = failWith (misMatch ty1 ty2)
	-- ForAlls??


------------------------------
unify_pred subst co (ClassP c1 tys1) (ClassP c2 tys2)
  | c1 == c2 = unify_tys subst co tys1 tys2
unify_pred subst co (IParam n1 t1) (IParam n2 t2)
  | n1 == n2 = unify subst co t1 t2
unify_pred subst co p1 p2 = failWith (misMatch (PredTy p1) (PredTy p2))
 
------------------------------
unify_tys :: InternalReft -> Coercion -> [Type] -> [Type] -> UM InternalReft
unify_tys subst co xs ys
  = unifyList subst (decomposeCo (length xs) co) xs ys

unifyList :: InternalReft -> [Coercion] -> [Type] -> [Type] -> UM InternalReft
unifyList subst orig_cos orig_xs orig_ys
  = go subst orig_cos orig_xs orig_ys
  where
    go subst _	      []     []     = return subst
    go subst (co:cos) (x:xs) (y:ys) = do { subst' <- unify subst co x y
				         ; go subst' cos xs ys }
    go subst _ _ _ = failWith (lengthMisMatch orig_xs orig_ys)

---------------------------------
uVar :: Bool            -- Swapped
     -> InternalReft	-- An existing substitution to extend
     -> Coercion
     -> TyVar           -- Type variable to be unified
     -> Type            -- with this type
     -> UM InternalReft

-- PRE-CONDITION: in the call (uVar swap r co tv1 ty), we know that
--	if swap=False	co :: (tv1:=:ty)
--	if swap=True	co :: (ty:=:tv1)

uVar swap subst co tv1 ty
 = -- Check to see whether tv1 is refined by the substitution
   case (lookupVarEnv subst tv1) of

     -- Yes, call back into unify'
     Just (co',ty')	-- co' :: (tv1:=:ty')
	| swap      	-- co :: (ty:=:tv1)
	-> unify subst (mkTransCoercion co co') ty ty' 
        | otherwise 	-- co :: (tv1:=:ty)
	-> unify subst (mkTransCoercion (mkSymCoercion co') co) ty' ty

     -- No, continue
     Nothing -> uUnrefined swap subst co
			   tv1 ty ty


uUnrefined :: Bool                -- Whether the input is swapped
           -> InternalReft        -- An existing substitution to extend
	   -> Coercion
           -> TyVar               -- Type variable to be unified
           -> Type                -- with this type
           -> Type                -- (de-noted version)
           -> UM InternalReft

-- We know that tv1 isn't refined
-- PRE-CONDITION: in the call (uUnrefined False r co tv1 ty2 ty2'), we know that
--	co :: tv1:=:ty2
-- and if the first argument is True instead, we know
--      co :: ty2:=:tv1

uUnrefined swap subst co tv1 ty2 ty2'
  | Just ty2'' <- tcView ty2'
  = uUnrefined swap subst co tv1 ty2 ty2''	-- Unwrap synonyms
		-- This is essential, in case we have
		--	type Foo a = a
		-- and then unify a :=: Foo a

uUnrefined swap subst co tv1 ty2 (TyVarTy tv2)
  | tv1 == tv2		-- Same type variable
  = return subst

    -- Check to see whether tv2 is refined
  | Just (co',ty') <- lookupVarEnv subst tv2	-- co' :: tv2:=:ty'
  = uUnrefined False subst (mkTransCoercion (doSwap swap co) co') tv1 ty' ty'

  -- So both are unrefined; next, see if the kinds force the direction
  | eqKind k1 k2	-- Can update either; so check the bind-flags
  = do	{ b1 <- tvBindFlag tv1
	; b2 <- tvBindFlag tv2
	; case (b1,b2) of
	    (BindMe, _) 	 -> bind swap tv1 ty2

	    (AvoidMe, BindMe)	 -> bind (not swap) tv2 ty1
	    (AvoidMe, _)	 -> bind swap tv1 ty2

	    (WildCard, WildCard) -> return subst
	    (WildCard, Skolem)   -> return subst
	    (WildCard, _)	 -> bind (not swap) tv2 ty1

	    (Skolem, WildCard)	 -> return subst
	    (Skolem, Skolem)	 -> failWith (misMatch ty1 ty2)
	    (Skolem, _)		 -> bind (not swap) tv2 ty1
	}

  | k1 `isSubKind` k2 = bindTv (not swap) subst co tv2 ty1  -- Must update tv2
  | k2 `isSubKind` k1 = bindTv swap subst co tv1 ty2	    -- Must update tv1

  | otherwise = failWith (kindMisMatch tv1 ty2)
  where
    ty1 = TyVarTy tv1
    k1 = tyVarKind tv1
    k2 = tyVarKind tv2
    bind swap tv ty = extendReft swap subst tv co ty

uUnrefined swap subst co tv1 ty2 ty2'	-- ty2 is not a type variable
  | tv1 `elemVarSet` substTvSet subst (tyVarsOfType ty2')
  = failWith (occursCheck tv1 ty2)	-- Occurs check
  | not (k2 `isSubKind` k1)
  = failWith (kindMisMatch tv1 ty2)	-- Kind check
  | otherwise
  = bindTv swap subst co tv1 ty2		-- Bind tyvar to the synonym if poss
  where
    k1 = tyVarKind tv1
    k2 = typeKind ty2'

substTvSet :: InternalReft -> TyVarSet -> TyVarSet
-- Apply the non-idempotent substitution to a set of type variables,
-- remembering that the substitution isn't necessarily idempotent
substTvSet subst tvs
  = foldVarSet (unionVarSet . get) emptyVarSet tvs
  where
    get tv = case lookupVarEnv subst tv of
		Nothing	    -> unitVarSet tv
		Just (_,ty) -> substTvSet subst (tyVarsOfType ty)

bindTv swap subst co tv ty	-- ty is not a type variable
  = do  { b <- tvBindFlag tv
	; case b of
	    Skolem   -> failWith (misMatch (TyVarTy tv) ty)
	    WildCard -> return subst
	    other    -> extendReft swap subst tv co ty
	}

doSwap :: Bool -> Coercion -> Coercion
doSwap swap co = if swap then mkSymCoercion co else co

extendReft :: Bool 
           -> InternalReft 
           -> TyVar 
           -> Coercion 
           -> Type 
           -> UM InternalReft
extendReft swap subst tv  co ty
  = ASSERT2( (coercionKindPredTy co1 `tcEqType` mkCoKind (mkTyVarTy tv) ty), 
          (text "Refinement invariant failure: co = " <+> ppr co1  <+> ppr (coercionKindPredTy co1) $$ text "subst = " <+> ppr tv <+> ppr (mkCoKind (mkTyVarTy tv) ty)) )
    return (extendVarEnv subst tv (co1, ty))
  where
    co1 = doSwap swap co

\end{code}

%************************************************************************
%*									*
		Unification monad
%*									*
%************************************************************************

\begin{code}
data BindFlag 
  = BindMe	-- A regular type variable
  | AvoidMe	-- Like BindMe but, given the choice, avoid binding it

  | Skolem	-- This type variable is a skolem constant
		-- Don't bind it; it only matches itself

  | WildCard 	-- This type variable matches anything,
		-- and does not affect the substitution

newtype UM a = UM { unUM :: (TyVar -> BindFlag)
		         -> MaybeErr Message a }

instance Monad UM where
  return a = UM (\tvs -> Succeeded a)
  fail s   = UM (\tvs -> Failed (text s))
  m >>= k  = UM (\tvs -> case unUM m tvs of
			   Failed err -> Failed err
			   Succeeded v  -> unUM (k v) tvs)

initUM :: (TyVar -> BindFlag) -> UM a -> MaybeErr Message a
initUM badtvs um = unUM um badtvs

tvBindFlag :: TyVar -> UM BindFlag
tvBindFlag tv = UM (\tv_fn -> Succeeded (tv_fn tv))

failWith :: Message -> UM a
failWith msg = UM (\tv_fn -> Failed msg)

maybeErrToMaybe :: MaybeErr fail succ -> Maybe succ
maybeErrToMaybe (Succeeded a) = Just a
maybeErrToMaybe (Failed m)    = Nothing
\end{code}


%************************************************************************
%*									*
		Error reporting
	We go to a lot more trouble to tidy the types
	in TcUnify.  Maybe we'll end up having to do that
	here too, but I'll leave it for now.
%*									*
%************************************************************************

\begin{code}
misMatch t1 t2
  = ptext SLIT("Can't match types") <+> quotes (ppr t1) <+> 
    ptext SLIT("and") <+> quotes (ppr t2)

lengthMisMatch tys1 tys2
  = sep [ptext SLIT("Can't match unequal length lists"), 
	 nest 2 (ppr tys1), nest 2 (ppr tys2) ]

kindMisMatch tv1 t2
  = vcat [ptext SLIT("Can't match kinds") <+> quotes (ppr (tyVarKind tv1)) <+> 
	    ptext SLIT("and") <+> quotes (ppr (typeKind t2)),
	  ptext SLIT("when matching") <+> quotes (ppr tv1) <+> 
		ptext SLIT("with") <+> quotes (ppr t2)]

occursCheck tv ty
  = hang (ptext SLIT("Can't construct the infinite type"))
       2 (ppr tv <+> equals <+> ppr ty)
\end{code}
