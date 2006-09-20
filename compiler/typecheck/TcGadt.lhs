%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

%************************************************************************
%*									*
		Type refinement for GADTs
%*									*
%************************************************************************

\begin{code}
module TcGadt (
	Refinement, emptyRefinement, gadtRefine, 
	refineType, refineResType,
	dataConCanMatch,
	tcUnifyTys, BindFlag(..)
  ) where

import HsSyn	( ExprCoFn(..), idCoercion, isIdCoercion )
import Coercion	( Coercion, mkSymCoercion, mkTransCoercion, mkUnsafeCoercion,
		  mkLeftCoercion, mkRightCoercion, mkCoKind, coercionKindPredTy,
		  splitCoercionKind, decomposeCo, coercionKind )
import TcType	( TvSubst(..), TvSubstEnv, substTy, mkTvSubst, 
		  substTyVar, zipTopTvSubst, typeKind,
		  eqKind, isSubKind, repSplitAppTy_maybe,
		  tcView, tcGetTyVar_maybe
		)
import Type	( Type, tyVarsOfType, tyVarsOfTypes, tcEqType, mkTyVarTy )
import TypeRep	( Type(..), PredType(..) )
import DataCon	( DataCon, dataConUnivTyVars, dataConEqSpec )
import Var	( CoVar, TyVar, tyVarKind, varUnique )
import VarEnv
import VarSet
import ErrUtils		( Message )
import Maybes		( MaybeErr(..), isJust )
import Control.Monad	( foldM )
import Outputable
import Unique		( Unique )
import UniqFM		( ufmToList )

#include "HsVersions.h"
\end{code}


%************************************************************************
%*									*
		What a refinement is
%*									*
%************************************************************************

\begin{code}
data Refinement = Reft InScopeSet InternalReft 
-- INVARIANT:   a->(co,ty)   then   co :: (a:=:ty)
-- Not necessarily idemopotent

instance Outputable Refinement where
  ppr (Reft in_scope env)
    = ptext SLIT("Refinement") <+>
        braces (ppr env)

emptyRefinement :: Refinement
emptyRefinement = (Reft emptyInScopeSet emptyVarEnv)


refineType :: Refinement -> Type -> (ExprCoFn, Type)
-- Apply the refinement to the type.
-- If (refineType r ty) = (co, ty')
-- Then co :: ty:=:ty'
refineType (Reft in_scope env) ty
  | not (isEmptyVarEnv env),		-- Common case
    any (`elemVarEnv` env) (varSetElems (tyVarsOfType ty))
  = (ExprCoFn (substTy co_subst ty), substTy tv_subst ty)
  | otherwise
  = (idCoercion, ty)	-- The type doesn't mention any refined type variables
  where
    tv_subst = mkTvSubst in_scope (mapVarEnv snd env)
    co_subst = mkTvSubst in_scope (mapVarEnv fst env)
 
refineResType :: Refinement -> Type -> (ExprCoFn, Type)
-- Like refineType, but returns the 'sym' coercion
-- If (refineResType r ty) = (co, ty')
-- Then co :: ty':=:ty
refineResType reft ty
  = case refineType reft ty of
	(ExprCoFn co, ty1) -> (ExprCoFn (mkSymCoercion co), ty1)
	(id_co,       ty1) -> ASSERT( isIdCoercion id_co )
			      (idCoercion, ty1)
\end{code}


%************************************************************************
%*									*
		Generating a type refinement
%*									*
%************************************************************************

\begin{code}
gadtRefine :: Refinement
	   -> [TyVar] 	-- Bind these by preference
	   -> [CoVar]
	   -> MaybeErr Message Refinement
\end{code}

(gadtRefine cvs) takes a list of coercion variables, and returns a
list of coercions, obtained by unifying the types equated by the
incoming coercions.  The returned coercions all have kinds of form
(a:=:ty), where a is a rigid type variable.

Example:
  gadtRefine [c :: (a,Int):=:(Bool,b)]
  = [ right (left c) :: a:=:Bool,	
      sym (right c)  :: b:=:Int ]

That is, given evidence 'c' that (a,Int)=(Bool,b), it returns derived
evidence in easy-to-use form.  In particular, given any e::ty, we know 
that:
	e `cast` ty[right (left c)/a, sym (right c)/b]
	:: ty [Bool/a, Int/b]
      
Two refinements:

- It can fail, if the coercion is unsatisfiable.

- It's biased, by being given a set of type variables to bind
  when there is a choice. Example:
	MkT :: forall a. a -> T [a]
	f :: forall b. T [b] -> b
	f x = let v = case x of { MkT y -> y }
	      in ...
  Here we want to bind [a->b], not the other way round, because
  in this example the return type is wobbly, and we want the
  program to typecheck


-- E.g. (a, Bool, right (left c))
-- INVARIANT: in the triple (tv, ty, co), we have (co :: tv:=:ty)
-- The result is idempotent: the 

\begin{code}
gadtRefine (Reft in_scope env1) 
	   ex_tvs co_vars
-- Precondition: fvs( co_vars ) # env1
-- That is, the kinds of the co_vars are a
-- fixed point of the  incoming refinement

  = ASSERT2( not $ any (`elemVarEnv` env1) (varSetElems $ tyVarsOfTypes $ map tyVarKind co_vars),
	     ppr env1 $$ ppr co_vars $$ ppr (map tyVarKind co_vars) )
    initUM (tryToBind tv_set) $
    do	{ 	-- Run the unifier, starting with an empty env
	; env2 <- foldM do_one emptyInternalReft co_vars

		-- Find the fixed point of the resulting 
		-- non-idempotent substitution
	; let tmp_env = env1 `plusVarEnv` env2
              out_env = fixTvCoEnv in_scope' tmp_env
	; WARN( not (null (badReftElts tmp_env)), ppr (badReftElts tmp_env) $$ ppr tmp_env )
	  WARN( not (null (badReftElts out_env)), ppr (badReftElts out_env) $$ ppr out_env )
	  return (Reft in_scope' out_env) }
  where
    tv_set = mkVarSet ex_tvs
    in_scope' = foldr extend in_scope co_vars
    extend co_var in_scope
	= extendInScopeSetSet (extendInScopeSet in_scope co_var)
			      (tyVarsOfType (tyVarKind co_var))
	
    do_one reft co_var = unify reft (TyVarTy co_var) ty1 ty2
	where
	   (ty1,ty2) = splitCoercionKind (tyVarKind co_var)
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
fixTvCoEnv :: InScopeSet -> InternalReft -> InternalReft
	-- Find the fixed point of a Refinement
	-- (assuming it has no loops!)
fixTvCoEnv in_scope env
  = fixpt
  where
    fixpt         = mapVarEnv step env

    step (co, ty) = (co1, ty')
      -- Apply fixpt one step:
      -- Use refineType to get a substituted type, ty', and a coercion, co_fn,
      -- which justifies the substitution.  If the coercion is not the identity
      -- then use transitivity with the original coercion
      where
        (co_fn, ty') = refineType (Reft in_scope fixpt) ty
        co1 | ExprCoFn co'' <- co_fn = mkTransCoercion co co''
            | otherwise              = ASSERT( isIdCoercion co_fn ) co 

-----------------------------
fixTvSubstEnv :: InScopeSet -> TvSubstEnv -> TvSubstEnv
fixTvSubstEnv in_scope env
  = fixpt 
  where
    fixpt = mapVarEnv (substTy (mkTvSubst in_scope fixpt)) env

----------------------------
dataConCanMatch :: DataCon -> [Type] -> Bool
-- Returns True iff the data con can match a scrutinee of type (T tys)
--		    where T is the type constructor for the data con
--
-- Instantiate the equations and try to unify them
dataConCanMatch con tys
  = isJust (tcUnifyTys (\tv -> BindMe) 
	   	       (map (substTyVar subst . fst) eq_spec)
		       (map snd eq_spec))
  where
    dc_tvs  = dataConUnivTyVars con
    eq_spec = dataConEqSpec con
    subst   = zipTopTvSubst dc_tvs tys

----------------------------
tryToBind :: TyVarSet -> TyVar -> BindFlag
tryToBind tv_set tv | tv `elemVarSet` tv_set = BindMe
		    | otherwise	             = AvoidMe


\end{code}


%************************************************************************
%*									*
		The workhorse
%*									*
%************************************************************************

\begin{code}
type InternalReft = TyVarEnv (Coercion, Type)

-- INVARIANT:   a->(co,ty)   then   co :: (a:=:ty)
-- Not necessarily idemopotent

badReftElts :: InternalReft -> [(Unique, (Coercion,Type))]
-- Return the BAD elements of the refinement
-- Should be empty; used in asserions only
badReftElts env
  = filter (not . ok) (ufmToList env)
  where
    ok :: (Unique, (Coercion, Type)) -> Bool
    ok (u, (co, ty)) | Just tv <- tcGetTyVar_maybe ty1
		     = varUnique tv == u && ty `tcEqType` ty2 
		     | otherwise = False
	where
	  (ty1,ty2) = coercionKind co

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