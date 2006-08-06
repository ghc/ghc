
 Module for type coercions, as in System FC.

Coercions are represented as types, and their kinds tell what types the 
coercion works on. 

The coercion kind constructor is a special TyCon that must always be saturated

  typeKind (symCoercion type) :: TyConApp CoercionTyCon{...} [type, type]

\begin{code}
module Coercion (
        Coercion,
 
        mkCoKind, mkReflCoKind, splitCoercionKind_maybe, splitCoercionKind,
        coercionKind, coercionKinds, coercionKindPredTy,

	-- Equality predicates
	isEqPred, mkEqPred, getEqPredTys, isEqPredTy,  

	-- Coercion transformations
        mkSymCoercion, mkTransCoercion,
        mkLeftCoercion, mkRightCoercion, mkInstCoercion, mkAppCoercion,
        mkForAllCoercion, mkFunCoercion, mkInstsCoercion, mkUnsafeCoercion,
        mkNewTypeCoercion, mkAppsCoercion,

        splitNewTypeRepCo_maybe, decomposeCo,

        unsafeCoercionTyCon, symCoercionTyCon,
        transCoercionTyCon, leftCoercionTyCon, 
        rightCoercionTyCon, instCoercionTyCon -- needed by TysWiredIn
       ) where 

#include "HsVersions.h"

import TypeRep
import Type	  ( Type, Kind, PredType, substTyWith, mkAppTy, mkForAllTy,
                    mkFunTy, splitAppTy_maybe, splitForAllTy_maybe, coreView,
                    kindView, mkTyConApp, isCoercionKind, isEqPred, mkAppTys
                  )
import TyCon      ( TyCon, tyConArity, mkCoercionTyCon, isNewTyCon,
                    newTyConRhs, newTyConCo, 
                    isCoercionTyCon, isCoercionTyCon_maybe )
import Var	  ( Var, TyVar, isTyVar, tyVarKind )
import Name       ( BuiltInSyntax(..), Name, mkWiredInName, tcName )
import OccName    ( mkOccNameFS )
import PrelNames  ( symCoercionTyConKey, 
                    transCoercionTyConKey, leftCoercionTyConKey,
                    rightCoercionTyConKey, instCoercionTyConKey, 
                    unsafeCoercionTyConKey, gHC_PRIM
                  )
import Util       ( lengthIs, snocView )
import Unique     ( hasKey )
import BasicTypes ( Arity )
import Outputable



------------------------------
decomposeCo :: Arity -> Coercion -> [Coercion]
-- (decomposeCo 3 c) = [right (left (left c)), right (left c), right c]
decomposeCo n co
  = go n co []
  where
    go 0 co cos = cos
    go n co cos = go (n-1) (mkLeftCoercion co)
			   (mkRightCoercion co : cos)

------------------------------

-------------------------------------------------------
-- and some coercion kind stuff

isEqPredTy (PredTy pred) = isEqPred pred
isEqPredTy other         = False

mkEqPred :: (Type, Type) -> PredType
mkEqPred (ty1, ty2) = EqPred ty1 ty2

getEqPredTys :: PredType -> (Type,Type)
getEqPredTys (EqPred ty1 ty2) = (ty1, ty2)
getEqPredTys other	      = pprPanic "getEqPredTys" (ppr other)

mkCoKind :: Type -> Type -> CoercionKind
mkCoKind ty1 ty2 = PredTy (EqPred ty1 ty2)

mkReflCoKind :: Type -> CoercionKind
mkReflCoKind ty = mkCoKind ty ty

splitCoercionKind :: CoercionKind -> (Type, Type)
splitCoercionKind co | Just co' <- kindView co = splitCoercionKind co'
splitCoercionKind (PredTy (EqPred ty1 ty2))    = (ty1, ty2)

splitCoercionKind_maybe :: Kind -> Maybe (Type, Type)
splitCoercionKind_maybe co | Just co' <- kindView co = splitCoercionKind_maybe co'
splitCoercionKind_maybe (PredTy (EqPred ty1 ty2)) = Just (ty1, ty2)
splitCoercionKind_maybe other = Nothing

isCoVar :: Var -> Bool
isCoVar tv = isTyVar tv && isCoercionKind (tyVarKind tv)

type Coercion     = Type
type CoercionKind = Kind	-- A CoercionKind is always of form (ty1 :=: ty2)

coercionKind :: Coercion -> (Type, Type)
-- 	c :: (t1 :=: t2)
-- Then (coercionKind c) = (t1,t2)

coercionKind (TyVarTy a) | isCoVar a = splitCoercionKind (tyVarKind a)
                         | otherwise = let t = (TyVarTy a) in (t, t)
coercionKind (AppTy ty1 ty2) 
  = let (t1, t2) = coercionKind ty1
        (s1, s2) = coercionKind ty2 in
    (mkAppTy t1 s1, mkAppTy t2 s2)
coercionKind (TyConApp tc args)
  | Just (ar, rule) <- isCoercionTyCon_maybe tc 
  = if length args >= ar 
    then splitCoercionKind (rule args)
    else pprPanic ("arity/arguments mismatch in coercionKind:") 
             (ppr ar $$ ppr tc <+> ppr args)
  | otherwise
  = let (lArgs, rArgs) = coercionKinds args in
    (TyConApp tc lArgs, TyConApp tc rArgs)
coercionKind (FunTy ty1 ty2) 
  = let (t1, t2) = coercionKind ty1
        (s1, s2) = coercionKind ty2 in
    (mkFunTy t1 s1, mkFunTy t2 s2)
coercionKind (ForAllTy tv ty) 
  = let (ty1, ty2) = coercionKind ty in
    (ForAllTy tv ty1, ForAllTy tv ty2)
coercionKind (NoteTy _ ty) = coercionKind ty
coercionKind (PredTy (EqPred c1 c2)) 
  = let k1 = coercionKindPredTy c1
        k2 = coercionKindPredTy c2 in
    (k1,k2)
coercionKind (PredTy (ClassP cl args)) 
  = let (lArgs, rArgs) = coercionKinds args in
    (PredTy (ClassP cl lArgs), PredTy (ClassP cl rArgs))
coercionKind (PredTy (IParam name ty))
  = let (ty1, ty2) = coercionKind ty in
    (PredTy (IParam name ty1), PredTy (IParam name ty2))

coercionKindPredTy :: Coercion -> CoercionKind
coercionKindPredTy c = let (t1, t2) = coercionKind c in mkCoKind t1 t2

coercionKinds :: [Coercion] -> ([Type], [Type])
coercionKinds tys = unzip $ map coercionKind tys

-------------------------------------
-- Coercion kind and type mk's
-- (make saturated TyConApp CoercionTyCon{...} args)

mkCoercion coCon args = ASSERT( tyConArity coCon == length args ) 
                        TyConApp coCon args

mkAppCoercion, mkFunCoercion, mkTransCoercion, mkInstCoercion :: Coercion -> Coercion -> Coercion
mkSymCoercion, mkLeftCoercion, mkRightCoercion :: Coercion -> Coercion

mkAppCoercion    co1 co2 = mkAppTy co1 co2
mkAppsCoercion   co1 tys = foldl mkAppTy co1 tys
-- note that a TyVar should be used here, not a CoVar (nor a TcTyVar)
mkForAllCoercion tv  co  = ASSERT ( isTyVar tv ) mkForAllTy tv co
mkFunCoercion    co1 co2 = mkFunTy co1 co2

mkSymCoercion co      
  | Just co2 <- splitSymCoercion_maybe co = co2
  | Just (co1, co2) <- splitAppCoercion_maybe co 
    -- should make this case better
  = mkAppCoercion (mkSymCoercion co1) (mkSymCoercion co2)
  | Just (co1, co2) <- splitTransCoercion_maybe co
  = mkTransCoercion (mkSymCoercion co1) (mkSymCoercion co2)
  | Just (co, ty) <- splitInstCoercion_maybe co
  = mkInstCoercion (mkSymCoercion co) ty
  | Just co <- splitLeftCoercion_maybe co
  = mkLeftCoercion (mkSymCoercion co)
  | Just co <- splitRightCoercion_maybe co
  = mkRightCoercion (mkSymCoercion co)
mkSymCoercion (ForAllTy tv ty) = ForAllTy tv (mkSymCoercion ty)
-- for atomic types and constructors, we can just ignore sym since these
-- are reflexive coercions
mkSymCoercion (TyVarTy tv) 
  | isCoVar tv = mkCoercion symCoercionTyCon [TyVarTy tv]
  | otherwise  = TyVarTy tv
mkSymCoercion co = mkCoercion symCoercionTyCon [co] 
                   -- this should not happen but does

-- Smart constructors for left and right
mkLeftCoercion co 
  | Just (co', _) <- splitAppCoercion_maybe co = co'
  | otherwise				 = mkCoercion leftCoercionTyCon [co]

mkRightCoercion  co      
  | Just (co1, co2) <- splitAppCoercion_maybe co = co2
  | otherwise = mkCoercion rightCoercionTyCon [co]

mkTransCoercion co1 co2 = mkCoercion transCoercionTyCon [co1, co2]

mkInstCoercion  co ty = mkCoercion instCoercionTyCon  [co, ty]

mkInstsCoercion co tys = foldl mkInstCoercion co tys

splitSymCoercion_maybe :: Coercion -> Maybe Coercion
splitSymCoercion_maybe (TyConApp tc [co]) = 
  if tc `hasKey` symCoercionTyConKey
  then Just co
  else Nothing
splitSymCoercion_maybe co = Nothing

splitAppCoercion_maybe :: Coercion -> Maybe (Coercion, Coercion)
-- Splits a coercion application, being careful *not* to split (left c), etc
-- which are really sytactic constructs, not applications
splitAppCoercion_maybe co  | Just co' <- coreView co = splitAppCoercion_maybe co'
splitAppCoercion_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
splitAppCoercion_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
splitAppCoercion_maybe (TyConApp tc tys) 
   | not (isCoercionTyCon tc)
   = case snocView tys of
       Just (tys', ty') -> Just (TyConApp tc tys', ty')
       Nothing          -> Nothing
splitAppCoercion_maybe co = Nothing

splitTransCoercion_maybe :: Coercion -> Maybe (Coercion, Coercion)
splitTransCoercion_maybe (TyConApp tc [ty1, ty2]) 
 = if tc `hasKey` transCoercionTyConKey then
       Just (ty1, ty2)
   else
       Nothing
splitTransCoercion_maybe other = Nothing

splitInstCoercion_maybe :: Coercion -> Maybe (Coercion, Type)
splitInstCoercion_maybe (TyConApp tc [ty1, ty2])
 = if tc `hasKey` instCoercionTyConKey then
       Just (ty1, ty2)
    else
       Nothing
splitInstCoercion_maybe other = Nothing

splitLeftCoercion_maybe :: Coercion -> Maybe Coercion
splitLeftCoercion_maybe (TyConApp tc [co])
 = if tc `hasKey` leftCoercionTyConKey then
       Just co
   else
       Nothing
splitLeftCoercion_maybe other = Nothing

splitRightCoercion_maybe :: Coercion -> Maybe Coercion
splitRightCoercion_maybe (TyConApp tc [co])
 = if tc `hasKey` rightCoercionTyConKey then
       Just co
   else
       Nothing
splitRightCoercion_maybe other = Nothing

-- Unsafe coercion is not safe, it is used when we know we are dealing with
-- bottom, which is the one case in which it is safe
mkUnsafeCoercion :: Type -> Type -> Coercion
mkUnsafeCoercion ty1 ty2 
  = mkCoercion unsafeCoercionTyCon [ty1, ty2]


-- make the coercion associated with a newtype
mkNewTypeCoercion :: Name -> TyCon -> [TyVar] -> Type -> TyCon
mkNewTypeCoercion name tycon tvs rhs_ty 
  = ASSERT (length tvs == tyConArity tycon)
    mkCoercionTyCon name (tyConArity tycon) rule
  where
    rule args = mkCoKind (substTyWith tvs args rhs_ty) (TyConApp tycon args)

--------------------------------------
-- Coercion Type Constructors...

-- Example.  The coercion ((sym c) (sym d) (sym e))
-- will be represented by (TyConApp sym [c, sym d, sym e])
-- If sym c :: p1=q1
--    sym d :: p2=q2
--    sym e :: p3=q3
-- then ((sym c) (sym d) (sym e)) :: (p1 p2 p3)=(q1 q2 q3)
--
-- (mkKindingFun f) is given the args [c, sym d, sym e]
mkKindingFun :: ([Type] -> (Type, Type, [Type])) -> [Type] -> Kind
mkKindingFun f args = 
  let (ty1, ty2, rest) = f args in 
  let (argtys1, argtys2) = unzip (map coercionKind rest) in
  mkCoKind (mkAppTys ty1 argtys1) (mkAppTys ty2 argtys2)
        

symCoercionTyCon, transCoercionTyCon, leftCoercionTyCon, rightCoercionTyCon, instCoercionTyCon :: TyCon
-- Each coercion TyCon is built with the special CoercionTyCon record and
-- carries its won kinding rule.  Such CoercionTyCons must be fully applied
-- by any TyConApp in which they are applied, however they may also be over
-- applied (see example above) and the kinding function must deal with this.
symCoercionTyCon = 
  mkCoercionTyCon symCoercionTyConName 1 (mkKindingFun flipCoercionKindOf)
  where
    flipCoercionKindOf (co:rest) = (ty2, ty1, rest)
	where
	  (ty1, ty2) = coercionKind co

transCoercionTyCon = 
  mkCoercionTyCon transCoercionTyConName 2 (mkKindingFun composeCoercionKindsOf)
  where
    composeCoercionKindsOf (co1:co2:rest) = (a1, r2, rest)
      where
        (a1, r1) = coercionKind co1
        (a2, r2) = coercionKind co2 

leftCoercionTyCon =
  mkCoercionTyCon leftCoercionTyConName 1 (mkKindingFun leftProjectCoercionKindOf)
  where
    leftProjectCoercionKindOf (co:rest) = (ty1, ty2, rest)
      where
        (ty1,ty2) = fst (splitCoercionKindOf co)

rightCoercionTyCon =
  mkCoercionTyCon rightCoercionTyConName 1 (mkKindingFun rightProjectCoercionKindOf)
  where
    rightProjectCoercionKindOf (co:rest) = (ty1, ty2, rest)
      where
        (ty1,ty2) = snd (splitCoercionKindOf co)

splitCoercionKindOf :: Type -> ((Type,Type), (Type,Type))
-- Helper for left and right.  Finds coercion kind of its input and
-- returns the left and right projections of the coercion...
--
-- if c :: t1 s1 :=: t2 s2 then splitCoercionKindOf c = ((t1, t2), (s1, s2))
splitCoercionKindOf co
  | Just (ty1, ty2) <- splitCoercionKind_maybe (coercionKindPredTy co)
  , Just (ty_fun1, ty_arg1) <- splitAppTy_maybe ty1
  , Just (ty_fun2, ty_arg2) <- splitAppTy_maybe ty2
  = ((ty_fun1, ty_fun2),(ty_arg1, ty_arg2))

instCoercionTyCon 
  =  mkCoercionTyCon instCoercionTyConName 2 (mkKindingFun instCoercionKind)
  where
    instantiateCo t s =
      let Just (tv, ty) = splitForAllTy_maybe t in
      substTyWith [tv] [s] ty

    instCoercionKind (co1:ty:rest) = (instantiateCo t1 ty, instantiateCo t2 ty, rest)
      where (t1, t2) = coercionKind co1

unsafeCoercionTyCon 
  = mkCoercionTyCon unsafeCoercionTyConName 2 (mkKindingFun unsafeCoercionKind)
  where
   unsafeCoercionKind (ty1:ty2:rest) = (ty1,ty2,rest) 
        
--------------------------------------
-- ...and their names

mkCoConName occ key coCon = mkWiredInName gHC_PRIM (mkOccNameFS tcName occ)
                            key Nothing (ATyCon coCon) BuiltInSyntax

transCoercionTyConName = mkCoConName FSLIT("trans") transCoercionTyConKey transCoercionTyCon
symCoercionTyConName   = mkCoConName FSLIT("sym") symCoercionTyConKey symCoercionTyCon
leftCoercionTyConName  = mkCoConName FSLIT("left") leftCoercionTyConKey leftCoercionTyCon
rightCoercionTyConName = mkCoConName FSLIT("right") rightCoercionTyConKey rightCoercionTyCon
instCoercionTyConName  = mkCoConName FSLIT("inst") instCoercionTyConKey instCoercionTyCon
unsafeCoercionTyConName = mkCoConName FSLIT("CoUnsafe") unsafeCoercionTyConKey unsafeCoercionTyCon



-- this is here to avoid module loops
splitNewTypeRepCo_maybe :: Type -> Maybe (Type, Coercion)  
-- Sometimes we want to look through a recursive newtype, and that's what happens here
-- It only strips *one layer* off, so the caller will usually call itself recursively
-- Only applied to types of kind *, hence the newtype is always saturated
splitNewTypeRepCo_maybe ty 
  | Just ty' <- coreView ty = splitNewTypeRepCo_maybe ty'
splitNewTypeRepCo_maybe (TyConApp tc tys)
  | isNewTyCon tc 
  = ASSERT( tys `lengthIs` tyConArity tc )	-- splitNewTypeRepCo_maybe only be applied 
                                                -- 	to *types* (of kind *)
        case newTyConRhs tc of
	  (tvs, rep_ty) -> 
              ASSERT( length tvs == length tys )
	      Just (substTyWith tvs tys rep_ty, mkTyConApp co_con tys)
  where
    co_con = maybe (pprPanic "splitNewTypeRepCo_maybe" (ppr tc)) id (newTyConCo tc)

splitNewTypeRepCo_maybe other = Nothing
\end{code}