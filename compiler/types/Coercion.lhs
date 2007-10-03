%
% (c) The University of Glasgow 2006
%

Module for type coercions, as in System FC.

Coercions are represented as types, and their kinds tell what types the 
coercion works on. 

The coercion kind constructor is a special TyCon that must always be saturated

  typeKind (symCoercion type) :: TyConApp CoercionTyCon{...} [type, type]

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Coercion (
        Coercion,
 
        mkCoKind, mkReflCoKind, splitCoercionKind_maybe, splitCoercionKind,
        coercionKind, coercionKinds, coercionKindPredTy,

	-- Equality predicates
	isEqPred, mkEqPred, getEqPredTys, isEqPredTy,  

	-- Coercion transformations
	mkCoercion,
        mkSymCoercion, mkTransCoercion,
        mkLeftCoercion, mkRightCoercion, mkRightCoercions,
	mkInstCoercion, mkAppCoercion,
        mkForAllCoercion, mkFunCoercion, mkInstsCoercion, mkUnsafeCoercion,
        mkNewTypeCoercion, mkFamInstCoercion, mkAppsCoercion,

        splitNewTypeRepCo_maybe, instNewTyCon_maybe, decomposeCo,

        unsafeCoercionTyCon, symCoercionTyCon,
        transCoercionTyCon, leftCoercionTyCon, 
        rightCoercionTyCon, instCoercionTyCon, -- needed by TysWiredIn

	-- CoercionI
	CoercionI(..),
	isIdentityCoercion,
	mkSymCoI, mkTransCoI, 
	mkTyConAppCoI, mkAppTyCoI, mkFunTyCoI,
	mkNoteTyCoI, mkForAllTyCoI,
	fromCoI, fromACo,
	mkClassPPredCoI, mkIParamPredCoI, mkEqPredCoI

       ) where 

#include "HsVersions.h"

import TypeRep
import Type
import TyCon
import Class
import Var
import Name
import OccName
import PrelNames
import Util
import Unique
import BasicTypes
import Outputable


type Coercion     = Type
type CoercionKind = Kind	-- A CoercionKind is always of form (ty1 :=: ty2)

------------------------------
decomposeCo :: Arity -> Coercion -> [Coercion]
-- (decomposeCo 3 c) = [right (left (left c)), right (left c), right c]
-- So this breaks a coercion with kind T A B C :=: T D E F into
-- a list of coercions of kinds A :=: D, B :=: E and E :=: F
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

coercionKind :: Coercion -> (Type, Type)
-- 	c :: (t1 :=: t2)
-- Then (coercionKind c) = (t1,t2)
coercionKind ty@(TyVarTy a) | isCoVar a = splitCoercionKind (tyVarKind a)
                            | otherwise = (ty, ty)
coercionKind (AppTy ty1 ty2) 
  = let (t1, t2) = coercionKind ty1
        (s1, s2) = coercionKind ty2 in
    (mkAppTy t1 s1, mkAppTy t2 s2)
coercionKind (TyConApp tc args)
  | Just (ar, rule) <- isCoercionTyCon_maybe tc 
    -- CoercionTyCons carry their kinding rule, so we use it here
  = ASSERT( length args >= ar )	-- Always saturated
    let (ty1,ty2)    = rule (take ar args)	-- Apply the rule to the right number of args
	(tys1, tys2) = coercionKinds (drop ar args)
    in (mkAppTys ty1 tys1, mkAppTys ty2 tys2)

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


-------------------------------
-- This smart constructor creates a sym'ed version its argument,
-- but tries to push the sym's down to the leaves.  If we come to
-- sym tv or sym tycon then we can drop the sym because tv and tycon
-- are reflexive coercions
mkSymCoercion co      
  | Just co' <- coreView co = mkSymCoercion co'

mkSymCoercion (ForAllTy tv ty)  = ForAllTy tv (mkSymCoercion ty)
mkSymCoercion (AppTy co1 co2) 	= AppTy (mkSymCoercion co1) (mkSymCoercion co2)
mkSymCoercion (FunTy co1 co2) 	= FunTy (mkSymCoercion co1) (mkSymCoercion co2)

mkSymCoercion (TyConApp tc cos) 
  | not (isCoercionTyCon tc) = mkTyConApp tc (map mkSymCoercion cos)

mkSymCoercion (TyConApp tc [co]) 
  | tc `hasKey` symCoercionTyConKey   = co    -- sym (sym co) --> co
  | tc `hasKey` leftCoercionTyConKey  = mkLeftCoercion (mkSymCoercion co)
  | tc `hasKey` rightCoercionTyConKey = mkRightCoercion (mkSymCoercion co)

mkSymCoercion (TyConApp tc [co1,co2]) 
  | tc `hasKey` transCoercionTyConKey
     -- sym (co1 `trans` co2) --> (sym co2) `trans (sym co2)
     -- Note reversal of arguments!
  = mkTransCoercion (mkSymCoercion co2) (mkSymCoercion co1)

  | tc `hasKey` instCoercionTyConKey
     -- sym (co @ ty) --> (sym co) @ ty
     -- Note: sym is not applied to 'ty'
  = mkInstCoercion (mkSymCoercion co1) co2

mkSymCoercion (TyConApp tc cos) 	-- Other coercion tycons, such as those
  = mkCoercion symCoercionTyCon [TyConApp tc cos]  -- arising from newtypes

mkSymCoercion (TyVarTy tv) 
  | isCoVar tv = mkCoercion symCoercionTyCon [TyVarTy tv]
  | otherwise  = TyVarTy tv	-- Reflexive

-------------------------------
-- ToDo: we should be cleverer about transitivity
mkTransCoercion g1 g2	-- sym g `trans` g = id
  | (t1,_) <- coercionKind g1
  , (_,t2) <- coercionKind g2
  , t1 `coreEqType` t2 
  = t1	

  | otherwise
  = mkCoercion transCoercionTyCon [g1, g2]


-------------------------------
-- Smart constructors for left and right
mkLeftCoercion co 
  | Just (co', _) <- splitAppCoercion_maybe co = co'
  | otherwise = mkCoercion leftCoercionTyCon [co]

mkRightCoercion  co      
  | Just (co1, co2) <- splitAppCoercion_maybe co = co2
  | otherwise = mkCoercion rightCoercionTyCon [co]

mkRightCoercions n co
  = go n co []
  where
    go n co acc 
       | n > 0
       = case splitAppCoercion_maybe co of
          Just (co1,co2) -> go (n-1) co1 (co2:acc)
          Nothing        -> go (n-1) (mkCoercion leftCoercionTyCon [co]) (mkCoercion rightCoercionTyCon [co]:acc)
       | otherwise
       = acc

mkInstCoercion co ty
  | Just (tv,co') <- splitForAllTy_maybe co
  = substTyWith [tv] [ty] co'	-- (forall a.co) @ ty  -->  co[ty/a]
  | otherwise
  = mkCoercion instCoercionTyCon  [co, ty]

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
-- bottom, which is one case in which it is safe.  It is also used to 
-- implement the unsafeCoerce# primitive.
mkUnsafeCoercion :: Type -> Type -> Coercion
mkUnsafeCoercion ty1 ty2 
  = mkCoercion unsafeCoercionTyCon [ty1, ty2]


-- See note [Newtype coercions] in TyCon
mkNewTypeCoercion :: Name -> TyCon -> [TyVar] -> Type -> TyCon
mkNewTypeCoercion name tycon tvs rhs_ty
  = mkCoercionTyCon name co_con_arity rule
  where
    co_con_arity = length tvs

    rule args = ASSERT( co_con_arity == length args )
		(TyConApp tycon args, substTyWith tvs args rhs_ty)

-- Coercion identifying a data/newtype/synonym representation type and its 
-- family instance.  It has the form `Co tvs :: F ts :=: R tvs', where `Co' is 
-- the coercion tycon built here, `F' the family tycon and `R' the (derived)
-- representation tycon.
--
mkFamInstCoercion :: Name	-- unique name for the coercion tycon
		  -> [TyVar]	-- type parameters of the coercion (`tvs')
		  -> TyCon	-- family tycon (`F')
		  -> [Type]	-- type instance (`ts')
		  -> TyCon	-- representation tycon (`R')
		  -> TyCon	-- => coercion tycon (`Co')
mkFamInstCoercion name tvs family instTys rep_tycon
  = mkCoercionTyCon name coArity rule
  where
    coArity = length tvs
    rule args = (substTyWith tvs args $		     -- with sigma = [tys/tvs],
		   TyConApp family instTys,	     --       sigma (F ts)
		 TyConApp rep_tycon args)	     --   :=: R tys

--------------------------------------
-- Coercion Type Constructors...

-- Example.  The coercion ((sym c) (sym d) (sym e))
-- will be represented by (TyConApp sym [c, sym d, sym e])
-- If sym c :: p1=q1
--    sym d :: p2=q2
--    sym e :: p3=q3
-- then ((sym c) (sym d) (sym e)) :: (p1 p2 p3)=(q1 q2 q3)

symCoercionTyCon, transCoercionTyCon, leftCoercionTyCon, rightCoercionTyCon, instCoercionTyCon :: TyCon
-- Each coercion TyCon is built with the special CoercionTyCon record and
-- carries its own kinding rule.  Such CoercionTyCons must be fully applied
-- by any TyConApp in which they are applied, however they may also be over
-- applied (see example above) and the kinding function must deal with this.
symCoercionTyCon = 
  mkCoercionTyCon symCoercionTyConName 1 flipCoercionKindOf
  where
    flipCoercionKindOf (co:rest) = ASSERT( null rest ) (ty2, ty1)
	where
	  (ty1, ty2) = coercionKind co

transCoercionTyCon = 
  mkCoercionTyCon transCoercionTyConName 2 composeCoercionKindsOf
  where
    composeCoercionKindsOf (co1:co2:rest)
      = ASSERT( null rest )
        WARN( not (r1 `coreEqType` a2), 
              text "Strange! Type mismatch in trans coercion, probably a bug"
              $$
              ppr r1 <+> text "=/=" <+> ppr a2)
        (a1, r2)
      where
        (a1, r1) = coercionKind co1
        (a2, r2) = coercionKind co2 

leftCoercionTyCon =
  mkCoercionTyCon leftCoercionTyConName 1 leftProjectCoercionKindOf
  where
    leftProjectCoercionKindOf (co:rest) = ASSERT( null rest ) (ty1, ty2)
      where
        (ty1,ty2) = fst (splitCoercionKindOf co)

rightCoercionTyCon =
  mkCoercionTyCon rightCoercionTyConName 1 rightProjectCoercionKindOf
  where
    rightProjectCoercionKindOf (co:rest) = ASSERT( null rest ) (ty1, ty2)
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
splitCoercionKindOf co 
  = pprPanic "Coercion.splitCoercionKindOf" 
             (ppr co $$ ppr (coercionKindPredTy co))

instCoercionTyCon 
  =  mkCoercionTyCon instCoercionTyConName 2 instCoercionKind
  where
    instantiateCo t s =
      let Just (tv, ty) = splitForAllTy_maybe t in
      substTyWith [tv] [s] ty

    instCoercionKind (co1:ty:rest) = ASSERT( null rest )
				     (instantiateCo t1 ty, instantiateCo t2 ty)
      where (t1, t2) = coercionKind co1

unsafeCoercionTyCon 
  = mkCoercionTyCon unsafeCoercionTyConName 2 unsafeCoercionKind
  where
   unsafeCoercionKind (ty1:ty2:rest) = ASSERT( null rest ) (ty1,ty2) 
        
--------------------------------------
-- ...and their names

mkCoConName occ key coCon = mkWiredInName gHC_PRIM (mkOccNameFS tcName occ)
                            key (ATyCon coCon) BuiltInSyntax

transCoercionTyConName = mkCoConName FSLIT("trans") transCoercionTyConKey transCoercionTyCon
symCoercionTyConName   = mkCoConName FSLIT("sym") symCoercionTyConKey symCoercionTyCon
leftCoercionTyConName  = mkCoConName FSLIT("left") leftCoercionTyConKey leftCoercionTyCon
rightCoercionTyConName = mkCoConName FSLIT("right") rightCoercionTyConKey rightCoercionTyCon
instCoercionTyConName  = mkCoConName FSLIT("inst") instCoercionTyConKey instCoercionTyCon
unsafeCoercionTyConName = mkCoConName FSLIT("CoUnsafe") unsafeCoercionTyConKey unsafeCoercionTyCon



instNewTyCon_maybe :: TyCon -> [Type] -> Maybe (Type, CoercionI)
-- instNewTyCon_maybe T ts
--	= Just (rep_ty, co)	if   co : T ts ~ rep_ty
instNewTyCon_maybe tc tys
  | Just (tvs, ty, mb_co_tc) <- unwrapNewTyCon_maybe tc
  = ASSERT( tys `lengthIs` tyConArity tc )
    Just (substTyWith tvs tys ty, 
	  case mb_co_tc of
	   Nothing    -> IdCo
	   Just co_tc -> ACo (mkTyConApp co_tc tys))
  | otherwise
  = Nothing

-- this is here to avoid module loops
splitNewTypeRepCo_maybe :: Type -> Maybe (Type, Coercion)  
-- Sometimes we want to look through a newtype and get its associated coercion
-- It only strips *one layer* off, so the caller will usually call itself recursively
-- Only applied to types of kind *, hence the newtype is always saturated
--    splitNewTypeRepCo_maybe ty
--	= Just (ty', co)  if   co : ty ~ ty'
-- Returns Nothing for non-newtypes or fully-transparent newtypes
splitNewTypeRepCo_maybe ty 
  | Just ty' <- coreView ty = splitNewTypeRepCo_maybe ty'
splitNewTypeRepCo_maybe (TyConApp tc tys)
  | Just (ty', coi) <- instNewTyCon_maybe tc tys
  = case coi of
	ACo co -> Just (ty', co)
	IdCo   -> panic "splitNewTypeRepCo_maybe"
			-- This case handled by coreView
splitNewTypeRepCo_maybe other 
  = Nothing
\end{code}


--------------------------------------
-- CoercionI smart constructors
--	lifted smart constructors of ordinary coercions

\begin{code}
	-- CoercionI is either 
	--	(a) proper coercion
	--	(b) the identity coercion
data CoercionI = IdCo | ACo Coercion

isIdentityCoercion :: CoercionI -> Bool
isIdentityCoercion IdCo = True
isIdentityCoercion _    = False

allIdCos :: [CoercionI] -> Bool
allIdCos = all isIdentityCoercion

zipCoArgs :: [CoercionI] -> [Type] -> [Coercion]
zipCoArgs cois tys = zipWith fromCoI cois tys

fromCoI :: CoercionI -> Type -> Type
fromCoI IdCo ty     = ty	-- Identity coercion represented 
fromCoI (ACo co) ty = co	-- 	by the type itself

mkSymCoI :: CoercionI -> CoercionI
mkSymCoI IdCo = IdCo
mkSymCoI (ACo co) = ACo $ mkCoercion symCoercionTyCon [co] 
				-- the smart constructor
				-- is too smart with tyvars

mkTransCoI :: CoercionI -> CoercionI -> CoercionI
mkTransCoI IdCo aco = aco
mkTransCoI aco IdCo = aco
mkTransCoI (ACo co1) (ACo co2) = ACo $ mkTransCoercion co1 co2

mkTyConAppCoI :: TyCon -> [Type] -> [CoercionI] -> CoercionI
mkTyConAppCoI tyCon tys cois
  | allIdCos cois = IdCo
  | otherwise	  = ACo (TyConApp tyCon (zipCoArgs cois tys))

mkAppTyCoI :: Type -> CoercionI -> Type -> CoercionI -> CoercionI
mkAppTyCoI ty1 IdCo ty2 IdCo = IdCo
mkAppTyCoI ty1 coi1 ty2 coi2 =
	ACo $ AppTy (fromCoI coi1 ty1) (fromCoI coi2 ty2)

mkFunTyCoI :: Type -> CoercionI -> Type -> CoercionI -> CoercionI
mkFunTyCoI ty1 IdCo ty2 IdCo = IdCo
mkFunTyCoI ty1 coi1 ty2 coi2 =
	ACo $ FunTy (fromCoI coi1 ty1) (fromCoI coi2 ty2)

mkNoteTyCoI :: TyNote -> CoercionI -> CoercionI
mkNoteTyCoI _ IdCo = IdCo
mkNoteTyCoI note (ACo co) = ACo $ NoteTy note co

mkForAllTyCoI :: TyVar -> CoercionI -> CoercionI
mkForAllTyCoI _ IdCo = IdCo
mkForAllTyCoI tv (ACo co) = ACo $ ForAllTy tv co

fromACo :: CoercionI -> Coercion
fromACo (ACo co) = co

mkClassPPredCoI :: Class -> [Type] -> [CoercionI] -> CoercionI
-- mkClassPPredCoI cls tys cois = coi
--    coi : PredTy (cls tys) ~ predTy (cls (tys `cast` cois))
mkClassPPredCoI cls tys cois 
  | allIdCos cois = IdCo
  | otherwise     = ACo $ PredTy $ ClassP cls (zipCoArgs cois tys)

mkIParamPredCoI :: (IPName Name) -> CoercionI -> CoercionI 
-- Similar invariant to mkclassPPredCoI
mkIParamPredCoI ipn IdCo     = IdCo
mkIParamPredCoI ipn (ACo co) = ACo $ PredTy $ IParam ipn co

mkEqPredCoI :: Type -> CoercionI -> Type -> CoercionI -> CoercionI
-- Similar invariant to mkclassPPredCoI
mkEqPredCoI _    IdCo     _   IdCo      = IdCo
mkEqPredCoI ty1  IdCo     _   (ACo co2) = ACo $ PredTy $ EqPred ty1 co2
mkEqPredCoI ty1 (ACo co1) ty2 coi2      = ACo $ PredTy $ EqPred co1 (fromCoI coi2 ty2)
\end{code}

