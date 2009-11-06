T%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-- | Module for type coercions, as used in System FC. See 'CoreSyn.Expr' for
-- more on System FC and how coercions fit into it.
--
-- Coercions are represented as types, and their kinds tell what types the 
-- coercion works on. The coercion kind constructor is a special TyCon that must always be saturated, like so:
--
-- > typeKind (symCoercion type) :: TyConApp CoercionTyCon{...} [type, type]
module Coercion (
        -- * Main data type
        Coercion,
 
        mkCoKind, mkReflCoKind, coVarKind,
        coercionKind, coercionKinds, isIdentityCoercion,

	-- ** Equality predicates
	isEqPred, mkEqPred, getEqPredTys, isEqPredTy,  

	-- ** Coercion transformations
	mkCoercion,
        mkSymCoercion, mkTransCoercion,
        mkLeftCoercion, mkRightCoercion, mkRightCoercions,
	mkInstCoercion, mkAppCoercion, mkTyConCoercion, mkFunCoercion,
        mkForAllCoercion, mkInstsCoercion, mkUnsafeCoercion,
        mkNewTypeCoercion, mkFamInstCoercion, mkAppsCoercion,
        mkCsel1Coercion, mkCsel2Coercion, mkCselRCoercion, 

        splitNewTypeRepCo_maybe, instNewTyCon_maybe, decomposeCo,

        unsafeCoercionTyCon, symCoercionTyCon,
        transCoercionTyCon, leftCoercionTyCon, 
        rightCoercionTyCon, instCoercionTyCon, -- needed by TysWiredIn
        csel1CoercionTyCon, csel2CoercionTyCon, cselRCoercionTyCon, 

        -- ** Optimisation
	optCoercion,

        -- ** Comparison
        coreEqCoercion,

	-- * CoercionI
	CoercionI(..),
	isIdentityCoI,
	mkSymCoI, mkTransCoI, 
	mkTyConAppCoI, mkAppTyCoI, mkFunTyCoI,
	mkForAllTyCoI,
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
import PrelNames
import Util
import BasicTypes
import Outputable
import FastString

-- | A 'Coercion' represents a 'Type' something should be coerced to.
type Coercion     = Type

-- | A 'CoercionKind' is always of form @ty1 ~ ty2@ and indicates the
-- types that a 'Coercion' will work on.
type CoercionKind = Kind

------------------------------

-- | This breaks a 'Coercion' with 'CoercionKind' @T A B C ~ T D E F@ into
-- a list of 'Coercion's of kinds @A ~ D@, @B ~ E@ and @E ~ F@. Hence:
--
-- > decomposeCo 3 c = [right (left (left c)), right (left c), right c]
decomposeCo :: Arity -> Coercion -> [Coercion]
decomposeCo n co
  = go n co []
  where
    go 0 _  cos = cos
    go n co cos = go (n-1) (mkLeftCoercion co)
			   (mkRightCoercion co : cos)

------------------------------

-------------------------------------------------------
-- and some coercion kind stuff

coVarKind :: CoVar -> (Type,Type) 
-- c :: t1 ~ t2
coVarKind cv = splitCoVarKind (tyVarKind cv)

-- | Take a 'CoercionKind' apart into the two types it relates: see also 'mkCoKind'.
-- Panics if the argument is not a valid 'CoercionKind'
splitCoVarKind :: Kind -> (Type, Type)
splitCoVarKind co | Just co' <- kindView co = splitCoVarKind co'
splitCoVarKind (PredTy (EqPred ty1 ty2))    = (ty1, ty2)

-- | Makes a 'CoercionKind' from two types: the types whose equality is proven by the relevant 'Coercion'
mkCoKind :: Type -> Type -> CoercionKind
mkCoKind ty1 ty2 = PredTy (EqPred ty1 ty2)

-- | (mkCoPredTy s t r) produces the type:   (s~t) => r
mkCoPredTy :: Type -> Type -> Type -> Type
mkCoPredTy s t r = ForAllTy (mkWildCoVar (mkCoKind s t)) r

-- | Tests whether a type is just a type equality predicate
isEqPredTy :: Type -> Bool
isEqPredTy (PredTy pred) = isEqPred pred
isEqPredTy _             = False

-- | Creates a type equality predicate
mkEqPred :: (Type, Type) -> PredType
mkEqPred (ty1, ty2) = EqPred ty1 ty2

-- | Splits apart a type equality predicate, if the supplied 'PredType' is one.
-- Panics otherwise
getEqPredTys :: PredType -> (Type,Type)
getEqPredTys (EqPred ty1 ty2) = (ty1, ty2)
getEqPredTys other	      = pprPanic "getEqPredTys" (ppr other)

-- | Create a reflexive 'CoercionKind' that asserts that a type can be coerced to itself
mkReflCoKind :: Type -> CoercionKind
mkReflCoKind ty = mkCoKind ty ty

-- | If it is the case that
--
-- > c :: (t1 ~ t2)
--
-- i.e. the kind of @c@ is a 'CoercionKind' relating @t1@ and @t2@, then @coercionKind c = (t1, t2)@.
coercionKind :: Coercion -> (Type, Type)
coercionKind ty@(TyVarTy a) | isCoVar a = coVarKind a
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
  | isCoVar tv
--     c1 :: s1~s2  c2 :: t1~t2   c3 :: r1~r2
--    ----------------------------------------------
--    c1~c2 => c3  ::  (s1~t1) => r1 ~ (s2~t2) => r2
--      or
--    forall (_:c1~c2)
  = let (c1,c2) = coVarKind tv
    	(s1,s2) = coercionKind c1
    	(t1,t2) = coercionKind c2
    	(r1,r2) = coercionKind ty
    in
    (mkCoPredTy s1 t1 r1, mkCoPredTy s2 t2 r2)

  | otherwise
--     c1 :: s1~s2  c2 :: t1~t2   c3 :: r1~r2
--   ----------------------------------------------
--    forall a:k. c :: forall a:k. t1 ~ forall a:k. t2
  = let (ty1, ty2) = coercionKind ty in
    (ForAllTy tv ty1, ForAllTy tv ty2)

coercionKind (PredTy (EqPred c1 c2)) 
  = pprTrace "coercionKind" (pprEqPred (c1,c2)) $
    let k1 = coercionKindPredTy c1
        k2 = coercionKindPredTy c2 in
    (k1,k2)
  -- These should not show up in coercions at all
  -- becuase they are in the form of for-alls
  where
    coercionKindPredTy c = let (t1, t2) = coercionKind c in mkCoKind t1 t2



coercionKind (PredTy (ClassP cl args)) 
  = let (lArgs, rArgs) = coercionKinds args in
    (PredTy (ClassP cl lArgs), PredTy (ClassP cl rArgs))
coercionKind (PredTy (IParam name ty))
  = let (ty1, ty2) = coercionKind ty in
    (PredTy (IParam name ty1), PredTy (IParam name ty2))

-- | Apply 'coercionKind' to multiple 'Coercion's
coercionKinds :: [Coercion] -> ([Type], [Type])
coercionKinds tys = unzip $ map coercionKind tys

-------------------------------------
isIdentityCoercion :: Coercion -> Bool
isIdentityCoercion co  
  = case coercionKind co of
       (t1,t2) -> t1 `coreEqType` t2
\end{code}

%************************************************************************
%*									*
            Building coercions
%*									*
%************************************************************************

Coercion kind and type mk's (make saturated TyConApp CoercionTyCon{...} args)

\begin{code}
-- | Make a coercion from the specified coercion 'TyCon' and the 'Type' arguments to
-- that coercion. Try to use the @mk*Coercion@ family of functions instead of using this function
-- if possible
mkCoercion :: TyCon -> [Type] -> Coercion
mkCoercion coCon args = ASSERT( tyConArity coCon == length args ) 
                        TyConApp coCon args

-- | Apply a 'Coercion' to another 'Coercion', which is presumably a
-- 'Coercion' constructor of some kind
mkAppCoercion :: Coercion -> Coercion -> Coercion
mkAppCoercion co1 co2 = mkAppTy co1 co2

-- | Applies multiple 'Coercion's to another 'Coercion', from left to right.
-- See also 'mkAppCoercion'
mkAppsCoercion :: Coercion -> [Coercion] -> Coercion
mkAppsCoercion co1 tys = foldl mkAppTy co1 tys

-- | Apply a type constructor to a list of coercions.
mkTyConCoercion :: TyCon -> [Coercion] -> Coercion
mkTyConCoercion con cos = mkTyConApp con cos

-- | Make a function 'Coercion' between two other 'Coercion's
mkFunCoercion :: Coercion -> Coercion -> Coercion
mkFunCoercion co1 co2 = mkFunTy co1 co2

-- | Make a 'Coercion' which binds a variable within an inner 'Coercion'
mkForAllCoercion :: Var -> Coercion -> Coercion
-- note that a TyVar should be used here, not a CoVar (nor a TcTyVar)
mkForAllCoercion tv  co  = ASSERT ( isTyVar tv ) mkForAllTy tv co


-------------------------------

mkSymCoercion :: Coercion -> Coercion
-- ^ Create a symmetric version of the given 'Coercion' that asserts equality
-- between the same types but in the other "direction", so a kind of @t1 ~ t2@ 
-- becomes the kind @t2 ~ t1@.
--
-- This function attempts to simplify the generated 'Coercion' by removing 
-- redundant applications of @sym@. This is done by pushing this new @sym@ 
-- down into the 'Coercion' and exploiting the fact that @sym (sym co) = co@.
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

mkTransCoercion :: Coercion -> Coercion -> Coercion
-- ^ Create a new 'Coercion' by exploiting transitivity on the two given 'Coercion's.
-- 
-- This function attempts to simplify the generated 'Coercion' by exploiting the fact that
-- @sym g `trans` g = id@.
mkTransCoercion g1 g2	-- sym g `trans` g = id
  | (t1,_) <- coercionKind g1
  , (_,t2) <- coercionKind g2
  , t1 `coreEqType` t2 
  = t1	

  | otherwise
  = mkCoercion transCoercionTyCon [g1, g2]


-------------------------------
-- Smart constructors for left and right

mkLeftCoercion :: Coercion -> Coercion
-- ^ From an application 'Coercion' build a 'Coercion' that asserts the equality of 
-- the "functions" on either side of the type equality. So if @c@ has kind @f x ~ g y@ then:
--
-- > mkLeftCoercion c :: f ~ g
mkLeftCoercion co 
  | Just (co', _) <- splitAppCoercion_maybe co = co'
  | otherwise = mkCoercion leftCoercionTyCon [co]

mkRightCoercion :: Coercion -> Coercion
-- ^ From an application 'Coercion' build a 'Coercion' that asserts the equality of 
-- the "arguments" on either side of the type equality. So if @c@ has kind @f x ~ g y@ then:
--
-- > mkLeftCoercion c :: x ~ y
mkRightCoercion  co      
  | Just (_, co2) <- splitAppCoercion_maybe co = co2
  | otherwise = mkCoercion rightCoercionTyCon [co]

mkRightCoercions :: Int -> Coercion -> [Coercion]
-- ^ As 'mkRightCoercion', but finds the 'Coercion's available on the right side of @n@
-- nested application 'Coercion's, manufacturing new left or right cooercions as necessary
-- if suffficiently many are not directly available.
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


mkCsel1Coercion, mkCsel2Coercion, mkCselRCoercion :: Coercion -> Coercion
mkCsel1Coercion co = mkCoercion csel1CoercionTyCon [co]
mkCsel2Coercion co = mkCoercion csel2CoercionTyCon [co]
mkCselRCoercion co = mkCoercion cselRCoercionTyCon [co]

-------------------------------
mkInstCoercion :: Coercion -> Type -> Coercion
-- ^ Instantiates a 'Coercion' with a 'Type' argument. If possible, it immediately performs
-- the resulting beta-reduction, otherwise it creates a suspended instantiation.
mkInstCoercion co ty
  | Just (tv,co') <- splitForAllTy_maybe co
  = substTyWith [tv] [ty] co'	-- (forall a.co) @ ty  -->  co[ty/a]
  | otherwise
  = mkCoercion instCoercionTyCon  [co, ty]

mkInstsCoercion :: Coercion -> [Type] -> Coercion
-- ^ As 'mkInstCoercion', but instantiates the coercion with a number of type arguments, left-to-right
mkInstsCoercion co tys = foldl mkInstCoercion co tys

{-
splitSymCoercion_maybe :: Coercion -> Maybe Coercion
splitSymCoercion_maybe (TyConApp tc [co]) = 
  if tc `hasKey` symCoercionTyConKey
  then Just co
  else Nothing
splitSymCoercion_maybe co = Nothing
-}

splitAppCoercion_maybe :: Coercion -> Maybe (Coercion, Coercion)
-- ^ Splits a coercion application, being careful *not* to split @left c@ etc.
-- This is because those are really syntactic constructs, not applications
splitAppCoercion_maybe co  | Just co' <- coreView co = splitAppCoercion_maybe co'
splitAppCoercion_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
splitAppCoercion_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
splitAppCoercion_maybe (TyConApp tc tys) 
   | not (isCoercionTyCon tc)
   = case snocView tys of
       Just (tys', ty') -> Just (TyConApp tc tys', ty')
       Nothing          -> Nothing
splitAppCoercion_maybe _ = Nothing

{-
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
-}

-- | Manufacture a coercion from this air. Needless to say, this is not usually safe,
-- but it is used when we know we are dealing with bottom, which is one case in which 
-- it is safe.  This is also used implement the @unsafeCoerce#@ primitive.
mkUnsafeCoercion :: Type -> Type -> Coercion
mkUnsafeCoercion ty1 ty2 
  = mkCoercion unsafeCoercionTyCon [ty1, ty2]


-- See note [Newtype coercions] in TyCon

-- | Create a coercion suitable for the given 'TyCon'. The 'Name' should be that of a
-- new coercion 'TyCon', the 'TyVar's the arguments expected by the @newtype@ and the
-- type the appropriate right hand side of the @newtype@, with the free variables
-- a subset of those 'TyVar's.
mkNewTypeCoercion :: Name -> TyCon -> [TyVar] -> Type -> TyCon
mkNewTypeCoercion name tycon tvs rhs_ty
  = mkCoercionTyCon name co_con_arity rule
  where
    co_con_arity = length tvs

    rule args = ASSERT( co_con_arity == length args )
		(TyConApp tycon args, substTyWith tvs args rhs_ty)

-- | Create a coercion identifying a @data@, @newtype@ or @type@ representation type
-- and its family instance.  It has the form @Co tvs :: F ts ~ R tvs@, where @Co@ is 
-- the coercion tycon built here, @F@ the family tycon and @R@ the (derived)
-- representation tycon.
mkFamInstCoercion :: Name	-- ^ Unique name for the coercion tycon
		  -> [TyVar]	-- ^ Type parameters of the coercion (@tvs@)
		  -> TyCon	-- ^ Family tycon (@F@)
		  -> [Type]	-- ^ Type instance (@ts@)
		  -> TyCon	-- ^ Representation tycon (@R@)
		  -> TyCon	-- ^ Coercion tycon (@Co@)
mkFamInstCoercion name tvs family instTys rep_tycon
  = mkCoercionTyCon name coArity rule
  where
    coArity = length tvs
    rule args = (substTyWith tvs args $		     -- with sigma = [tys/tvs],
		   TyConApp family instTys,	     --       sigma (F ts)
		 TyConApp rep_tycon args)	     --   ~ R tys
\end{code}


%************************************************************************
%*									*
            Coercion Type Constructors
%*									*
%************************************************************************

Example.  The coercion ((sym c) (sym d) (sym e))
will be represented by (TyConApp sym [c, sym d, sym e])
If sym c :: p1=q1
   sym d :: p2=q2
   sym e :: p3=q3
then ((sym c) (sym d) (sym e)) :: (p1 p2 p3)=(q1 q2 q3)

\begin{code}
-- | Coercion type constructors: avoid using these directly and instead use 
-- the @mk*Coercion@ and @split*Coercion@ family of functions if possible.
--
-- Each coercion TyCon is built with the special CoercionTyCon record and
-- carries its own kinding rule.  Such CoercionTyCons must be fully applied
-- by any TyConApp in which they are applied, however they may also be over
-- applied (see example above) and the kinding function must deal with this.
symCoercionTyCon, transCoercionTyCon, leftCoercionTyCon, 
  rightCoercionTyCon, instCoercionTyCon, unsafeCoercionTyCon,
  csel1CoercionTyCon, csel2CoercionTyCon, cselRCoercionTyCon :: TyCon

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
	      _err_stuff )
        (a1, r2)
      where
        (a1, r1) = coercionKind co1
        (a2, r2) = coercionKind co2 

        _err_stuff = vcat [ text "co1:" <+> ppr co1
                          , text "co1 kind left:"  <+> ppr a1
                          , text "co1 kind right:" <+> ppr r1
                          , text "co2:" <+> ppr co2
                          , text "co2 kind left:"  <+> ppr a2
                          , text "co2 kind right:" <+> ppr r2 ]

---------------------------------------------------
leftCoercionTyCon  = mkCoercionTyCon leftCoercionTyConName  1 (fst . decompLR)
rightCoercionTyCon = mkCoercionTyCon rightCoercionTyConName 1 (snd . decompLR)

decompLR :: [Type] -> ((Type,Type), (Type,Type))
-- Helper for left and right.  Finds coercion kind of its input and
-- returns the left and right projections of the coercion...
--
-- if c :: t1 s1 ~ t2 s2 then splitCoercionKindOf c = ((t1, t2), (s1, s2))
decompLR (co : rest)
  | (ty1, ty2) <- coercionKind co
  , Just (ty_fun1, ty_arg1) <- splitAppTy_maybe ty1
  , Just (ty_fun2, ty_arg2) <- splitAppTy_maybe ty2
  = ASSERT( null rest) 
    ((ty_fun1, ty_fun2),(ty_arg1, ty_arg2))
decompLR cos 
  = pprPanic "Coercion.decompLR" 
             (ppr cos $$ vcat (map (pprEqPred .coercionKind) cos))

---------------------------------------------------
instCoercionTyCon 
  =  mkCoercionTyCon instCoercionTyConName 2 instCoercionKind
  where
    instantiateCo t s =
      let Just (tv, ty) = splitForAllTy_maybe t in
      substTyWith [tv] [s] ty

    instCoercionKind (co1:ty:rest) = ASSERT( null rest )
				     (instantiateCo t1 ty, instantiateCo t2 ty)
      where (t1, t2) = coercionKind co1

---------------------------------------------------
unsafeCoercionTyCon 
  = mkCoercionTyCon unsafeCoercionTyConName 2 unsafeCoercionKind
  where
   unsafeCoercionKind (ty1:ty2:rest) = ASSERT( null rest ) (ty1,ty2) 
        
---------------------------------------------------
-- The csel* family
--   If         co :: (s1~t1 => r1) ~ (s2~t2 => r2)
-- Then   csel1 co :: s1 ~ s2
--        csel2 co :: t1 ~ t2
--        cselR co :: r1 ~ r2

csel1CoercionTyCon = mkCoercionTyCon csel1CoercionTyConName 1 (fstOf3   . decompCsel)
csel2CoercionTyCon = mkCoercionTyCon csel2CoercionTyConName 1 (sndOf3   . decompCsel)
cselRCoercionTyCon = mkCoercionTyCon cselRCoercionTyConName 1 (thirdOf3 . decompCsel)

decompCsel :: [Coercion] -> ((Type,Type), (Type,Type), (Type,Type))
decompCsel (co : rest)
  | (ty1,ty2) <- coercionKind co
  , Just (cv1, r1) <- splitForAllTy_maybe ty1
  , Just (cv2, r2) <- splitForAllTy_maybe ty2
  , (s1,t1) <- ASSERT( isCoVar cv1) coVarKind cv1
  , (s2,t2) <- ASSERT( isCoVar cv1) coVarKind cv2
  = ASSERT( null rest )
    ((s1,s2), (t1,t2), (r1,r2))
decompCsel other = pprPanic "decompCsel" (ppr other)

fstOf3   :: (a,b,c) -> a    
sndOf3   :: (a,b,c) -> b    
thirdOf3 :: (a,b,c) -> c    
fstOf3      (a,_,_) =  a
sndOf3      (_,b,_) =  b
thirdOf3    (_,_,c) =  c

--------------------------------------
-- Their Names

transCoercionTyConName, symCoercionTyConName, leftCoercionTyConName, 
   rightCoercionTyConName, instCoercionTyConName, unsafeCoercionTyConName,
   csel1CoercionTyConName, csel2CoercionTyConName, cselRCoercionTyConName :: Name

transCoercionTyConName 	= mkCoConName (fsLit "trans") transCoercionTyConKey transCoercionTyCon
symCoercionTyConName   	= mkCoConName (fsLit "sym") symCoercionTyConKey symCoercionTyCon
leftCoercionTyConName  	= mkCoConName (fsLit "left") leftCoercionTyConKey leftCoercionTyCon
rightCoercionTyConName 	= mkCoConName (fsLit "right") rightCoercionTyConKey rightCoercionTyCon
instCoercionTyConName  	= mkCoConName (fsLit "inst") instCoercionTyConKey instCoercionTyCon
csel1CoercionTyConName  = mkCoConName (fsLit "csel1") csel1CoercionTyConKey csel1CoercionTyCon
csel2CoercionTyConName  = mkCoConName (fsLit "csel2") csel2CoercionTyConKey csel2CoercionTyCon
cselRCoercionTyConName  = mkCoConName (fsLit "cselR") cselRCoercionTyConKey cselRCoercionTyCon
unsafeCoercionTyConName = mkCoConName (fsLit "CoUnsafe") unsafeCoercionTyConKey unsafeCoercionTyCon

mkCoConName :: FastString -> Unique -> TyCon -> Name
mkCoConName occ key coCon = mkWiredInName gHC_PRIM (mkTcOccFS occ)
                            key (ATyCon coCon) BuiltInSyntax
\end{code}


%************************************************************************
%*									*
            Newtypes
%*									*
%************************************************************************

\begin{code}
instNewTyCon_maybe :: TyCon -> [Type] -> Maybe (Type, CoercionI)
-- ^ If @co :: T ts ~ rep_ty@ then:
--
-- > instNewTyCon_maybe T ts = Just (rep_ty, co)
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
-- ^ Sometimes we want to look through a @newtype@ and get its associated coercion.
-- This function only strips *one layer* of @newtype@ off, so the caller will usually call
-- itself recursively. Furthermore, this function should only be applied to types of kind @*@,
-- hence the newtype is always saturated. If @co : ty ~ ty'@ then:
--
-- > splitNewTypeRepCo_maybe ty = Just (ty', co)
--
-- The function returns @Nothing@ for non-@newtypes@ or fully-transparent @newtype@s.
splitNewTypeRepCo_maybe ty 
  | Just ty' <- coreView ty = splitNewTypeRepCo_maybe ty'
splitNewTypeRepCo_maybe (TyConApp tc tys)
  | Just (ty', coi) <- instNewTyCon_maybe tc tys
  = case coi of
	ACo co -> Just (ty', co)
	IdCo   -> panic "splitNewTypeRepCo_maybe"
			-- This case handled by coreView
splitNewTypeRepCo_maybe _
  = Nothing

-- | Determines syntactic equality of coercions
coreEqCoercion :: Coercion -> Coercion -> Bool
coreEqCoercion = coreEqType
\end{code}


%************************************************************************
%*									*
            CoercionI and its constructors
%*									*
%************************************************************************

--------------------------------------
-- CoercionI smart constructors
--	lifted smart constructors of ordinary coercions

\begin{code}
-- | 'CoercionI' represents a /lifted/ ordinary 'Coercion', in that it
-- can represent either one of:
--
-- 1. A proper 'Coercion'
--
-- 2. The identity coercion
data CoercionI = IdCo | ACo Coercion

instance Outputable CoercionI where
  ppr IdCo     = ptext (sLit "IdCo")
  ppr (ACo co) = ppr co

isIdentityCoI :: CoercionI -> Bool
isIdentityCoI IdCo = True
isIdentityCoI _    = False

-- | Tests whether all the given 'CoercionI's represent the identity coercion
allIdCoIs :: [CoercionI] -> Bool
allIdCoIs = all isIdentityCoI

-- | For each 'CoercionI' in the input list, return either the 'Coercion' it
-- contains or the corresponding 'Type' from the other list
zipCoArgs :: [CoercionI] -> [Type] -> [Coercion]
zipCoArgs cois tys = zipWith fromCoI cois tys

-- | Return either the 'Coercion' contained within the 'CoercionI' or the given
-- 'Type' if the 'CoercionI' is the identity 'Coercion'
fromCoI :: CoercionI -> Type -> Type
fromCoI IdCo ty     = ty	-- Identity coercion represented 
fromCoI (ACo co) _  = co	-- 	by the type itself

-- | Smart constructor for @sym@ on 'CoercionI', see also 'mkSymCoercion'
mkSymCoI :: CoercionI -> CoercionI
mkSymCoI IdCo = IdCo
mkSymCoI (ACo co) = ACo $ mkCoercion symCoercionTyCon [co] 
				-- the smart constructor
				-- is too smart with tyvars

-- | Smart constructor for @trans@ on 'CoercionI', see also 'mkTransCoercion'
mkTransCoI :: CoercionI -> CoercionI -> CoercionI
mkTransCoI IdCo aco = aco
mkTransCoI aco IdCo = aco
mkTransCoI (ACo co1) (ACo co2) = ACo $ mkTransCoercion co1 co2

-- | Smart constructor for type constructor application on 'CoercionI', see also 'mkAppCoercion'
mkTyConAppCoI :: TyCon -> [Type] -> [CoercionI] -> CoercionI
mkTyConAppCoI tyCon tys cois
  | allIdCoIs cois = IdCo
  | otherwise	   = ACo (TyConApp tyCon (zipCoArgs cois tys))

-- | Smart constructor for honest-to-god 'Coercion' application on 'CoercionI', see also 'mkAppCoercion'
mkAppTyCoI :: Type -> CoercionI -> Type -> CoercionI -> CoercionI
mkAppTyCoI _   IdCo _   IdCo = IdCo
mkAppTyCoI ty1 coi1 ty2 coi2 =
	ACo $ AppTy (fromCoI coi1 ty1) (fromCoI coi2 ty2)

-- | Smart constructor for function-'Coercion's on 'CoercionI', see also 'mkFunCoercion'
mkFunTyCoI :: Type -> CoercionI -> Type -> CoercionI -> CoercionI
mkFunTyCoI _   IdCo _   IdCo = IdCo
mkFunTyCoI ty1 coi1 ty2 coi2 =
	ACo $ FunTy (fromCoI coi1 ty1) (fromCoI coi2 ty2)

-- | Smart constructor for quantified 'Coercion's on 'CoercionI', see also 'mkForAllCoercion'
mkForAllTyCoI :: TyVar -> CoercionI -> CoercionI
mkForAllTyCoI _ IdCo = IdCo
mkForAllTyCoI tv (ACo co) = ACo $ ForAllTy tv co

-- | Extract a 'Coercion' from a 'CoercionI' if it represents one. If it is the identity coercion,
-- panic
fromACo :: CoercionI -> Coercion
fromACo (ACo co) = co

-- | Smart constructor for class 'Coercion's on 'CoercionI'. Satisfies:
--
-- > mkClassPPredCoI cls tys cois :: PredTy (cls tys) ~ PredTy (cls (tys `cast` cois))
mkClassPPredCoI :: Class -> [Type] -> [CoercionI] -> CoercionI
mkClassPPredCoI cls tys cois 
  | allIdCoIs cois = IdCo
  | otherwise      = ACo $ PredTy $ ClassP cls (zipCoArgs cois tys)

-- | Smart constructor for implicit parameter 'Coercion's on 'CoercionI'. Similar to 'mkClassPPredCoI'
mkIParamPredCoI :: (IPName Name) -> CoercionI -> CoercionI 
mkIParamPredCoI _   IdCo     = IdCo
mkIParamPredCoI ipn (ACo co) = ACo $ PredTy $ IParam ipn co

-- | Smart constructor for type equality 'Coercion's on 'CoercionI'. Similar to 'mkClassPPredCoI'
mkEqPredCoI :: Type -> CoercionI -> Type -> CoercionI -> CoercionI
mkEqPredCoI _    IdCo     _   IdCo      = IdCo
mkEqPredCoI ty1  IdCo     _   (ACo co2) = ACo $ PredTy $ EqPred ty1 co2
mkEqPredCoI _   (ACo co1) ty2 coi2      = ACo $ PredTy $ EqPred co1 (fromCoI coi2 ty2)
\end{code}

%************************************************************************
%*                                                                      *
                 Optimising coercions									
%*                                                                      *
%************************************************************************

\begin{code}
optCoercion :: Coercion -> Coercion
optCoercion co
  = ASSERT2( coercionKind co `eq` coercionKind result, 
             ppr co $$ ppr result $$ ppr (coercionKind co) $$ ppr (coercionKind result) )
    result
  where
        (s1,t1) `eq` (s2,t2) = s1 `coreEqType` s2 && t1 `coreEqType` t2

        (result,_,_) = go co
                         -- optimized, changed?, identity?
        go :: Coercion -> ( Coercion, Bool, Bool )
        -- traverse coercion term bottom up and return
        --
        --    1) equivalent coercion, in optimized form
        --
        --    2) whether the output coercion differs from
        --       the input coercion
        --
        --    3) whether the coercion is an identity coercion
        --
        -- Performs the following optimizations:
        --
        --      sym id          >->     id
        --      trans id co     >->     co
        --      trans co id     >->     co
        --
        go ty@(TyVarTy a) | isCoVar a = let (ty1,ty2) = coercionKind ty
                                        in (ty, False, ty1 `coreEqType` ty2)
                          | otherwise = (ty, False, True)
        go ty@(AppTy ty1 ty2)
          = let (ty1', chan1, id1) = go ty1
                (ty2', chan2, id2) = go ty2
            in if chan1 || chan2
                 then (AppTy ty1' ty2', True,  id1 && id2)
                 else (ty             , False, id1 && id2)
        go ty@(TyConApp tc args)
          | tc == symCoercionTyCon, [ty1] <- args
          = case go ty1 of
              (ty1', _   ,    True) -> (ty1', True, True)
              (ty1', True, _      ) -> (TyConApp tc [ty1'], True, False)
              (_   , _   , _      ) -> (ty, False, False)
          | tc == transCoercionTyCon, [ty1,ty2] <- args
          = let (ty1', chan1, id1) = go ty1
                (ty2', chan2, id2) = go ty2
            in  if id1
                  then (ty2', True, id2)
                  else if id2
                         then (ty1', True, False)
                         else if chan1 || chan2
                                then (TyConApp tc [ty1',ty2'], True , False)
                                else (ty                     , False, False)
          | tc == leftCoercionTyCon, [ty1] <- args
          = let (ty1', chan1, id1) = go ty1
            in  if chan1
                  then (TyConApp tc [ty1'], True , id1)
                  else (ty                , False, id1) 
          | tc == rightCoercionTyCon, [ty1] <- args
          = let (ty1', chan1, id1) = go ty1
            in  if chan1
                  then (TyConApp tc [ty1'], True , id1)
                  else (ty                , False, id1) 
	  | not (isCoercionTyCon tc)
          = let (args', chans, ids) = mapAndUnzip3 go args
            in  if or chans
                  then (TyConApp tc args', True , and ids)
                  else (ty               , False, and ids) 
          | otherwise
          = (ty, False, False)
        go ty@(FunTy ty1 ty2)
          = let (ty1',chan1,id1) = go ty1
                (ty2',chan2,id2) = go ty2
            in  if chan1 || chan2
                  then (FunTy ty1' ty2', True , id1 && id2)
                  else (ty             , False, id1 && id2)
        go ty@(ForAllTy tv ty1)
          = let (ty1', chan1, id1) = go ty1
            in if chan1
                 then (ForAllTy tv ty1', True , id1)
                 else (ty              , False, id1)
        go ty@(PredTy (EqPred ty1 ty2))
          = let (ty1', chan1, id1) = go ty1
                (ty2', chan2, id2) = go ty2
            in if chan1 || chan2
                 then (PredTy (EqPred ty1' ty2'), True , id1 && id2)
                 else (ty                       , False, id1 && id2)
        go ty@(PredTy (ClassP cl args))
          = let (args', chans, ids) = mapAndUnzip3 go args
            in  if or chans
                  then (PredTy (ClassP cl args'), True , and ids)
                  else (ty                      , False, and ids)
        go ty@(PredTy (IParam name ty1))
          = let (ty1', chan1, id1) = go ty1
            in  if chan1
                  then (PredTy (IParam name ty1'), True , id1)
                  else (ty                       , False, id1)
\end{code}
