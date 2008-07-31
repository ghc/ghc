%
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
 
        mkCoKind, mkReflCoKind, splitCoercionKind_maybe, splitCoercionKind,
        coercionKind, coercionKinds, coercionKindPredTy,

	-- ** Equality predicates
	isEqPred, mkEqPred, getEqPredTys, isEqPredTy,  

	-- ** Coercion transformations
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

        -- ** Comparison
        coreEqCoercion,

	-- * CoercionI
	CoercionI(..),
	isIdentityCoercion,
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
import OccName
import PrelNames
import Util
import Unique
import BasicTypes
import Outputable
import FastString

-- | A 'Coercion' represents a 'Type' something should be coerced to.
type Coercion     = Type

-- | A 'CoercionKind' is always of form @ty1 :=: ty2@ and indicates the
-- types that a 'Coercion' will work on.
type CoercionKind = Kind

------------------------------

-- | This breaks a 'Coercion' with 'CoercionKind' @T A B C :=: T D E F@ into
-- a list of 'Coercion's of kinds @A :=: D@, @B :=: E@ and @E :=: F@. Hence:
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

-- | Makes a 'CoercionKind' from two types: the types whose equality is proven by the relevant 'Coercion'
mkCoKind :: Type -> Type -> CoercionKind
mkCoKind ty1 ty2 = PredTy (EqPred ty1 ty2)

-- | Create a reflexive 'CoercionKind' that asserts that a type can be coerced to itself
mkReflCoKind :: Type -> CoercionKind
mkReflCoKind ty = mkCoKind ty ty

-- | Take a 'CoercionKind' apart into the two types it relates: see also 'mkCoKind'.
-- Panics if the argument is not a valid 'CoercionKind'
splitCoercionKind :: CoercionKind -> (Type, Type)
splitCoercionKind co | Just co' <- kindView co = splitCoercionKind co'
splitCoercionKind (PredTy (EqPred ty1 ty2))    = (ty1, ty2)

-- | Take a 'CoercionKind' apart into the two types it relates, if possible. See also 'splitCoercionKind'
splitCoercionKind_maybe :: Kind -> Maybe (Type, Type)
splitCoercionKind_maybe co | Just co' <- kindView co = splitCoercionKind_maybe co'
splitCoercionKind_maybe (PredTy (EqPred ty1 ty2)) = Just (ty1, ty2)
splitCoercionKind_maybe _ = Nothing

-- | If it is the case that
--
-- > c :: (t1 :=: t2)
--
-- i.e. the kind of @c@ is a 'CoercionKind' relating @t1@ and @t2@, then @coercionKind c = (t1, t2)@.
-- See also 'coercionKindPredTy'
coercionKind :: Coercion -> (Type, Type)
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

-- | Recover the 'CoercionKind' corresponding to a particular 'Coerceion'. See also 'coercionKind'
-- and 'mkCoKind'
coercionKindPredTy :: Coercion -> CoercionKind
coercionKindPredTy c = let (t1, t2) = coercionKind c in mkCoKind t1 t2

-- | Apply 'coercionKind' to multiple 'Coercion's
coercionKinds :: [Coercion] -> ([Type], [Type])
coercionKinds tys = unzip $ map coercionKind tys

-------------------------------------
-- Coercion kind and type mk's
-- (make saturated TyConApp CoercionTyCon{...} args)

-- | Make a coercion from the specified coercion 'TyCon' and the 'Type' arguments to
-- that coercion. Try to use the @mk*Coercion@ family of functions instead of using this function
-- if possible
mkCoercion :: TyCon -> [Type] -> Coercion
mkCoercion coCon args = ASSERT( tyConArity coCon == length args ) 
                        TyConApp coCon args

-- | Apply a 'Coercion' to another 'Coercion', which is presumably a 'Coercion' constructor of some
-- kind
mkAppCoercion :: Coercion -> Coercion -> Coercion
mkAppCoercion    co1 co2 = mkAppTy co1 co2

-- | Applies multiple 'Coercion's to another 'Coercion', from left to right.
-- See also 'mkAppCoercion'
mkAppsCoercion :: Coercion -> [Coercion] -> Coercion
mkAppsCoercion   co1 tys = foldl mkAppTy co1 tys

-- | Make a 'Coercion' which binds a variable within an inner 'Coercion'
mkForAllCoercion :: Var -> Coercion -> Coercion
-- note that a TyVar should be used here, not a CoVar (nor a TcTyVar)
mkForAllCoercion tv  co  = ASSERT ( isTyVar tv ) mkForAllTy tv co

-- | Make a function 'Coercion' between two other 'Coercion's
mkFunCoercion :: Coercion -> Coercion -> Coercion
mkFunCoercion    co1 co2 = mkFunTy co1 co2


-------------------------------

mkSymCoercion :: Coercion -> Coercion
-- ^ Create a symmetric version of the given 'Coercion' that asserts equality between
-- the same types but in the other "direction", so a kind of @t1 :=: t2@ becomes the
-- kind @t2 :=: t1@.
--
-- This function attempts to simplify the generated 'Coercion' by removing redundant applications
-- of @sym@. This is done by pushing this new @sym@ down into the 'Coercion' and exploiting the fact that 
-- @sym (sym co) = co@.
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
-- and its family instance.  It has the form @Co tvs :: F ts :=: R tvs@, where @Co@ is 
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
		 TyConApp rep_tycon args)	     --   :=: R tys

--------------------------------------
-- Coercion Type Constructors...

-- Example.  The coercion ((sym c) (sym d) (sym e))
-- will be represented by (TyConApp sym [c, sym d, sym e])
-- If sym c :: p1=q1
--    sym d :: p2=q2
--    sym e :: p3=q3
-- then ((sym c) (sym d) (sym e)) :: (p1 p2 p3)=(q1 q2 q3)

-- | Coercion type constructors: avoid using these directly and instead use the @mk*Coercion@ and @split*Coercion@ family
-- of functions if possible.
symCoercionTyCon, transCoercionTyCon, leftCoercionTyCon, rightCoercionTyCon, instCoercionTyCon, unsafeCoercionTyCon :: TyCon
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

mkCoConName :: FastString -> Unique -> TyCon -> Name
mkCoConName occ key coCon = mkWiredInName gHC_PRIM (mkTcOccFS occ)
                            key (ATyCon coCon) BuiltInSyntax

transCoercionTyConName, symCoercionTyConName, leftCoercionTyConName, rightCoercionTyConName, instCoercionTyConName, unsafeCoercionTyConName :: Name

transCoercionTyConName = mkCoConName (fsLit "trans") transCoercionTyConKey transCoercionTyCon
symCoercionTyConName   = mkCoConName (fsLit "sym") symCoercionTyConKey symCoercionTyCon
leftCoercionTyConName  = mkCoConName (fsLit "left") leftCoercionTyConKey leftCoercionTyCon
rightCoercionTyConName = mkCoConName (fsLit "right") rightCoercionTyConKey rightCoercionTyCon
instCoercionTyConName  = mkCoConName (fsLit "inst") instCoercionTyConKey instCoercionTyCon
unsafeCoercionTyConName = mkCoConName (fsLit "CoUnsafe") unsafeCoercionTyConKey unsafeCoercionTyCon



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

isIdentityCoercion :: CoercionI -> Bool
isIdentityCoercion IdCo = True
isIdentityCoercion _    = False

-- | Tests whether all the given 'CoercionI's represent the identity coercion
allIdCos :: [CoercionI] -> Bool
allIdCos = all isIdentityCoercion

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
  | allIdCos cois = IdCo
  | otherwise	  = ACo (TyConApp tyCon (zipCoArgs cois tys))

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
  | allIdCos cois = IdCo
  | otherwise     = ACo $ PredTy $ ClassP cls (zipCoArgs cois tys)

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
