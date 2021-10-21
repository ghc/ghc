{-# LANGUAGE Haskell2010 #-}
{- This is the code extracted from "A reflection on types", by Simon PJ,
Stephanie Weirich, Richard Eisenberg, and Dimitrios Vytiniotis, 2016. -}

-- NB: it includes a negative-recursive function (see delta1), and
-- so will give "simplifier ticks exhausted", at least with -O

{-#  LANGUAGE RankNTypes, PolyKinds, TypeOperators,
             ScopedTypeVariables, GADTs, FlexibleInstances,
             UndecidableInstances, RebindableSyntax,
             DataKinds, MagicHash #-}
{-# OPTIONS_GHC -Wno-missing-methods -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
  -- Because we define a local Typeable class and have
  --   instance Data.Typeable.Typeable a => Typeable a

module Dynamic where

import Data.Map ( Map )
import qualified Data.Map as Map
import Unsafe.Coerce ( unsafeCoerce )
import Control.Monad ( (<=<) )
import Prelude hiding ( lookup, fromInteger, replicate )
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Data
import Data.Kind

lookupMap = Map.lookup
insertMap = Map.insert

--  let's ignore overloaded numbers
fromInteger :: Integer -> Int
fromInteger = Prelude.fromInteger

insertStore = undefined
schema = undefined
withTypeable _ _ = undefined
throw# = undefined

toDynamicST = undefined
fromDynamicST = undefined

extendStore  :: Typeable a => STRef s a -> a -> Store -> Store
lookupStore  :: Typeable a => STRef s a -> Store -> Maybe a

type Key = Int
data STRef s a = STR Key
type Store = Map Key Dynamic

extendStore (STR k) v  s =  insertMap k (toDynamicST v) s
lookupStore (STR k)    s =  case lookupMap k s of
                              Just d   -> fromDynamicST d
                              Nothing  -> Nothing

toDynamicST    :: Typeable a => a -> Dynamic
fromDynamicST  :: Typeable a => Dynamic -> Maybe a

eval = undefined
data Term

data DynamicSilly  =  DIntSilly  Int
              |  DBoolSilly Bool
              |  DCharSilly Char
              |  DPairSilly DynamicSilly DynamicSilly


toDynInt :: Int -> DynamicSilly
toDynInt = DIntSilly

fromDynInt :: DynamicSilly -> Maybe Int
fromDynInt (DIntSilly n)  = Just n
fromDynInt _         = Nothing

toDynPair :: DynamicSilly -> DynamicSilly -> DynamicSilly
toDynPair = DPairSilly

dynFstSilly :: DynamicSilly -> Maybe DynamicSilly
dynFstSilly (DPairSilly x1 x2) = Just x1
dynFstSilly _             = Nothing

eval :: Term -> DynamicSilly

eqT = undefined

instance Typeable (->)
instance Typeable Maybe
instance Typeable Bool
instance Typeable Int
instance (Typeable a, Typeable b) => Typeable (a b)
instance Typeable (,)

instance Eq SomeTypeRep

data Dynamic where
   Dyn :: TypeRep a -> a -> Dynamic

toDynamic :: Typeable a => a -> Dynamic
toDynamic x = Dyn typeRep x

eqTNoKind = undefined

eqTNoKind :: TypeRep a -> TypeRep b -> Maybe (a :***: b)
   --  Primitive; implemented by compiler

data a :***: b where
  ReflNoKind :: a :***: a

fromDynamic :: forall d. Typeable d => Dynamic -> Maybe d
fromDynamic (Dyn (ra :: TypeRep a) (x :: a))
  =  case eqT ra (typeRep :: TypeRep d) of
       Nothing    -> Nothing
       Just Refl  -> Just x

fromDynamicMonad :: forall d. Typeable d => Dynamic -> Maybe d

fromDynamicMonad (Dyn ra x)
  = do  Refl <- eqT ra (typeRep :: TypeRep d)
        return x

cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
cast x = do  Refl <- eqT  (typeRep :: TypeRep a)
                          (typeRep :: TypeRep b)
             return x

gcast :: forall a b c. (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast x = do  Refl <- eqT  (typeRep :: TypeRep a)
                           (typeRep :: TypeRep b)
              return x

data SameKind :: k -> k -> Type

  --  not the most thorough check
foo :: AppResult x -> AppResultNoKind x
foo (App y z) = AppNoKind y z

splitApp :: TypeRep a -> Maybe (AppResult a)
splitApp = undefined
splitAppNoKind = undefined
splitAppNoKind :: TypeRep a -> Maybe (AppResultNoKind a)
   --  Primitive; implemented by compiler

data AppResultNoKind t where
  AppNoKind :: TypeRep a -> TypeRep b -> AppResultNoKind (a b)

dynFstNoKind :: Dynamic -> Maybe Dynamic
dynFstNoKind (Dyn rpab x)
  = do  AppNoKind rpa rb  <- splitAppNoKind rpab
        AppNoKind rp  ra  <- splitAppNoKind rpa
        Refl        <- eqT rp (typeRep :: TypeRep (,))
        return (Dyn ra (fst x))

dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dyn rf f) (Dyn rx x) = do
    App ra rt2   <- splitApp rf
    App rtc rt1  <- splitApp ra
    Refl         <- eqT rtc (typeRep :: TypeRep (->))
    Refl         <- eqT rt1 rx
    return (Dyn rt2 (f x))

data TypeRepAbstract (a :: k)  --  primitive, indexed by type and kind

class Typeable (a :: k) where
   typeRep :: TypeRep a

data AppResult (t :: k) where
  App ::  forall k1 k (a :: k1 -> k) (b :: k1).
          TypeRep a -> TypeRep b -> AppResult (a b)

dynFst :: Dynamic -> Maybe Dynamic
dynFst (Dyn (rpab :: TypeRep pab) (x :: pab))

  = do  App (rpa  :: TypeRep pa ) (rb :: TypeRep b)  <- splitApp rpab
            --  introduces kind |k2|, and types |pa :: k2 -> Type|, |b :: k2|

        App (rp   :: TypeRep p  ) (ra :: TypeRep a)  <- splitApp rpa
            --  introduces kind |k1|, and types |p :: k1 -> k2 -> Type|,
            --                                  |a :: k1|

        Refl       <- eqT rp (typeRep :: TypeRep (,))
            --  introduces |p ~ (,)| and
            --             |(k1 -> k2 -> Type) ~ (Type -> Type -> Type)|

        return (Dyn ra (fst x))

eqT :: forall k1 k2 (a :: k1) (b :: k2).
       TypeRep a -> TypeRep b -> Maybe (a :~: b)

data (a :: k1) :~: (b :: k2) where
  Refl :: forall k (a :: k). a :~: a

castDance :: (Typeable a, Typeable b)  => a -> Maybe b
castDance = castR typeRep typeRep

withTypeable :: TypeRep a -> (Typeable a => r) -> r

castR :: TypeRep a -> TypeRep b -> a -> Maybe b
castR ta tb = withTypeable ta (withTypeable tb castDance)

cmpT = undefined
compareTypeRep = undefined

data SomeTypeRep where
   SomeTypeRep :: TypeRep a -> SomeTypeRep

type TyMapLessTyped = Map SomeTypeRep Dynamic

insertLessTyped :: forall a. Typeable a => a -> TyMapLessTyped -> TyMapLessTyped
insertLessTyped x
  = Map.insert (SomeTypeRep (typeRep :: TypeRep a)) (toDynamic x)

lookupLessTyped :: forall a. Typeable a => TyMapLessTyped -> Maybe a
lookupLessTyped
  = fromDynamic <=< Map.lookup (SomeTypeRep (typeRep :: TypeRep a))

instance Ord SomeTypeRep where
  compare (SomeTypeRep tr1) (SomeTypeRep tr2) = compareTypeRep tr1 tr2

compareTypeRep :: TypeRep a -> TypeRep b -> Ordering  --  primitive

data TyMap = Empty | Node Dynamic TyMap TyMap

lookup :: TypeRep a -> TyMap -> Maybe a
lookup tr1 (Node (Dyn tr2 v) left right) =
  case compareTypeRep tr1 tr2 of
    LT  -> lookup tr1 left
    EQ  -> castR tr2 tr1 v   --  know this cast will succeed
    GT  -> lookup tr1 right
lookup tr1 Empty = Nothing

cmpT :: TypeRep a -> TypeRep b -> OrderingT a b
  --  definition is primitive

data OrderingT a b where
  LTT  :: OrderingT a b
  EQT  :: OrderingT t t
  GTT  :: OrderingT a b

data TypeRep (a :: k) where
  TrApp    :: TypeRep a -> TypeRep b -> TypeRep (a b)
  TrTyCon  :: TyCon -> TypeRep k -> TypeRep (a :: k)

data TyCon = TyCon { tc_module :: Module, tc_name :: String }
data Module = Module { mod_pkg :: String, mod_name :: String }

tcMaybe  :: TyCon
tcMaybe  = TyCon  { tc_module  = Module  { mod_pkg   = "base"
                                               , mod_name  = "Data.Maybe" }
                        , tc_name    = "Maybe" }

rt = undefined

delta1 :: Dynamic -> Dynamic
-- NB: this function behaves like a negative-recursive data type
-- and hence leads compiler into an infinite inlining loop,
-- and we get "simplifier ticks exhausted".
-- See Section 7 of the paper "A reflection on types"
delta1 dn = case fromDynamic dn of
             Just f   -> f dn
             Nothing  -> dn
loop1 = delta1 (toDynamic delta1)

data Rid = MkT (forall a. TypeRep a -> a -> a)
rt :: TypeRep Rid
delta :: forall a. TypeRep a -> a -> a
delta ra x = case (eqT ra rt) of
             Just Refl  -> case x of MkT y -> y rt x
             Nothing    -> x
loop = delta rt (MkT delta)

throw# :: SomeExceptionWithLocation -> a

data SomeExceptionWithLocation where
  SomeExceptionWithLocation :: Exception e => e -> SomeExceptionWithLocation

class (Typeable e, Show e) => Exception e where {   }

data Company
data Salary
incS :: Float -> Salary -> Salary
incS = undefined

--  some impedance matching with SYB
instance Data.Data.Data Company
instance {-#  INCOHERENT  #-} Data.Typeable.Typeable a => Typeable a

mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f x = case (cast f) of
            Just g   -> g x
            Nothing  -> x

data Expr a
frontEnd = undefined

data DynExp where
  DE :: TypeRep a -> Expr a -> DynExp

frontEnd :: String -> DynExp

data TyConOld

typeOf = undefined
eqTOld = undefined
funTcOld = undefined :: TyConOld
splitTyConApp = undefined
mkTyCon3 = undefined
boolTcOld = undefined
tupleTc = undefined
mkTyConApp = undefined
instance Eq TypeRepOld
instance Eq TyConOld

data TypeRepOld       --  Abstract

class TypeableOld a where
  typeRepOld :: proxy a -> TypeRepOld

data DynamicOld where
   DynOld :: TypeRepOld -> a -> DynamicOld

data Proxy a = Proxy

fromDynamicOld :: forall d. TypeableOld d => DynamicOld -> Maybe d
fromDynamicOld (DynOld trx x)
 | typeRepOld (Proxy :: Proxy d) == trx  = Just (unsafeCoerce x)
 | otherwise                          = Nothing

dynApplyOld :: DynamicOld -> DynamicOld -> Maybe DynamicOld
dynApplyOld (DynOld trf f) (DynOld trx x) =
  case splitTyConApp trf of
      (tc, [t1,t2]) | tc == funTcOld && t1 == trx ->
          Just (DynOld t2 ((unsafeCoerce f) x))
      _ -> Nothing

data DynamicClosed where
  DynClosed :: TypeRepClosed a -> a -> DynamicClosed

data TypeRepClosed (a :: Type) where
  TBool  :: TypeRepClosed Bool
  TFun   :: TypeRepClosed a -> TypeRepClosed b -> TypeRepClosed (a -> b)
  TProd  :: TypeRepClosed a -> TypeRepClosed b -> TypeRepClosed (a, b)


lookupPil = undefined

lookupPil :: Typeable a => [Dynamic] -> Maybe a

data Dyn1 = Dyn1 Int
         | DynFun (Dyn1 -> Dyn1)
         | DynPair (Dyn1, Dyn1)

data TypeEnum = IntType | FloatType | BoolType | DateType | StringType
data Schema = Object [Schema] |
              Field TypeEnum |
              Array Schema

schema :: Typeable a => a -> Schema
