{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T14174a where

import Data.Kind

data TyFun :: * -> * -> *
type a ~> b = TyFun a b -> *
infixr 0 ~>

type family Apply (f :: k1 ~> k2) (x :: k1) :: k2
type a @@ b = Apply a b
infixl 9 @@

data FunArrow = (:->) | (:~>)

class FunType (arr :: FunArrow) where
  type Fun (k1 :: Type) arr (k2 :: Type) :: Type

class FunType arr => AppType (arr :: FunArrow) where
  type App k1 arr k2 (f :: Fun k1 arr k2) (x :: k1) :: k2

type FunApp arr = (FunType arr, AppType arr)

instance FunType (:->) where
  type Fun k1 (:->) k2 = k1 -> k2

instance AppType (:->) where
  type App k1 (:->) k2 (f :: k1 -> k2) x = f x

instance FunType (:~>) where
  type Fun k1 (:~>) k2 = k1 ~> k2

instance AppType (:~>) where
  type App k1 (:~>) k2 (f :: k1 ~> k2) x = f @@ x

infixr 0 -?>
type (-?>) (k1 :: Type) (k2 :: Type) (arr :: FunArrow) = Fun k1 arr k2

type family ElimBool (p :: Bool -> Type)
                     (z :: Bool)
                     (pFalse :: p False)
                     (pTrue  :: p True)
            :: p z where
  -- Commenting out the line below makes the panic go away
  ElimBool p z pFalse pTrue = ElimBoolPoly (:->) p z pFalse pTrue

type family ElimBoolPoly (arr :: FunArrow)
                         (p :: (Bool -?> Type) arr)
                         (z :: Bool)
                         (pFalse :: App Bool arr Type p False)
                         (pTrue  :: App Bool arr Type p True)
            :: App Bool arr Type p z
