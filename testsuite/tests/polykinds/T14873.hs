{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module T14873 where

import Data.Kind (Type)

data family Sing (a :: k)

newtype instance Sing (f :: k1 ~> k2) =
  SLambda { applySing :: forall t. Sing t -> Sing (Apply f t) }

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>
type family Apply (f :: k1 ~> k2) (x :: k1) :: k2

class SingI (a :: k) where
  sing :: Sing a

data ColSym1 :: f a -> a ~> Bool
type instance Apply (ColSym1 x) y = Col x y

class PColumn (f :: Type -> Type) where
  type Col (x :: f a) (y :: a) :: Bool

class SColumn (f :: Type -> Type) where
  sCol :: forall (x :: f a) (y :: a).
    Sing x -> Sing y -> Sing (Col x y :: Bool)

instance (SColumn f, SingI x) => SingI (ColSym1 (x :: f a) :: a ~> Bool) where
  sing = SLambda (sCol (sing @_ @x))

{-  When the bug was present, this smaller kind-incorrect version also
    elicited the same piResultTy crash

    But it's kind-incorrect, whereas the main test case should compile file

class SingI (a :: k) where

class SColumn (f :: Type -> Type) where

instance -- forall (f :: Type -> Type) a (x :: f a).
         SColumn f => SingI (x :: f a)
-}

