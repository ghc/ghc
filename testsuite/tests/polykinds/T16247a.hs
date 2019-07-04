{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module Bug where

import Data.Kind (Constraint, Type)
import GHC.Generics (Rep1, U1(..))

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>
type family Apply (f :: a ~> b) (x :: a) :: b
type SameKind (a :: k) (b :: k) = (() :: Constraint)

type family From1 (z :: (f :: Type -> Type) a) :: Rep1 f a

type family From1U1 (x :: U1 (p :: k)) :: Rep1 U1 p where {}
data From1U1Sym0 :: forall p k. (U1 :: k -> Type) p ~> Rep1 (U1 :: k -> Type) p where
  From1Sym0KindInference :: forall z arg. SameKind (Apply From1U1Sym0 arg) (From1U1 arg)
                         => From1U1Sym0 z
