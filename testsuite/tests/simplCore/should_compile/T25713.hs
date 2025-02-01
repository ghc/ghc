{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}

module Ifoldl (module Ifoldl) where

import Data.Kind (Type)
import Data.Type.Equality
import Prelude hiding (reverse)

data Nat = Z | S Nat

data Vec n a where
    Nil  :: Vec 'Z a
    (:::) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :::

deriving instance Show a => Show (Vec n a)

type family a + b where
  Z + b = b
  S a + b = S (a + b)

newtype Wonk k w = Wonk (S k + S w :~: S (S k) + w)

upWonk :: Wonk k w -> Wonk (S k) w
upWonk (Wonk Refl) = Wonk Refl

ifoldlRec :: forall b n a. (forall m. b m -> a -> b ('S m)) -> b 'Z -> Vec n a -> b n
ifoldlRec f = go Refl Refl (Wonk Refl)
  where
    go :: forall k m. k + Z :~: k -> k + m :~: n -> (forall w. Wonk k w) -> b k -> Vec m a -> b n
    go Refl Refl (Wonk Refl) bk Nil = bk
    go Refl Refl !pf bk (a ::: (as :: Vec pm a)) = go @(S k) Refl
      (case pf :: Wonk k pm of Wonk Refl -> Refl) (upWonk pf) (f bk a) as
{-# INLINE ifoldlRec #-}

newtype Flip f y x = Flip
    { unFlip :: f x y
    }

reverseVec :: Vec n a -> Vec n a
reverseVec = unFlip . ifoldlRec (\(Flip xs) x -> Flip $ x ::: xs) (Flip Nil)
