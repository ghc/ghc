
{-# LANGUAGE Arrows, TypeOperators, GeneralizedNewtypeDeriving #-}

module T5267 where

import Prelude
import Control.Arrow
import Control.Category

newtype A (~>) b c = A { unA :: b ~> c }
    deriving (Arrow, Category)

ite :: ArrowChoice (~>)
    => (env ~> Bool) -> A (~>) env d -> A (~>) env d -> A (~>) env d
ite iA tA eA = A $ proc env ->
  do i <- iA -< env
     if i then unA tA -< env else unA eA -< env

ite_perm tA eA i = ite i tA eA

-- In 6.12, this worked:
ite' cA tA eA = proc x ->
  do c <- cA -< x
     (| (ite_perm tA eA) (returnA -< c) |)

-- but this didn't:
ite'' cA tA eA = proc x ->
  do c <- cA -< x
     (| ite_perm' (returnA -< c) |)
  where ite_perm' i = ite i tA eA

