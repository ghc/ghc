
{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving #-}

module T5267 where

import Prelude
import Control.Arrow
import Control.Category

newtype A a b c = A { unA :: a b c }
    deriving (Category, Arrow)

ite :: ArrowChoice a
    => a env Bool -> A a env d -> A a env d -> A a env d
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

