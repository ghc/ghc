{-# LANGUAGE TypeFamilies #-}

module D where

import A
import C

type instance F a b = a

unsafeCoerce :: a -> b
unsafeCoerce x = oops x x
