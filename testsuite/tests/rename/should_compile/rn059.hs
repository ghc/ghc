{-# OPTIONS_GHC -XDisambiguateRecordFields #-}

module Foo () where

import Rn059_A
import Rn059_B

a = A { label = 'a' }

b = B { label = 'b' }

f (A { label = a }) (B { label = b }) = (a,b)

