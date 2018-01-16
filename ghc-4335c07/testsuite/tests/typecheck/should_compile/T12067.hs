{-# OPTIONS_GHC -Wunused-imports #-}
module T12067 where

import Data.Functor.Identity
import Data.Coerce
import T12067a

foo :: M [a] -> MT [] a
foo = coerce
