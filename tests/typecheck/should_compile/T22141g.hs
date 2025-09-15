{-# LANGUAGE NoDataKinds #-}
{-# LANGUAGE TypeData #-}
module T22141g where

import Data.Kind (Type)

-- `type data` type constructors should be able to be used without DataKinds.

type data Letter = A | B | C

type F :: Letter -> Type
data F l
