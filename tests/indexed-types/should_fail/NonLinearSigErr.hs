{-# LANGUAGE TypeFamilies #-}

-- This is actually perfectly ok!

module NonLinearSigErr where

import Data.Kind (Type)

type family E a b
type instance E a (a :: Type) = [a]
