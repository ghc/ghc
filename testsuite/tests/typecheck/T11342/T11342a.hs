{-# LANGUAGE DataKinds #-}

module T11342a where

import Data.Type.Equality

type A = 'a' :: Char

t :: 'x' :~: 'x'
t = Refl
