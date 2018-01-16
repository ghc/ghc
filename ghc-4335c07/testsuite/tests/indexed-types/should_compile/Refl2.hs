{-# LANGUAGE TypeFamilies #-}

module Refl2 where

type family T (a :: * -> *) :: * -> *

data U a x = U (T a x)

mkU :: a x -> U a x
mkU x = U undefined

-- The first definition says "Could not deduce (T a x ~ T a x)", the other two
-- work fine

foo :: a x -> U a x
foo x = case mkU x of U t -> id (U t)
-- foo x = case mkU x of U t -> id ((U :: T a x -> U a x) t)
-- foo x = case mkU x of U t -> U t

