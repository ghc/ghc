{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module T15772 where

import Data.Kind

data NP (f :: Type -> Type) (xs :: [Type])

type family Curry (f :: Type -> Type) (xs :: [Type]) (r :: Type) (a :: Type) :: Constraint where
  Curry f xs r (f x -> (a :: Type)) = (xs ~ (x : Tail xs), Curry f (Tail xs) r a)
  Curry f xs r a                    = (xs ~ '[], r ~ a)

type family Tail (a :: [Type]) :: [Type] where
  Tail (_ : xs) = xs

uncurry_NP :: (Curry f xs r a) => (NP f xs -> r) -> a
uncurry_NP = undefined

fun_NP :: NP Id xs -> ()
fun_NP = undefined

newtype Id a = MkId a

-- Bizarrely: uncommenting this allows the program to type-check in GHC 8.8
-- It should type-check without it.
-- test1 :: ()
-- test1 = uncurry_NP fun_NP (MkId 5)

test2 :: ()
test2 = uncurry_NP fun_NP (MkId True) (MkId 5) (MkId True)
