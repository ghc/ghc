{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies, TypeOperators #-}
module NativeRetainedFamilyGiven where

import Data.Type.Equality ((:~:)(Refl))

#if defined(CLOSED_GIVEN) || defined(CLOSED_WRONG)
type family Closed (b :: Bool) :: Bool where
    Closed 'False = 'False

#ifdef CLOSED_WRONG
check :: foreach (b :: Bool) -> Closed b :~: b -> 'Just (Closed b) :~: 'Just 'True
#else
check :: foreach (b :: Bool) -> Closed b :~: b -> 'Just (Closed b) :~: 'Just b
#endif
check _ Refl = Refl
#else
import Data.Type.Coercion (Coercion(..))

data Size = Zero | Succ Size
type family F (xs :: [Bool]) :: Size
type family G (xs :: [Bool]) :: Size
type family W (n :: Size) :: Size

#ifdef OUTSIDE
check :: foreach (xs :: [Bool]) -> Maybe (F xs :~: G xs) -> W (F xs) :~: W (G xs)
check xs proof = case proof of
    Just Refl -> Refl
    Nothing -> Refl
#elif defined(DISTINCT)
check :: foreach (xs :: [Bool]) -> foreach (ys :: [Bool])
      -> F xs :~: G xs -> W (F ys) :~: W (G ys)
check xs ys proof = case proof of Refl -> Refl
#elif defined(ROLE)
check :: foreach (xs :: [Bool]) -> Coercion (F xs) (G xs) -> W (F xs) :~: W (G xs)
check xs proof = case proof of Coercion -> Refl
#elif defined(CYCLE)
check :: foreach (xs :: [Bool]) -> F xs :~: 'Succ (F xs) -> W (F xs) :~: W 'Zero
check xs proof = case proof of Refl -> Refl
#else
check :: foreach (xs :: [Bool]) -> F xs :~: G xs -> W (F xs) :~: W (G xs)
check xs proof = case proof of Refl -> Refl
#endif
#endif
