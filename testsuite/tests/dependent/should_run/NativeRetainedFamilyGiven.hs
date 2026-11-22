{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes, TypeFamilies, TypeOperators #-}
module NativeRetainedFamilyGiven where

import Data.Type.Equality ((:~:)(Refl), gcastWith)

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

-- Apply the same type family on both sides of a local equality.
type family Count (n :: Size) :: Size
#if defined(CALL_HEAD)
underCall :: foreach (f :: Size -> Size) -> foreach (g :: Size -> Size) -> foreach (n :: Size)
          -> Count n :~: f n -> W (Count n) :~: W (g n)
underCall _ _ _ Refl = Refl
#elif defined(CALL_ARG)
underCall :: foreach (f :: Size -> Size) -> foreach (n :: Size) -> foreach (m :: Size)
          -> Count n :~: f n -> W (Count n) :~: W (f m)
underCall _ _ _ Refl = Refl
#elif defined(CALL_ROLE)
underCall :: foreach (f :: Size -> Size) -> foreach (n :: Size)
          -> Coercion (Count n) (f n) -> W (Count n) :~: W (f n)
underCall _ _ Coercion = Refl
#elif defined(CALL_CYCLE)
underCall :: foreach (f :: Size -> Size) -> foreach (n :: Size)
          -> Count n :~: f (Count n) -> W (Count n) :~: W 'Zero
underCall _ _ Refl = Refl
#else
underCall :: foreach (f :: Size -> Size) -> foreach (n :: Size)
          -> f (f n) :~: Count n -> W (Count n) :~: W (f (f n))
underCall _ _ Refl = Refl
#endif

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

-- Infer all of the eliminator's type arguments from its witness and result.
checkWith :: foreach (xs :: [Bool]) -> F xs :~: G xs -> W (F xs) :~: W (G xs)
checkWith _ proof = gcastWith proof Refl

-- Check the same continuation through an ordinary, locally defined function.
eliminate :: (a :~: b) -> ((a ~ b) => r) -> r
eliminate Refl continuation = continuation

checkWithLocal :: foreach (xs :: [Bool]) -> F xs :~: G xs -> W (F xs) :~: W (G xs)
checkWithLocal _ proof = eliminate proof Refl

#ifdef WRONG_CONTINUATION
wrong :: foreach (xs :: [Bool]) -> foreach (ys :: [Bool])
      -> F xs :~: G xs -> W (F ys) :~: W (G ys)
wrong _ _ proof = gcastWith proof Refl
#endif
#endif
#endif
