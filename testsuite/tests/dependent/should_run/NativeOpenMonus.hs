{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:) (Refl))
import NativeOpenMonusSource (monus)
import PreludeMapLengthSize (Nat (..))

#if defined(WRONG_SUCCESSORS)
bad :: foreach (m :: Nat) -> foreach (n :: Nat)
    -> monus ('Succ m) ('Succ n) :~: 'Succ (monus m n)
bad _ _ = Refl
#elif defined(UNKNOWN_RIGHT)
bad :: foreach (n :: Nat) -> monus 'Zero n :~: 'Zero
bad _ = Refl
#endif

zeroMonus :: foreach (n :: Nat) -> monus 'Zero n :~: 'Zero
zeroMonus Zero = Refl
zeroMonus (Succ _) = Refl

monusZero :: foreach (n :: Nat) -> monus n 'Zero :~: n
monusZero _ = Refl

cancelSuccessors :: foreach (m :: Nat) -> foreach (n :: Nat)
                 -> monus ('Succ m) ('Succ n) :~: monus m n
cancelSuccessors _ _ = Refl

holds :: (a :: Nat) :~: b -> Bool
holds Refl = True

main :: IO ()
main = do
  print ( holds (zeroMonus Zero)
        , holds (zeroMonus (Succ Zero))
        , holds (monusZero (Succ Zero))
        , holds (cancelSuccessors (Succ Zero) Zero)
        )
  case monus Zero (Succ (error "forced predecessor")) of
    Zero -> pure ()
    Succ _ -> error "wrong zero result"
  case monus (Succ (error "forced predecessor")) Zero of
    Zero -> error "wrong successor result"
    Succ _ -> pure ()
  outcome <- try (evaluate (monus Zero (error "forced right operand"))) :: IO (Either ErrorCall Nat)
  case outcome of
    Left err | "forced right operand" `isInfixOf` displayException err -> pure ()
    _ -> error "monus did not inspect its right operand"
