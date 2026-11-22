{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TupleSections #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import Data.Proxy (Proxy(..))
import qualified FamilyCallback
import qualified CompareAt
import NativeLiftedCallbackProvider (Decision(..), decide)
import System.Environment (getArgs)

newtype Flag = Flag Bool

observe :: forall a. (a -> Bool) -> a -> Bool
observe f x = case decide @a f x of
  IsTrue -> True
  IsFalse -> False

observeInferred :: (a -> Bool) -> a -> Bool
observeInferred f x = case decide f x of
  IsTrue -> True
  IsFalse -> False

tupleClient :: (Bool -> Bool) -> Bool -> IO ()
tupleClient f x = do
  print (decide f x, ())
  print ((decide f x,) ())

tupleLazy :: (Bool -> Bool) -> Bool -> Bool
tupleLazy f x = snd (decide f x, True)

forceFailure :: String -> Bool -> IO Bool
forceFailure message value = do
  result <- try (evaluate value)
  pure $ case result of
    Left (ErrorCall actual) -> actual == message
    Right _ -> False

-- | Pass an opaque callback with the same family-indexed type as the provider.
keepFamily :: (Bool -> FamilyCallback.Result 'True) -> Bool -> IO ()
keepFamily f x = print (FamilyCallback.law @'True (Proxy @'True) f x)

readFamily :: (Bool -> CompareAt.Result 'True) -> Bool -> Bool -> Ordering
readFamily f x y = case CompareAt.decideFamilyCompare f x y of
  CompareAt.Less -> LT
  CompareAt.Equal -> EQ
  CompareAt.Greater -> GT

readVia :: (Bool -> FamilyCallback.Via 'True) -> Bool -> Bool -> Ordering
readVia f x y = case FamilyCallback.decideVia f x y of
  CompareAt.Less -> LT
  CompareAt.Equal -> EQ
  CompareAt.Greater -> GT

keepBoth :: (Bool -> CompareAt.Result 'True) -> Bool -> Bool -> IO ()
keepBoth f x y = print (CompareAt.keepFamilyCompare f x y)

main :: IO ()
main = do
  [input] <- getArgs
  let truth = read input
  print (observe not truth, observe null [truth], observeInferred (\(Flag value) -> value) (Flag truth))
  print (observe (const True) (error "unused argument" :: Flag))
  print =<< forceFailure "used argument" (observe (\flag -> flag `seq` True) (error "used argument" :: Flag))
  print =<< forceFailure "used callback" (observe (error "used callback" :: Flag -> Bool) (Flag truth))
  tupleClient not truth
  print (tupleLazy (error "unused predicate") truth)
  let cmp :: Bool -> Bool -> Ordering
      cmp = flip compare
      observed :: CompareAt.Witness o -> Ordering
      observed CompareAt.Less = LT
      observed CompareAt.Equal = EQ
      observed CompareAt.Greater = GT
      order x y = observed (CompareAt.decideCompare cmp x y)
  print (order (not truth) truth, order truth (not truth), order truth truth)
  let lazyProof :: (Bool -> Bool -> Ordering) -> Bool -> Bool -> IO ()
      lazyProof f x y = print (CompareAt.keepCompare f x y)
  lazyProof (error "unused comparator") (error "unused first argument") (error "unused second argument")
  keepFamily (error "forced family callback") (error "forced callback argument")
  print (readFamily (flip compare) (not truth) truth, readFamily (flip compare) truth (not truth))
  print (readVia compare truth (not truth))
  print (readFamily (\_ _ -> EQ) (error "unused first operand") (error "unused second operand"))
  print =<< forceFailure "forced family callback" (readFamily (error "forced family callback") truth truth == EQ)
  keepBoth (error "forced family callback") (error "forced first operand") (error "forced second operand")
