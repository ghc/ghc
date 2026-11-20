{-# LANGUAGE DataKinds, ImpredicativeTypes, LambdaCase, NoExperimentalDependentTypes, RequiredTypeArguments, TypeApplications #-}
module WitnessGuardedClient where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.Proxy
import WitnessSchema

viaGuarded :: GuardedBuilder
viaGuarded = \cases
    False _ | never -> error "unreachable guard"
            | otherwise -> Payload id atTwoTypes
    True b  | b -> consumeAt @True Proxy id b
            | otherwise -> buildAt b

viaGuardedAnnotated :: GuardedBuilder
viaGuardedAnnotated =
    (\cases
        False _ | never -> error "unreachable guard"
                | otherwise -> Payload id atTwoTypes
        True b  | b -> consumeAt @True Proxy id b
                | otherwise -> buildAt b
    ) :: Bool -> Bool -> Payload PolyId

viaJoint :: TwoWitnessBuilder
viaJoint = \cases
    False b | never -> error "unreachable joint guard"
            | otherwise -> fromLeft b
    True b -> fromRight b

viaJointAnnotated :: TwoWitnessBuilder
viaJointAnnotated = (\cases
    True b -> fromRight b
    False b -> fromLeft b) :: Bool -> Bool -> TwoCallbacks PolyId PolyMaybe

localBuilders :: ((Int, Bool), (Int, Bool))
localBuilders = (observeGuarded required True True, observeTwo joint True False)
  where
    required :: GuardedBuilder
    required = \_ b -> (withWitness @Int (type PolyId) Proxy) id (\f -> (f 7, f b))

    joint :: TwoWitnessBuilder
    joint = \cases
        False b -> fromLeft b
        True b -> fromRight b

main :: IO ()
main = do
    let inputs = [(False, error "unused callback argument"), (True, True), (True, False)]
        results builder = [observeGuarded builder flag b | (flag, b) <- inputs]
        guarded = results viaGuarded
    unless (guarded == results viaGuardedAnnotated) $
        fail "the annotation changed the guarded results"
    print guarded
    checkGuard viaGuarded
    checkGuard viaGuardedAnnotated
    putStrLn "only the selected guard forced its argument"
    let observeBoth :: TwoWitnessBuilder -> ((Int, Bool), (Int, Bool))
        observeBoth builder = (observeTwo builder False True, observeTwo builder True False)
        joint = observeBoth viaJoint
    unless (joint == observeBoth viaJointAnnotated) $
        fail "the annotated telescope changed the result"
    print (joint, fst (observeTwo viaJoint True (error "forced the unused field")))
    print localBuilders
  where
    checkGuard builder = do
        result <- try @ErrorCall $ evaluate $
            observeGuarded builder True (error "selected guard argument")
        case result of
            Left err
                | takeWhile (/= '\n') (displayException err) == "selected guard argument" -> pure ()
            _ -> fail "expected the selected guard argument"
