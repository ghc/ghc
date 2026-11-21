{-# LANGUAGE DataKinds, GADTs, ImpredicativeTypes, LambdaCase, MultiWayIf, NoExperimentalDependentTypes, RequiredTypeArguments, TypeApplications #-}
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

observeConditional :: Bool -> Bool -> (Int, Bool)
observeConditional flag b =
    observeGuarded
        (\choose value -> if choose
            then Payload id atTwoTypes
            else if value then buildAt (not value) else error "selected conditional branch")
        flag b

observeCase :: Flag a -> a -> Bool -> (Int, Bool)
observeCase tag input branch =
    observeGuarded
        (\choose ready -> case (choose, tag) of
            (True, _) -> Payload id atTwoTypes
            (False, Flag) -> if ready then buildAt (not input) else error "selected case arm")
        branch True

observeGuardedExpr :: Flag a -> a -> Bool -> (Int, Bool)
observeGuardedExpr tag input branch =
    observeGuarded
        (\choose ready -> if
            | choose -> Payload id atTwoTypes
            | ready, Flag <- tag -> buildAt (not input)
            | otherwise -> error "selected guard")
        branch True

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
    unless (observeConditional True (error "unselected condition") == (7, True)
         && observeConditional False True == (9, False)) $
        fail "the conditional forced an unselected arm"
    unless (observeCase (error "unused case tag" :: Flag Bool) (error "unused case input") True == (7, True)
         && observeCase Flag True False == (9, False)) $
        fail "the case producer selected the wrong arm"
    unless (observeGuardedExpr (error "unused guard tag" :: Flag Bool) (error "unused guard input") True == (7, True)
         && observeGuardedExpr Flag True False == (9, False)) $
        fail "the multi-way if selected the wrong arm"
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
