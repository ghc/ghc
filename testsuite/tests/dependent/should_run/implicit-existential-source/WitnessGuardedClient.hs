{-# LANGUAGE DataKinds, ImpredicativeTypes, LambdaCase, NoExperimentalDependentTypes, TypeApplications #-}
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
  where
    checkGuard builder = do
        result <- try @ErrorCall $ evaluate $
            observeGuarded builder True (error "selected guard argument")
        case result of
            Left err
                | takeWhile (/= '\n') (displayException err) == "selected guard argument" -> pure ()
            _ -> fail "expected the selected guard argument"
