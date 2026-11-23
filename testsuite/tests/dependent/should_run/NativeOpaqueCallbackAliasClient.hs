{-# LANGUAGE CPP, DataKinds, NoExperimentalDependentTypes #-}

module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import NativeOpaqueCallbackAliasBridge (bridge)
import NativeOpaqueCallbackAliasProof (Witness(..), accepts, runCallback, token)
import qualified NativeOpaqueCallbackAliasProvider as Provider
import qualified NativeOpaqueCallbackAliasProvider as SameProvider

main :: IO ()
#if defined(DIRECT_LOCAL)
main = print (accepts localMapper True (token localMapper True))

localMapper :: Bool -> Char
localMapper _ = error "forced local callback"
#elif defined(DIFFERENT_FUNCTION)
main = let f = Provider.mapper; g = Provider.otherMapper
       in print (accepts f True (token g True))
#elif defined(DIFFERENT_ALIAS)
main = let f = Provider.mapper; g = SameProvider.mapper
       in print (accepts f True (token g True))
#elif defined(GUESSED_REDUCTION)
main = let f = Provider.mapper in print (literalResult (token f True))

literalResult :: Witness 'l' -> Bool
literalResult _ = True
#else
main = do
    let mapper = Provider.mapper
        otherMapper = Provider.otherMapper
        bottomMapper = error "forced unused callback" :: Bool -> Char
        bottomInput = error "forced unused callback input" :: Bool
    print (bridge Provider.mapper, accepts mapper True (token mapper True))
    print (accepts bottomMapper bottomInput (token bottomMapper bottomInput))
    print (fst (runCallback mapper True), fst (runCallback otherMapper True))
    result <- try @ErrorCall (evaluate (fst (runCallback mapper False)))
    case result of
        Left err | "mapper demanded its input" `isPrefixOf` displayException err -> pure ()
        _ -> fail "expected the actual callback to run"
#endif
