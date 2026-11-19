{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}

module ObserveBoth (agreement, main) where

import qualified BoolObservation as B
import Control.Exception (ErrorCall, evaluate, try)
import Data.Type.Equality ((:~:)(Refl))

-- | Evidence for the two defined inputs, in constructor order.
agreement :: (B.sameResult 'False :~: 'True, B.sameResult 'True :~: 'True)
agreement = (Refl, Refl)

-- | Compare the same functions when their input raises an exception.
main :: IO ()
main = do
    print agreement
    let missing = errorWithoutStackTrace "missing Bool"
    lazy <- evaluate (B.lazyTrue missing)
    strict <- try (evaluate (B.strictTrue missing)) :: IO (Either ErrorCall Bool)
    case (lazy, strict) of
        (True, Left _) -> putStrLn "At bottom, lazyTrue returns True and strictTrue raises."
        _ -> fail "unexpected evaluation of the missing Bool"
