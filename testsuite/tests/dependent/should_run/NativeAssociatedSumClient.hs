{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies #-}

module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Proxy (Proxy (..))
import GHC.Sigma (Sigma (Pair), first)
import NativeAssociatedSumClass (Build (Result), Shape (Keep))
import NativeAssociatedSumProvider (DefaultSum (Defaulted), Tagged (..))

instance DefaultSum Char

readAssociated :: Result [Int] Char -> Bool
readAssociated (Pair tag _) = tag

readKind :: Keep (Proxy False) Bool True -> Bool
readKind (Pair tag _) = tag

readDefault :: Defaulted Char -> Bool
readDefault = first

main :: IO ()
main = do
    outer <- try (evaluate (readAssociated (error "outer pair"))) :: IO (Either ErrorCall Bool)
    case outer of
        Left _ ->
            print (readAssociated (Pair True (error "unused instance payload")),
                   readKind (Pair False (Proxy :: Proxy False, Proxy :: Proxy False, Proxy :: Proxy True)),
                   readDefault (Pair True (Yes 'q')),
                   readDefault (Pair False (error "unused default payload")))
        Right _ -> fail "package pattern left the outer constructor unevaluated"
