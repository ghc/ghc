{-# LANGUAGE CPP, DataKinds, FlexibleContexts, GADTs, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}

module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
#ifdef REPRESENTATIONAL_ONLY
import Data.Coerce (Coercible)
#endif
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.Sigma (Sigma (Pair), first)
import NativeAssociatedSumProvider (DefaultSum (Defaulted), Tagged (..))
import NativeConstrainedSumProvider (Choice (Shape), Permit (Permit), make)

instance DefaultSum Char

readFirst :: Defaulted Char -> Bool
readFirst = first

readPair :: Defaulted Char -> Bool
readPair pkg = case pkg of Pair tag _payload -> tag

readPayload :: Defaulted Char -> (Bool, Char)
readPayload pkg = case pkg of
    Pair tag payload -> (tag, case payload of Yes c -> c; No c -> c)

readGiven :: Permit a -> Proxy a -> Bool
readGiven Permit ix = case make ix of Pair tag _ -> tag

#ifdef MISSING_DOMAIN
badMissing :: Bool
badMissing = case make (Proxy :: Proxy Int) of Pair tag _ -> tag
#endif

#ifdef STUCK_FAMILY
badStuck :: (a ~ Bool) => Shape a Float -> Bool
badStuck pkg = case pkg of Pair tag _ -> tag
#endif

#ifdef NEWTYPE_HEAD
badNewtype :: (a ~ Bool) => Shape a Int -> Bool
badNewtype pkg = case pkg of Pair tag _ -> tag
#endif

readKnown :: Shape Bool Char -> Bool
readKnown pkg = case pkg of Pair tag _ -> tag

readRefined :: (a ~ Bool) => Shape a Char -> Bool
readRefined pkg = case pkg of Pair tag _ -> tag

readProof :: Choice a => a :~: Bool -> Shape a Char -> Bool
readProof Refl pkg = case pkg of Pair tag _ -> tag

#ifdef REPRESENTATIONAL_ONLY
badRepresentation :: (Choice a, Coercible a Bool) => Proxy a -> Shape a Char -> Bool
badRepresentation _ pkg = case pkg of Pair tag _ -> tag
#endif

main :: IO ()
main = do
    outer <- try (evaluate (readPair (error "outer pair"))) :: IO (Either ErrorCall Bool)
    case outer of
        Left _ -> do
            let pkg = Pair False (error "unused default payload") :: Defaulted Char
                shaped = Pair True (error "unused shaped payload") :: Shape Bool Char
            print (readFirst pkg, readPair pkg, readPayload (Pair True (Yes 'x')),
                   readGiven Permit (Proxy :: Proxy Bool), readKnown shaped,
                   readRefined shaped, readProof Refl shaped)
        Right _ -> fail "package pattern left the outer constructor unevaluated"
