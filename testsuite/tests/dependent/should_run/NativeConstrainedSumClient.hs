{-# LANGUAGE CPP, DataKinds, FlexibleContexts, GADTs, ImpredicativeTypes, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}

module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
#ifdef REPRESENTATIONAL_ONLY
import Data.Coerce (Coercible)
#endif
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.Sigma (Sigma (Pair), first)
import NativeAssociatedSumProvider (DefaultSum (Defaulted), Tagged (..))
import NativeConstrainedSumProvider (Allowed, Choice (Shape), Forward (Forwarded), Permit (Permit), Staged (Staged), make)

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

readStaged :: Staged a -> Bool
readStaged (Staged _ pkg) = case pkg of Pair tag _ -> tag

main :: IO ()
main = do
    outer <- try (evaluate (readPair (error "outer pair"))) :: IO (Either ErrorCall Bool)
    case outer of
        Left _ -> do
            if readDefaultPair False || not (readGivenPair Permit (Proxy :: Proxy Bool) True)
               then fail "incorrect witness from a constructed package"
               else pure ()
            let pkg = Pair False (error "unused default payload") :: Defaulted Char
                shaped = Pair True (error "unused shaped payload") :: Shape Bool Char
                staged = Staged (error "unused earlier field" :: Proxy Bool)
                                (Pair True (error "unused later payload"))
            print (readFirst pkg, readPair pkg, readPayload (Pair True (Yes 'x')),
                   readGiven Permit (Proxy :: Proxy Bool), readKnown shaped,
                   readRefined shaped, readProof Refl shaped, readStaged staged)
        Right _ -> fail "package pattern left the outer constructor unevaluated"

readDefaultPair :: Bool -> Bool
readDefaultPair tag = readPair (Pair tag (error "unused constructed default payload"))

readForwarded :: Allowed a => Proxy a -> Forwarded [a] -> Bool
readForwarded _ = first

readGivenPair :: Permit a -> Proxy a -> Bool -> Bool
readGivenPair Permit ix tag = readForwarded ix (Pair tag (error "unused constructed forwarded payload"))

#ifdef WRONG_CONSTRUCTED_PAYLOAD
badConstructorPayload :: (Bool, Char)
badConstructorPayload = readPayload (Pair False (Yes 'x'))
#endif

#ifdef MISSING_CONSTRUCTOR_DOMAIN
badConstructorDomain :: Bool
badConstructorDomain = readForwarded (Proxy :: Proxy Int) (Pair True (error "missing constructor domain"))
#endif
