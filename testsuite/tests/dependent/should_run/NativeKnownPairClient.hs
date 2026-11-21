{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, NoExperimentalDependentTypes #-}

module Main (main) where

import GHC.Sigma (Sigma (Pair), first)
import NativeKnownPairProvider

#ifdef NAMED
type Package = Named
#else
type Package = Open Bool
#endif

value :: Package
value = Pair True (error "unused payload" :: Payload True)

flag :: Package -> Bool
flag p = first p

nested :: Bool
nested = flag (Pair True (error "unused nested payload" :: Payload True))

#ifdef BAD_CONSTRUCTION
wrong :: Open Bool
wrong = Pair True (No 9)

independent :: forall a. Open a
independent = Pair True (Yes 1)
#endif

main :: IO ()
main = print (flag value, nested)
