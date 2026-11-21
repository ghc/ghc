{-# LANGUAGE CPP, DataKinds, NoExperimentalDependentTypes #-}

module Main (main) where

import GHC.Sigma (Sigma (Pair), first)
import NativeClosedFamilySumProvider (Pack, Tagged (..))

closed :: Pack False Int
closed = Pair False (FalseTag True)

captured :: Pack True Char
#ifdef BAD_TAG
captured = Pair False (TrueTag 'x')
#else
captured = Pair True (TrueTag 'x')
#endif

lazyPayload :: Pack True Char
lazyPayload = Pair False (error "unused payload" :: Tagged False Char)

main :: IO ()
main = print (first closed, first captured, first lazyPayload)
