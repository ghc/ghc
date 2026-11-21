{-# LANGUAGE CPP, DataKinds, NoExperimentalDependentTypes #-}

module Main (main) where

import Data.Proxy (Proxy (..))
import GHC.Sigma (Sigma (Pair), first)
import NativeClosedFamilySumProvider (KindCapture, Pack, Tagged (..))

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

kindCapture :: KindCapture True Bool False
kindCapture = Pair True (Proxy :: Proxy True, Proxy :: Proxy False)

main :: IO ()
main = print (first closed, first captured, first lazyPayload, first kindCapture)
