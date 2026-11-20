{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Data.IORef (newIORef, readIORef)
import NativeIndexedGadtFieldsProvider (Shape(..), inspectCons, inspectKnown)
import PackedVecLibrary (Nat(..), Vec(..))

observe :: Vec Bool n -> Bool
observe xs = case inspectCons xs of
    Just ConsSeen -> True
    Nothing -> False

observeKnown :: Vec Bool ('S n) -> Bool
observeKnown xs = case inspectKnown xs of
    ConsSeen -> True

main :: IO ()
main = do
    value <- newIORef (VCons (error "head") (error "tail") :: Vec Bool ('S 'Z)) >>= readIORef
    spine <- try (evaluate (observe (error "spine" :: Vec Bool ('S 'Z))))
    print (observe value, observeKnown value, observe VNil,
           either (const True) (const False) (spine :: Either ErrorCall Bool))
