{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import NativeIndexedConsProof (Seen(..), see, inlineCons, inlineNestedCons, inlineNil, writtenCons, writtenNil)
import NativeIndexedOperandWire (tagCons)
import qualified PackedVecLibrary as P

observesCons :: P.Vec Bool ('P.S 'P.Z) -> Bool
observesCons input = case see input of SeenCons -> True

main :: IO ()
main = do
    let headField = error "forced head" :: Bool
        secondHead = error "forced second head" :: Bool
        tailField = error "forced tail" :: P.Vec Bool 'P.Z
        lazyFields = P.VCons headField tailField
    unless (observesCons lazyFields) (fail "cons proof changed field demand")
    case inlineNil of SeenNil -> pure ()
    case inlineCons headField tailField of SeenCons -> pure ()
    case inlineNestedCons headField secondHead tailField of SeenCons -> pure ()
    case writtenNil of SeenNil -> pure ()
    case writtenCons headField tailField of SeenCons -> pure ()
    case fst (tagCons headField tailField) of P.VCons _ _ -> pure ()
    result <- try (evaluate (observesCons (error "forced scrutinee")))
              :: IO (Either SomeException Bool)
    case result of
      Left _ -> putStrLn "retained VCons preserves demand"
      Right _ -> fail "case stopped inspecting its scrutinee"
