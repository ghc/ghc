{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import NativeIndexedConsProof (Seen(..), see)
import qualified PackedVecLibrary as P

observesCons :: P.Vec Bool ('P.S 'P.Z) -> Bool
observesCons input = case see input of SeenCons -> True

main :: IO ()
main = do
    let lazyFields = P.VCons (error "forced head") (error "forced tail" :: P.Vec Bool 'P.Z)
    unless (observesCons lazyFields) (fail "cons proof changed field demand")
    result <- try (evaluate (observesCons (error "forced scrutinee")))
              :: IO (Either SomeException Bool)
    case result of
      Left _ -> putStrLn "retained VCons preserves demand"
      Right _ -> fail "case stopped inspecting its scrutinee"
