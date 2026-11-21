{-# LANGUAGE DataKinds, ExplicitNamespaces, GADTs, ImpredicativeTypes, RequiredTypeArguments, StandaloneKindSignatures, TypeApplications #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import TerminalGadtForAllProvider

type Promoted :: Promotable Char Bool
type Promoted = 'Payload @Char 'True Bool

number :: G a b -> Int
number (After n (type b)) = n
number (Force n (type b)) = n
number (Refine _ (type b)) = 0

refined :: forall a. G a a -> Bool
refined (Refine P (type Int)) = True
refined _ = False

partial :: Int -> forall b -> G Char b
partial = After @Char

main :: IO ()
main = do
  print (number (partial 37 (type Bool)))
  print (refined (Refine @Int P (type Int)))
  print (seq @(forall b -> G Char b) (After @Char (error "lazy")) True)
  demanded <- try (evaluate (seq @(forall b -> G Char b) (Force @Char (error "strict")) False))
  case demanded :: Either SomeException Bool of
    Left _ -> print True
    Right _ -> print False
