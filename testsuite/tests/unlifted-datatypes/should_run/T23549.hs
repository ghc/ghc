{-# LANGUAGE GADTs, TypeData, TypeFamilies #-}
module Main where

import GHC.Types (Type)

import T23549a

type data TDWrapper = TDWrapperCon Type

type family UnwrapTDW tdw where
  UnwrapTDW (TDWrapperCon a) = a

data ProxyBox tdw = ProxyBox (UnliftedGADTProxy (UnwrapTDW tdw))

shouldBeExhaustive :: ProxyBox (TDWrapperCon Int) -> ()
shouldBeExhaustive pb = id $ case pb of ProxyBox UGPInt -> ()

main :: IO ()
main = let
  pb :: ProxyBox (TDWrapperCon Int)
  pb = ProxyBox UGPInt
  !_ = shouldBeExhaustive pb
  in putStrLn "OK"
