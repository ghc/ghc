{-# LANGUAGE GHC2021, DataKinds, TypeFamilies #-}
module Main where

import Data.Maybe
import Data.Kind

main :: IO ()
main = putStrLn str

str :: String
str = case runInstrImpl @(TOption TUnit) mm MAP of
         C VOption -> "good"
         C Unused -> "bad"

runInstrImpl :: forall inp out. Value (MapOpRes inp TUnit) -> Instr inp out -> Rec out
runInstrImpl m MAP = C m

type MapOpRes :: T -> T -> T
type family MapOpRes c :: T -> T
type instance MapOpRes ('TOption x) = 'TOption

mm :: Value (TOption TUnit)
mm = VOption
{-# NOINLINE mm #-}

type Value :: T -> Type
data Value t where
  VOption :: Value ('TOption t)
  Unused :: Value t

data T = TOption T | TUnit

data Instr (inp :: T) (out :: T) where
  MAP :: Instr c (TOption (MapOpRes c TUnit))

data Rec :: T -> Type where
  C :: Value r -> Rec (TOption r)
