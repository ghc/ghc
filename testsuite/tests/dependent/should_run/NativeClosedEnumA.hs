{-# LANGUAGE GADTs #-}
module NativeClosedEnumA
  (Token(..), Gadt(..), Abstract, Strict(..), choose, score, chooseGadt,
   scoreGadt, abstractValue, strictValue) where

import Numeric.Natural (Natural)

data Token
  = Ready | Done | C02 | C03 | C04 | C05
  | C06 | C07 | C08 | C09 | C10 | C11

data Gadt where
  First :: Gadt
  Second :: Gadt

data Abstract = Private
data Strict = Field !Natural

choose :: Natural -> Token
choose 0 = Ready
choose 1 = Done
choose _ = C11

score :: Token -> Natural
score Ready = 7
score Done = 2
score C11 = 11
score _ = 0

chooseGadt :: Natural -> Gadt
chooseGadt 0 = First
chooseGadt _ = Second

scoreGadt :: Gadt -> Natural
scoreGadt First = 6
scoreGadt Second = 9

{-# INLINABLE abstractValue #-}
abstractValue :: Natural -> Abstract
abstractValue _ = Private

strictValue :: Natural -> Strict
strictValue _ = errorWithoutStackTrace "unsupported enum result ran"
