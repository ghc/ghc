{-# LANGUAGE DataKinds, GADTs, MagicHash #-}
module NativeClosedEnumA
  (Token(..), Gadt(..), Abstract, Hidden, Strict(..), Unboxed(..),
   choose, score, chooseGadt, scoreGadt, abstractValue, scoreAbstract,
   strictValue, readStrict, unboxedValue, A(..), B(..), mutual, readMutual,
   Growth(..), depth, expanding) where

import GHC.Exts (Int#)
import Numeric.Natural (Natural)

data Token
  = Ready | Done | C02 | C03 | C04 | C05
  | C06 | C07 | C08 | C09 | C10 | C11

data Gadt where
  First :: Gadt
  Second :: Gadt

data Abstract = Private
type Hidden = 'Private

data Strict = Field !Natural
data Unboxed = Raw Int#

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

scoreAbstract :: Abstract -> Natural
scoreAbstract _ = errorWithoutStackTrace "ran a private constructor operand"

strictValue :: Natural -> Strict
strictValue = Field

readStrict :: Strict -> Natural
readStrict (Field n) = n

unboxedValue :: Natural -> Unboxed
unboxedValue _ = errorWithoutStackTrace "ran an unsupported unboxed result"

data A a = End a | Step (B a)
data B a = Back (A a)

mutual :: Natural -> A Natural
mutual n = Step (Back (End n))

readMutual :: A Natural -> Natural
readMutual (End n) = n
readMutual (Step (Back next)) = readMutual next

data Growth a = Stop | Grow (Growth [a])

depth :: Growth Natural -> Natural
depth Stop = 1
depth (Grow _) = 2

expanding :: Natural -> Growth Natural
expanding _ = errorWithoutStackTrace "ran an expanding result"
