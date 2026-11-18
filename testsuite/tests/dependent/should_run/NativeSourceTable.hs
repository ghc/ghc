{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}

module Main where

import ClosedTableLibrary
import Control.Exception (ArithException(DivideByZero), evaluate, throw, try)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), isSuccessor, natToInt, previous)

type Flipped = '[flipBit 'False, flipBit 'True]

flipProof :: Flipped :~: '[ 'True, 'False]
flipProof = Refl

type Signals = '[signal 'Red, signal 'Amber, signal 'Green]

signalProof :: Signals :~: '[ 'Stop, 'Stop, 'Go]
signalProof = Refl

-- The local table and its input type both follow the type that uses them.
type FirstSignal = atPhase 'Start

phaseProof :: FirstSignal :~: 'Stop
phaseProof = Refl

data Phase = Start | Proceed

atPhase :: Phase -> Signal
atPhase Start   = Stop
atPhase Proceed = Go

-- A known constructor is enough to project a field whose value is unknown.
fieldProof :: Proxy n -> previous ('S n) :~: n
fieldProof _ = Refl

type LastSignal = after ('Change 'Stop 'Go)

afterProof :: LastSignal :~: 'Go
afterProof = Refl

data Change = Change Signal Signal

after :: Change -> Signal
after (Change _ new) = new

main :: IO ()
main = case (flipProof, signalProof, phaseProof) of
  (Refl, Refl, Refl) -> do
    print (map flipBit [False, True])
    print (map signal [Red, Amber, Green])
    forced <- try (evaluate (flipBit (throw DivideByZero)))
    print (forced == Left DivideByZero)
    print (natToInt (previous (S (S Z))), isSuccessor (S (throw DivideByZero)))
    demanded <- try (evaluate (natToInt (previous (S (throw DivideByZero)))))
    print (demanded == Left DivideByZero)
