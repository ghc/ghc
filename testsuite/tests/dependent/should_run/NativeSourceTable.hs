{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}

module Main where

import ClosedTableLibrary
import Control.Exception (ArithException(DivideByZero), evaluate, throw, try)
import Data.Type.Equality ((:~:)(Refl))

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

main :: IO ()
main = case (flipProof, signalProof, phaseProof) of
  (Refl, Refl, Refl) -> do
    print (map flipBit [False, True])
    print (map signal [Red, Amber, Green])
    forced <- try (evaluate (flipBit (throw DivideByZero)))
    print (forced == Left DivideByZero)
