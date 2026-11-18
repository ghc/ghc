{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}

module Main where

import ClosedTableLibrary
import Control.Exception (ArithException(DivideByZero), evaluate, throw, try)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), boolVector, isSuccessor, listCount, natToInt, previous,
                         sourceLength, sourceVector, toList)

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

lengthProof :: listCount '[ 'True, 'False] :~: 'S ('S 'Z)
lengthProof = Refl

tailProof :: Proxy (xs :: [Bool]) -> listCount ('True ': xs) :~: 'S (listCount xs)
tailProof _ = Refl

type EmptyInts = sourceLength @Int '[]

emptyProof :: EmptyInts :~: 'Z
emptyProof = Refl

polyStep :: forall a (x :: a) (xs :: [a]). Proxy x -> Proxy xs
         -> sourceLength @a (x ': xs) :~: 'S (sourceLength xs)
polyStep _ _ = Refl

-- Both the type and the recursive term can supply the specified argument.
type Singleton = count @Bool '[ 'False]

count :: forall a. [a] -> Nat
count []       = Z
count (_ : xs) = S (count @a xs)

localSingleton :: Singleton :~: 'S 'Z
localSingleton = Refl

-- The recursive equation can precede its input datatype in this group.
bitCount :: Bits -> Nat
bitCount End = Z
bitCount (Bit _ bs) = S (bitCount bs)

data Bits = End | Bit Bool Bits

bitsProof :: bitCount ('Bit 'False 'End) :~: 'S 'Z
bitsProof = Refl

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
    print (toList (boolVector [False, True]), natToInt (listCount [throw DivideByZero]))
    print (isSuccessor (listCount (True : throw DivideByZero)))
    spine <- try (evaluate (natToInt (listCount (True : throw DivideByZero))))
    print (spine == Left DivideByZero)
    let ints = [11, 22] :: [Int]
    print (toList (sourceVector @Int ints))
