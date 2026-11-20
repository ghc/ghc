{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables, TypeAbstractions, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeReverseLengthProof (lengthReverse, showPreludeReverseLength) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeAppendLengthSource (Plus)
import PreludeMapLengthProof (Spine(..))
import qualified PreludeMapLengthSize as S
import PreludeMapLengthSource (Count)
import PreludeReverseLengthSource (Reversed, ReversedOnto)

-- | Move a successor from the right operand to the result.
plusSucc :: Spine xs -> Proxy n
         -> Plus (Count xs) ('S.Succ n) :~: 'S.Succ (Plus (Count xs) n)
plusSucc Empty _ = Refl
plusSucc (Cons xs) n = case plusSucc xs n of
    Refl -> Refl

-- | Prove that adding zero on the right preserves the count.
plusZero :: Spine xs -> Plus (Count xs) 'S.Zero :~: Count xs
plusZero Empty = Refl
plusZero (Cons xs) = case plusZero xs of
    Refl -> Refl

-- | Count the result of the checked worker with any accumulator.
accumulatorInvariant :: forall a (xs :: [a]) (acc :: [a]). Spine xs -> Proxy acc
                     -> Count (ReversedOnto xs acc) :~: Plus (Count xs) (Count acc)
accumulatorInvariant Empty _ = Refl
accumulatorInvariant (Cons @_ @x xs) _ =
    case accumulatorInvariant xs (Proxy @(x ': acc)) of
        Refl -> case plusSucc xs (Proxy @(Count acc)) of
            Refl -> Refl

-- | Prove that Prelude reverse preserves the length.
lengthReverse :: Spine xs -> Count (Reversed xs) :~: Count xs
lengthReverse xs = case accumulatorInvariant xs (Proxy @'[]) of
    Refl -> case plusZero xs of
        Refl -> Refl

-- | Count the constructors of the installed reverse result.
{-# NOINLINE observedLength #-}
observedLength :: [a] -> S.Size
observedLength xs = S.size (reverse xs)

-- | Observe the proof and count a reversed spine with bottom elements.
showPreludeReverseLength :: String
showPreludeReverseLength =
    show ( lengthReverse (Cons (Cons Empty) :: Spine '[ 'True, 'False])
         , observedLength [bottom, bottom]
         )
  where
    bottom = error "reverse forced an element" :: Bool
