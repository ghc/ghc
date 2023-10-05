{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module T18005 where

type S1 = Int -> (forall a. a -> a) -> Int

data T1a = MkT1a {unT1a :: S1}
         | Dummy1

newtype T1b = MkT1b S1

unT1b' :: T1b -> S1
unT1b' (MkT1b x) = x

pattern MkT1b' :: S1 -> T1b
pattern MkT1b' {unT1b} <- (unT1b' -> unT1b)

type S2 = Int -> forall a. a -> a

data T2a = MkT2a {unT2a :: S2}
         | Dummy2

newtype T2b = MkT2b S2

unT2b' :: T2b -> S2
unT2b' (MkT2b x) = x

pattern MkT2b' :: S2 -> T2b
pattern MkT2b' {unT2b} <- (unT2b' -> unT2b)

