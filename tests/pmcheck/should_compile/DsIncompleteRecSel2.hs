-- Case for long-range info and GADTs caught with PMC
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
module DsIncompleteRecSel2 where

data T a where
    T1 :: { x :: Bool } -> T a
    T2 :: T Bool
    T3 :: T Int
    T4 :: Int -> T Bool

f :: T Char -> Bool
f a = x a

f2 :: T Int -> Bool
f2 T3 = True
f2 a = x a

f3 :: T Bool -> Bool
f3 T2 = True
f3 (T4 1) = False
f3 a = x a


pattern G <- _
pattern P <- T1 _
f4 :: T Int -> Bool
f4 a@G | P <- a = x a
f4 _ = True