{-# LANGUAGE GADTs, OverloadedRecordDot, DataKinds #-}

module T24891 where

import GHC.Records

data T a where
  T1 :: T Int
  T2 :: {sel :: Int} -> T Bool
  T3 :: T Bool

f :: T Bool -> Int
f x = x.sel -- warn, but only once, suggesting to match on T3

data Dot = No | Yes {sel2 :: Int}

ldiDot :: Dot -> Int
ldiDot No = 0
ldiDot d  = d.sel2 -- do not warn

accessDot :: HasField "sel2" t Int => t -> Int
accessDot x = x.sel2 -- do not warn

solveDot :: Dot -> Int
solveDot = accessDot -- warn

data Dot2 t = No2 | Yes2 {sel3 :: t}

accessDot2 :: HasField "sel2" t Int => Dot2 t -> Int
accessDot2 x = x.sel3.sel2 -- warn about x.sel3
