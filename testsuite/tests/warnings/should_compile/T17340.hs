{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T17340 where

import GHC.Exts

data A = A { a :: () }
data B = B
newtype C = C Int
pattern P = B

f_nowarn :: Bool -> Bool
f_nowarn !x = x

f :: Bool -> Bool
f True = False
f !x   = x

g :: (Int, Int) -> Bool -> ()
g (a,b) True = ()
g !x False = ()

data T = MkT !Int
h :: T -> ()
h (MkT !x) = ()

k :: Bool -> Int
k True = 1
k !_   = 2  -- clause is accessible, so warn for the bang

t :: () -> Bool -> Int
t _   True  = 1
t !() True  = 2 -- the clause has inaccessible RHS, warn for the bang
t _   False = 3

q :: Bool -> Int
q True  = 1
q !True = 2 -- clause is redundant, don't warn for the bang
q False = 3

i :: Bool -> Int
i True       = 1
i !x | x     = 2 -- redundant
     | not x = 3 -- accessible. This one will stay alive, so warn for the bang

newtype T2 a = T2 a
w :: T2 a -> Bool -> ()
w _      True = ()
w (T2 _) True = () -- redundant
w !_     True = () -- inaccessible
w _      _    = ()

z :: T2 a -> Bool -> ()
z _ True                = ()
z t2 !x | T2 _ <- t2, x = () -- redundant
        | !_ <- t2, x   = () -- inaccessable

-- conPat :: B -> ()
-- conPat !B = ()

-- parPat :: A -> ()
-- parPat !(A _) = ()

-- asPat :: A -> ()
-- asPat !a@(A _) = ()

-- sigPat :: B -> ()
-- sigPat (!B :: B) = ()

-- insideViewPat :: B -> ()
-- insideViewPat (id -> !B) = ()

-- listPat :: [Int] -> ()
-- listPat ![] = ()
-- listPat _ = ()

-- list2Pat :: [Int] -> ()
-- list2Pat !(_:_) = ()
-- list2Pat _ = ()

-- tuplePat :: (B,B) -> ()
-- tuplePat !(_, _) = ()

-- casePat :: B -> ()
-- casePat b = case b of !B -> ()

-- nPat :: Int -> ()
-- nPat !1 = ()
-- nPat _ = ()

-- unboxedPat :: Int# -> Int
-- unboxedPat !x = I# x

-- unliftedBind :: ()
-- unliftedBind =
--   let x :: Int#
--       !x = undefined in ()

-- wildPat :: Int# -> ()
-- wildPat !_ = ()

-- -- no warning expected
-- liftedBind:: ()
-- liftedBind =
--   let x :: Int
--       !x = undefined in ()

-- -- no warning expected
-- boxedPatNoWarn :: Int -> Int
-- boxedPatNoWarn !x = x

-- -- no warning expected
-- newtyPatNoWarn :: C -> ()
-- newtyPatNoWarn !(C _) = ()

-- -- no warning expected
-- patSynNoWarn :: B -> ()
-- patSynNoWarn !P = ()

-- -- no warning expected
-- wildPatNoWarn :: B -> ()
-- wildPatNoWarn !_ = ()

-- -- no warning expected
-- viewPatNoWarn :: B -> ()
-- viewPatNoWarn !(id -> B) = ()

-- todo:
-- unlifted tuples
-- list with rebindable syntax
-- SumPat
-- more top-level banged binds
-- more numeric patterns
-- more view patterns
