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

conPat :: B -> ()
conPat !B = ()

parPat :: A -> ()
parPat !(A _) = ()

asPat :: A -> ()
asPat !a@(A _) = ()

sigPat :: B -> ()
sigPat (!B :: B) = ()

insideViewPat :: B -> ()
insideViewPat (id -> !B) = ()

listPat :: [Int] -> ()
listPat ![] = ()
listPat _ = ()

list2Pat :: [Int] -> ()
list2Pat !(_:_) = ()
list2Pat _ = ()

tuplePat :: (B,B) -> ()
tuplePat !(_, _) = ()

casePat :: B -> ()
casePat b = case b of !B -> ()

nPat :: Int -> ()
nPat !1 = ()
nPat _ = ()

unboxedPat :: Int# -> Int
unboxedPat !x = I# x

unliftedBind :: ()
unliftedBind =
  let x :: Int#
      !x = undefined in ()

wildPat :: Int# -> ()
wildPat !_ = ()

-- no warning expected
liftedBind:: ()
liftedBind =
  let x :: Int
      !x = undefined in ()

-- no warning expected
boxedPatNoWarn :: Int -> Int
boxedPatNoWarn !x = x

-- no warning expected
newtyPatNoWarn :: C -> ()
newtyPatNoWarn !(C _) = ()

-- no warning expected
patSynNoWarn :: B -> ()
patSynNoWarn !P = ()

-- no warning expected
wildPatNoWarn :: B -> ()
wildPatNoWarn !_ = ()

-- no warning expected
viewPatNoWarn :: B -> ()
viewPatNoWarn !(id -> B) = ()

-- todo:
-- unlifted tuples
-- list with rebindable syntax
-- SumPat
-- more top-level banged binds
-- more numeric patterns
-- more view patterns
