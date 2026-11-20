{-# LANGUAGE BangPatterns, GADTs, PatternSynonyms, ViewPatterns #-}
module T28014 where

import Data.List (uncons)
import Data.Void

-- | Never matches, and does not force its argument.
pattern F :: a
pattern F <- (const False -> True)

-- | The bang is not redundant: withBang undefined is ⊥, but would be 2
-- without the bang.
withBang :: Bool -> Int
withBang F  = 1
withBang !_ = 2

data T = MkT Void

-- | Not exhaustive: h (MkT undefined) fails to match.
h :: T -> Int
h (MkT F) = 1

pattern Nil :: [a]
pattern Nil <- (null -> True)
pattern Cons :: a -> [a] -> [a]
pattern Cons x xs <- (uncons -> Just (x, xs))
{-# COMPLETE Nil, Cons #-}

-- | Exhaustive, and the last clause is redundant.
g :: [a] -> Int
g Nil        = 0
g (Cons _ _) = 1
g _          = 2

-- | The last clause is redundant.
gBang :: [a] -> Int
gBang Nil        = 0
gBang (Cons _ _) = 1
gBang !_         = 2

data G a where
  G1' :: G Int
  G2' :: G Bool

pattern G1 :: () => (a ~ Int) => G a
pattern G1 = G1'
pattern G2 :: () => (a ~ Bool) => G a
pattern G2 = G2'
{-# COMPLETE G1, G2 #-}

-- | Exhaustive: G2 cannot match at G Int.
fb :: G Int -> Int
fb G1 = 1

data U = MkU (G Char)

-- | Exhaustive, by the COMPLETE pragma.
hu :: U -> Int
hu (MkU G1) = 1

-- | Not exhaustive: no member of the COMPLETE set for G can match at G Char,
-- and hc (MkU F) fails on MkU undefined.
hc :: U -> Int
hc (MkU F) = 1

-- | E Int has no constructor.
data E a where
  E1 :: E Bool

-- | No clause is inaccessible: w undefined G1' is 2.
w :: E a -> G a -> Int
w F _   = 1
w _ G1' = 2
w _ G2' = 3

pattern J :: a -> Maybe a
pattern J x = Just x

-- | The bang is dead, but the checker can't see that J forces its argument.
k :: Maybe Int -> Int
k (J _) = 1
k !_    = 2
