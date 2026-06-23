{-# LANGUAGE BangPatterns #-}

-- See Note [Stable Core dump order] in GHC.Core.Ppr.
--
-- A small Data.Map-style module exercising the trickier parts of the stable
-- dump ordering. Under -O it produces, alongside the user functions:
--   * derived Eq/Ord instances for a custom Key type ($fEqKey/$fOrdKey/...),
--   * a call-site specialisation of lookupG (findI_$slookupG), and
--   * a worker/wrapper split of the recursive, strict rotate ($wrotate).
-- Each generated binder inherits its origin's source span, so the stable order
-- clusters it next to that origin. The source order is deliberately neither
-- alphabetical nor the default dump order (insertG forward-references balance),
-- so the test pins source-position ordering specifically.
module T27296
  ( Key(..), size, lookupG, member, findI, rotate, insertG, insertManyI
  , insertTwoI, weight, balance, ratios, fromAscI )
  where

-- A custom key with a derived Ord instance: the derived $fEqKey/$fOrdKey
-- binders inherit this declaration's source span, so they cluster here.
data Key = Key Int deriving (Eq, Ord)

data Map k a = Tip | Bin !Int k a !(Map k a) !(Map k a)

data Sizes = Sizes !Int !Int

size :: Map k a -> Int
size Tip              = 0
size (Bin sz _ _ _ _) = sz

lookupG :: Ord k => k -> Map k a -> Maybe a
lookupG _ Tip = Nothing
lookupG k (Bin _ kx x l r) = case compare k kx of
  LT -> lookupG k l
  GT -> lookupG k r
  EQ -> Just x
{-# SPECIALISE lookupG :: Key -> Map Key a -> Maybe a #-}

member :: Key -> Map Key a -> Bool
member k m = case lookupG k m of
  Nothing -> False
  Just _  -> True

findI :: Key -> Map Key a -> a -> a
findI k m def = case lookupG k m of
  Nothing -> def
  Just v  -> v

-- rotate is recursive and strict in the product 'Sizes', so worker/wrapper
-- unboxes it into a recursive worker ($wrotate). The loop only repackages the
-- fields (no arithmetic), so the worker is stable across build flavours.
rotate :: Sizes -> [a] -> Sizes
rotate s []               = s
rotate (Sizes a b) (_:xs) = rotate (Sizes b a) xs

-- insertG references 'balance', which is defined further down (forward ref).
insertG :: Ord k => k -> a -> Map k a -> Map k a
insertG k x Tip = Bin 1 k x Tip Tip
insertG k x (Bin sz kx kv l r) = case compare k kx of
  LT -> balance kx kv (insertG k x l) r
  GT -> balance kx kv l (insertG k x r)
  EQ -> Bin sz k x l r
{-# SPECIALISE insertG :: Key -> a -> Map Key a -> Map Key a #-}

insertManyI :: [(Key, a)] -> Map Key a -> Map Key a
insertManyI xs m0 = foldr (\(k, x) m -> insertG k x m) m0 xs

insertTwoI :: Key -> Key -> a -> Map Key a
insertTwoI k1 k2 x = insertG k1 x (insertG k2 x Tip)

-- weight unboxes the strict fields of Sizes -> worker/wrapper $wweight.
weight :: Sizes -> Int
weight (Sizes a b) = a * a + 3 * b * b + a * b + 1

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r = Bin (weight (Sizes sl sr)) k x l r
  where
    sl = size l
    sr = size r

-- baseRatios is a closed constant under a lambda -> floated to a top-level lvl.
ratios :: Int -> [Int]
ratios n = map (n +) baseRatios
  where baseRatios = [2, 3, 5, 7, 11, 13]

fromAscI :: [(Key, a)] -> Map Key a
fromAscI = foldr (\(k, x) m -> insertG k x m) Tip
