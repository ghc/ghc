{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnboxedTuples         #-}

module T26615a (HashMap, disjointSubtrees) where

import           Data.Bits
import           GHC.Exts
import           Prelude          hiding (filter, length, foldr)

data Leaf k v = L !k v

data Array a = Array { unArray :: !(SmallArray# a) }

data HashMap k v
    = Empty
    | Leaf !Word !(Leaf k v)
    | Collision !Word !(Array (Leaf k v))
    | BitmapIndexed !Word !(Array (HashMap k v))
    | Full !(Array (HashMap k v))

disjointSubtrees :: Eq k => Int -> HashMap k a -> HashMap k b -> Bool
disjointSubtrees !_s Empty _b = True
disjointSubtrees _ (Leaf hA (L kA _)) (Leaf hB (L kB _)) =
  hA /= hB || kA /= kB
disjointSubtrees s (Leaf hA (L kA _)) b =
  lookupCont (\_ -> True) (\_ _ -> False) hA kA s b
disjointSubtrees s (BitmapIndexed bmA aryA) (BitmapIndexed bmB aryB)
  | bmA .&. bmB == 0 = True
  | aryA `unsafeSameArray` aryB = False
  | otherwise = disjointArrays s bmA aryA bmB aryB
disjointSubtrees s (BitmapIndexed bmA aryA) (Full aryB) =
  disjointArrays s bmA aryA fullBitmap aryB
disjointSubtrees s (Full aryA) (BitmapIndexed bmB aryB) =
  disjointArrays s fullBitmap aryA bmB aryB
disjointSubtrees s (Full aryA) (Full aryB)
  | aryA `unsafeSameArray` aryB = False
  | otherwise = go (maxChildren - 1)
  where
    go i
      | i < 0 = True
      | otherwise = case index# aryA i of
          (# stA #) -> case index# aryB i of
            (# stB #) ->
              disjointSubtrees (nextShift s) stA stB &&
              go (i - 1)
disjointSubtrees s a@(Collision hA _) (BitmapIndexed bmB aryB)
  | m .&. bmB == 0 = True
  | otherwise = case index# aryB i of
      (# stB #) -> disjointSubtrees (nextShift s) a stB
  where
    m = mask hA s
    i = sparseIndex bmB m
disjointSubtrees s a@(Collision hA _) (Full aryB) =
  case index# aryB (index hA s) of
    (# stB #) -> disjointSubtrees (nextShift s) a stB
disjointSubtrees _ (Collision hA aryA) (Collision hB aryB) =
  disjointCollisions hA aryA hB aryB
disjointSubtrees _s _a Empty = True
disjointSubtrees s a (Leaf hB (L kB _)) =
  lookupCont (\_ -> True) (\_ _ -> False) hB kB s a
disjointSubtrees s a b@Collision{} = disjointSubtrees s b a
{-# INLINABLE disjointSubtrees #-}

disjointArrays :: Eq k => Int -> Word -> Array (HashMap k a) -> Word -> Array (HashMap k b) -> Bool
disjointArrays !s !bmA !aryA !bmB !aryB = go (bmA .&. bmB)
  where
    go 0 = True
    go bm = case index# aryA iA of
        (# stA #) -> case index# aryB iB of
          (# stB #) ->
            disjointSubtrees (nextShift s) stA stB &&
            go (bm .&. complement m)
      where
        m = bm .&. negate bm
        iA = sparseIndex bmA m
        iB = sparseIndex bmB m
{-# INLINE disjointArrays #-}

disjointCollisions :: Eq k => Word -> Array (Leaf k a) -> Word -> Array (Leaf k b) -> Bool
disjointCollisions !hA !aryA !hB !aryB
  | hA == hB = all' predicate aryA
  | otherwise = True
  where
    predicate (L kA _) = lookupInArrayCont (\_ -> True) (\_ _ -> False) kA aryB
{-# INLINABLE disjointCollisions #-}

length :: Array a -> Int
length ary = I# (sizeofSmallArray# (unArray ary))
{-# INLINE length #-}

lookupCont ::
  forall rep (r :: TYPE rep) k v.
     Eq k
  => ((# #) -> r)    -- Absent continuation
  -> (v -> Int -> r) -- Present continuation
  -> Word -- The hash of the key
  -> k
  -> Int
  -> HashMap k v -> r
lookupCont absent present !h0 !k0 !s0 m0 = lookupCont_ h0 k0 s0 m0
  where
    lookupCont_ :: Eq k => Word -> k -> Int -> HashMap k v -> r
    lookupCont_ !_ !_ !_ Empty = absent (# #)
    lookupCont_ h k _ (Leaf hx (L kx x))
        | h == hx && k == kx = present x (-1)
        | otherwise          = absent (# #)
    lookupCont_ h k s (BitmapIndexed b v)
        | b .&. m == 0 = absent (# #)
        | otherwise =
            case index# v (sparseIndex b m) of
              (# st #) -> lookupCont_ h k (nextShift s) st
      where m = mask h s
    lookupCont_ h k s (Full v) =
      case index# v (index h s) of
        (# st #) -> lookupCont_ h k (nextShift s) st
    lookupCont_ h k _ (Collision hx v)
        | h == hx   = lookupInArrayCont absent present k v
        | otherwise = absent (# #)
{-# INLINE lookupCont #-}

unsafeSameArray :: Array a -> Array b -> Bool
unsafeSameArray (Array xs) (Array ys) =
  tagToEnum# (unsafeCoerce# reallyUnsafePtrEquality# xs ys)

fullBitmap :: Word
fullBitmap = complement (complement 0 `shiftL` maxChildren)
{-# INLINE fullBitmap #-}

maxChildren :: Int
maxChildren = 1 `unsafeShiftL` bitsPerSubkey

index# :: Array a -> Int -> (# a #)
index# ary _i@(I# i#) = indexSmallArray# (unArray ary) i#
{-# INLINE index# #-}

nextShift :: Int -> Int
nextShift s = s + bitsPerSubkey
{-# INLINE nextShift #-}

mask :: Word -> Int -> Word
mask w s = 1 `unsafeShiftL` index w s
{-# INLINE mask #-}

sparseIndex :: Word -> Word -> Int
sparseIndex b m = popCount (b .&. (m - 1))
{-# INLINE sparseIndex #-}

index :: Word -> Int -> Int
index w s = fromIntegral $ unsafeShiftR w s .&. subkeyMask
{-# INLINE index #-}

lookupInArrayCont ::
  forall rep (r :: TYPE rep) k v.
  Eq k => ((# #) -> r) -> (v -> Int -> r) -> k -> Array (Leaf k v) -> r
lookupInArrayCont absent present k0 ary0 =
    lookupInArrayCont_ k0 ary0 0 (length ary0)
  where
    lookupInArrayCont_ :: Eq k => k -> Array (Leaf k v) -> Int -> Int -> r
    lookupInArrayCont_ !k !ary !i !n
        | i >= n    = absent (# #)
        | otherwise = case index# ary i of
            (# L kx v #)
                | k == kx   -> present v i
                | otherwise -> lookupInArrayCont_ k ary (i+1) n
{-# INLINE lookupInArrayCont #-}

bitsPerSubkey :: Int
bitsPerSubkey = 5

subkeyMask :: Word
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

all' :: (a -> Bool) -> Array a -> Bool
all' p = foldr (\a acc -> p a && acc) True
{-# INLINE all' #-}

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f = \ z0 ary0 -> foldr_ ary0 (length ary0) 0 z0
  where
    foldr_ ary n i z
        | i >= n = z
        | otherwise
        = case index# ary i of
            (# x #) -> f x (foldr_ ary n (i+1) z)
{-# INLINE foldr #-}
