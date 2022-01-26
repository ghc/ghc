{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}

-- | Small-array
module GHC.Data.SmallArray
  ( SmallMutableArray (..)
  , SmallArray (..)
  , newSmallArray
  , getSizeSmallArray
  , readSmallArray
  , writeSmallArray
  , copySmallArray
  , resizeSmallArray
  , freezeSmallArray
  , unsafeFreezeSmallArray
  , indexSmallArray
  , listToArray
  )
where

import GHC.Exts
import GHC.Prelude
import GHC.ST

data SmallArray a = SmallArray (SmallArray# a)

data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)

newSmallArray
  :: Int  -- ^ size
  -> a    -- ^ initial contents
  -> State# s
  -> (# State# s, SmallMutableArray s a #)
{-# INLINE newSmallArray #-}
newSmallArray (I# sz) x s = case newSmallArray# sz x s of
  (# s', a #) -> (# s', SmallMutableArray a #)

getSizeSmallArray
  :: SmallMutableArray s a -- ^ array
  -> State# s
  -> (# State# s, Int #)
{-# INLINE getSizeSmallArray #-}
getSizeSmallArray (SmallMutableArray a) s = case getSizeofSmallMutableArray# a s of
  (# s', n #) -> (# s', I# n #)


readSmallArray
  :: SmallMutableArray s a -- ^ array
  -> Int                   -- ^ index
  -> State# s
  -> (# State# s, a #)
{-# INLINE readSmallArray #-}
readSmallArray (SmallMutableArray a) (I# i) s = readSmallArray# a i s

writeSmallArray
  :: SmallMutableArray s a -- ^ array
  -> Int                   -- ^ index
  -> a                     -- ^ new element
  -> State# s
  -> (# State# s, () #)
{-# INLINE writeSmallArray #-}
writeSmallArray (SmallMutableArray a) (I# i) x s = (# writeSmallArray# a i x s, () #)

-- | Copy a slice of a mutable array.
copySmallArray
  :: SmallMutableArray s a -- ^ source
  -> Int                   -- ^ offset in source
  -> SmallMutableArray s a -- ^ destination
  -> Int                   -- ^ offset in dest
  -> Int                   -- ^ number of elements to copy
  -> State# s
  -> (# State# s, () #)
{-# INLINE copySmallArray #-}
copySmallArray (SmallMutableArray src) (I# offs_src) (SmallMutableArray dst) (I# offs_dst) (I# n) s =
  (# copySmallMutableArray# src offs_src dst offs_dst n s, () #)

resizeSmallArray
  :: SmallMutableArray s a -- ^ array to resize
  -> Int                   -- ^ new size of array
  -> a                     -- ^ initial contents
  -> State# s
  -> (# State# s, SmallMutableArray s a #)
{-# INLINE resizeSmallArray #-}
resizeSmallArray (SmallMutableArray src) (I# size) x s =
  case resizeSmallMutableArray# src size x s of
    (# s', arr #) -> (# s', SmallMutableArray arr #)

-- | Copy and freeze a slice of a mutable array.
freezeSmallArray
  :: SmallMutableArray s a -- ^ source
  -> Int                   -- ^ offset
  -> Int                   -- ^ length
  -> State# s
  -> (# State# s, SmallArray a #)
{-# INLINE freezeSmallArray #-}
freezeSmallArray (SmallMutableArray ma) (I# offset) (I# len) s =
  case freezeSmallArray# ma offset len s of
    (# s', a #) -> (# s', SmallArray a #)

-- | Freeze a mutable array (no copy!)
unsafeFreezeSmallArray
  :: SmallMutableArray s a
  -> State# s
  -> (# State# s, SmallArray a #)
{-# INLINE unsafeFreezeSmallArray #-}
unsafeFreezeSmallArray (SmallMutableArray ma) s =
  case unsafeFreezeSmallArray# ma s of
    (# s', a #) -> (# s', SmallArray a #)


-- | Index a small-array (no bounds checking!)
indexSmallArray
  :: SmallArray a -- ^ array
  -> Int          -- ^ index
  -> a
{-# INLINE indexSmallArray #-}
indexSmallArray (SmallArray sa#) (I# i) = case indexSmallArray# sa# i of
  (# v #) -> v


-- | Convert a list into an array.
listToArray :: Int -> (e -> Int) -> (e -> a) -> [e] -> SmallArray a
{-# INLINE listToArray #-}
listToArray (I# size) index_of value_of xs = runST $ ST \s ->
  let
    index_of' e = case index_of e of I# i -> i
    write_elems ma es s = case es of
      []    -> s
      e:es' -> case writeSmallArray# ma (index_of' e) (value_of e) s of
                 s' -> write_elems ma es' s'
  in
  case newSmallArray# size undefined s of
    (# s', ma #) -> case write_elems ma xs s' of
      s'' -> case unsafeFreezeSmallArray# ma s'' of
        (# s''', a #) -> (# s''', SmallArray a #)
