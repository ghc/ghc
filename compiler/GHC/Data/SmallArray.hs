{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Small-array
module GHC.Data.SmallArray
  ( SmallMutableArray (..)
  , SmallArray (..)
  , newSmallArray
  , writeSmallArray
  , freezeSmallArray
  , unsafeFreezeSmallArray
  , indexSmallArray
  , sizeofSmallArray
  , listToArray

  -- * IO Operations
  , SmallMutableArrayIO
  , newSmallArrayIO
  , writeSmallArrayIO
  , unsafeFreezeSmallArrayIO
  )
where

import GHC.Exts
import GHC.Prelude
import GHC.IO
import GHC.ST
import GHC.Utils.Binary
import Control.DeepSeq
import Data.Foldable

data SmallArray a = SmallArray (SmallArray# a)

data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)

type SmallMutableArrayIO a = SmallMutableArray RealWorld a

newSmallArray
  :: Int  -- ^ size
  -> a    -- ^ initial contents
  -> State# s
  -> (# State# s, SmallMutableArray s a #)
{-# INLINE newSmallArray #-}
newSmallArray (I# sz) x s = case newSmallArray# sz x s of
  (# s', a #) -> (# s', SmallMutableArray a #)

newSmallArrayIO :: Int -> a -> IO (SmallMutableArrayIO a)
newSmallArrayIO sz x = IO $ \s -> newSmallArray sz x s

writeSmallArray
  :: SmallMutableArray s a -- ^ array
  -> Int                   -- ^ index
  -> a                     -- ^ new element
  -> State# s
  -> State# s
{-# INLINE writeSmallArray #-}
writeSmallArray (SmallMutableArray a) (I# i) x = writeSmallArray# a i x

writeSmallArrayIO :: SmallMutableArrayIO a
                  -> Int
                  -> a
                  -> IO ()
writeSmallArrayIO a ix v = IO $ \s -> (# writeSmallArray a ix v s, () #)


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

unsafeFreezeSmallArrayIO :: SmallMutableArrayIO a -> IO (SmallArray a)
unsafeFreezeSmallArrayIO arr = IO $ \s -> unsafeFreezeSmallArray arr s

-- | Get the size of a 'SmallArray'
sizeofSmallArray
  :: SmallArray a
  -> Int
{-# INLINE sizeofSmallArray #-}
sizeofSmallArray (SmallArray sa#) =
  case sizeofSmallArray# sa# of
    s -> I# s

-- | Index a small-array (no bounds checking!)
indexSmallArray
  :: SmallArray a -- ^ array
  -> Int          -- ^ index
  -> a
{-# INLINE indexSmallArray #-}
indexSmallArray (SmallArray sa#) (I# i) =
  case indexSmallArray# sa# i of
    (# v #) -> v

instance Functor SmallArray where
  fmap f sa = runST $ ST $ \s ->
    let
      n = sizeofSmallArray sa
      go !i saMut# state#
        | i < n =
          let
            a = indexSmallArray sa i
            newState# = writeSmallArray saMut# i (f a) state#
          in
            go (i + 1) saMut# newState#
        | otherwise = state#
    in
    case newSmallArray n (error "SmallArray: internal error, uninitialised elements") s of
      (# s', mutArr #) ->
        case go 0 mutArr s' of
          s'' -> unsafeFreezeSmallArray mutArr s''
  {-# INLINE fmap #-}

instance Foldable SmallArray where
  foldMap f sa = go 0
    where
      n = sizeofSmallArray sa
      go i
        | i < n = f (indexSmallArray sa i) `mappend` go (i + 1)
        | otherwise = mempty
  {-# INLINE foldMap #-}

  null sa = sizeofSmallArray sa == 0
  {-# INLINE null #-}

  length = sizeofSmallArray
  {-# INLINE length #-}

  toList sa = go 0
    where
      n = sizeofSmallArray sa
      go i
        | i < n = indexSmallArray sa i : go (i + 1)
        | otherwise = []
  {-# INLINE toList #-}

instance Traversable SmallArray where
  traverse f sa = fromListN n <$> go 0
    where
      n = sizeofSmallArray sa
      go i
        | i < n = liftA2 (:) (f (indexSmallArray sa i)) (go (i + 1))
        | otherwise = pure []
  {-# INLINE traverse #-}

instance NFData a => NFData (SmallArray a) where
  rnf sa = go 0
    where
      n = sizeofSmallArray sa
      go !i
        | i < n = rnf (indexSmallArray sa i) `seq` go (i + 1)
        | otherwise = ()
  {-# INLINE rnf #-}

instance IsList (SmallArray a) where
  type Item (SmallArray a) = a

  fromList xs = fromListN (length xs) xs
  {-# INLINE fromList #-}

  fromListN n xs
    | n < 0 = error "SmallArray.fromListN: negative length"
    | otherwise = runST $ ST $ \s ->
        case newSmallArray n (error "SmallArray: internal error, uninitialised elements") s of
          (# s', ma #) ->
            let
              fill !i ys state#
                | i < n = case ys of
                    y:ys' -> fill (i + 1) ys' (writeSmallArray ma i y state#)
                    []    -> error "SmallArray.fromListN: insufficient elements"
                | otherwise = state#
            in case fill 0 xs s' of
                 s'' -> unsafeFreezeSmallArray ma s''
  {-# INLINE fromListN #-}

  toList sa = go 0
    where
      n = sizeofSmallArray sa
      go i
        | i < n = indexSmallArray sa i : go (i + 1)
        | otherwise = []
  {-# INLINE toList #-}

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

instance (Binary a) => Binary (SmallArray a) where
  get bh = do
    len <- get bh
    ma <- newSmallArrayIO len undefined
    for_ [0 .. len - 1] $ \i -> do
      a <- get bh
      writeSmallArrayIO ma i a
    unsafeFreezeSmallArrayIO ma

  put_ bh sa = do
    let len = sizeofSmallArray sa
    put_ bh len
    for_ [0 .. len - 1] $ \i -> put_ bh $ sa `indexSmallArray` i
