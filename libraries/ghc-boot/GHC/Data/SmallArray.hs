{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
  , smallArrayFromList
  , smallArrayToList
  , mapSmallArray
  , foldMapSmallArray
  , replicateSmallArrayIO
  , mapSmallArrayIO
  , mapSmallArrayM_
  , imapSmallArrayM_
  , rnfSmallArray

  -- * IO Operations
  , SmallMutableArrayIO
  , newSmallArrayIO
  , writeSmallArrayIO
  , unsafeFreezeSmallArrayIO
  )
where

import GHC.Exts
import GHC.IO
import GHC.ST
import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.Foldable
import Data.List (unfoldr)

data SmallArray a = SmallArray (SmallArray# a)

data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)

type SmallMutableArrayIO a = SmallMutableArray RealWorld a

instance Binary a => Binary (SmallArray a) where
  put sa = put (sizeofSmallArray sa) *> foldMapSmallArray put sa

  get = do
    n <- get
    smallArrayFromList <$> replicateM n get


instance Show a => Show (SmallArray a) where
  showsPrec p = showsPrec p . smallArrayToList

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

-- | Map a function over the elements of a 'SmallArray'
--
mapSmallArray :: (a -> b) -> SmallArray a -> SmallArray b
{-# INLINE mapSmallArray #-}
mapSmallArray f sa = runST $ ST $ \s ->
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

-- | Fold the values of a 'SmallArray' into a 'Monoid m' of choice
foldMapSmallArray :: Monoid m => (a -> m) -> SmallArray a -> m
{-# INLINE foldMapSmallArray #-}
foldMapSmallArray f sa = go 0
  where
    n = sizeofSmallArray sa
    go i
      | i < n = f (indexSmallArray sa i) `mappend` go (i + 1)
      | otherwise = mempty

-- | Execute the 'IO' action the given number of times and store the
-- results in a 'SmallArray'.
{-# INLINE replicateSmallArrayIO #-}
replicateSmallArrayIO :: Int -> IO a -> IO (SmallArray a)
replicateSmallArrayIO n m = do
  arr <- newSmallArrayIO n undefined
  let go i
        | i < n = do
            writeSmallArrayIO arr i =<< m
            go $ succ i
        | otherwise = pure ()
  go 0
  unsafeFreezeSmallArrayIO arr

-- | Apply the 'IO' action to every element, producing a new
-- 'SmallArray'.
{-# INLINE mapSmallArrayIO #-}
mapSmallArrayIO :: (a -> IO b) -> SmallArray a -> IO (SmallArray b)
mapSmallArrayIO f sa = do
  ma <- newSmallArrayIO (sizeofSmallArray sa) undefined
  flip imapSmallArrayM_ sa $ \i v -> writeSmallArrayIO ma i =<< f v
  unsafeFreezeSmallArrayIO ma

-- | Apply the monadic action to every element, ignoring the results.
{-# INLINE mapSmallArrayM_ #-}
mapSmallArrayM_ :: Applicative f => (a -> f b) -> SmallArray a -> f ()
mapSmallArrayM_ f = imapSmallArrayM_ (\_ v -> f v)

-- | Apply the monadic action to every element and its index, ignoring
-- the results.
{-# INLINE imapSmallArrayM_ #-}
imapSmallArrayM_ :: Applicative f => (Int -> a -> f b) -> SmallArray a -> f ()
imapSmallArrayM_ f sa = go 0
  where
    n = sizeofSmallArray sa
    go i
      | i < n = f i (indexSmallArray sa i) *> go (succ i)
      | otherwise = pure ()

-- | Force the elements of the given 'SmallArray'
--
rnfSmallArray :: NFData a => SmallArray a -> ()
{-# INLINE rnfSmallArray #-}
rnfSmallArray sa = go 0
  where
    n = sizeofSmallArray sa
    go !i
      | i < n = rnf (indexSmallArray sa i) `seq` go (i + 1)
      | otherwise = ()

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

-- | Construct a 'SmallArray' from a list. This is different from
-- 'listToArray' since the list elements fill the 'SmallArray'
-- sequentially without recalculating the indices or remapping to
-- other element types.
{-# INLINE smallArrayFromList #-}
smallArrayFromList :: [a] -> SmallArray a
smallArrayFromList vs = runST $ ST $ \s0 ->
  case newSmallArray (length vs) undefined s0 of
    (# s1, ma #) ->
      case foldlM (\i v -> ST $ \s0 ->
        case writeSmallArray ma i v s0 of
          s1 -> (# s1, succ i #)) 0 vs of
            ST m -> case m s1 of
              (# s2, _ #) -> unsafeFreezeSmallArray ma s2

-- | Construct a list from a 'SmallArray'.
{-# INLINE smallArrayToList #-}
smallArrayToList :: SmallArray a -> [a]
smallArrayToList sa = unfoldr go 0
  where
    n = sizeofSmallArray sa
    go i
      | i < n = Just (indexSmallArray sa i, succ i)
      | otherwise = Nothing
