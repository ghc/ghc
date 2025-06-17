{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
module T19700 where

import GHC.Exts (Int(..), Int#, SmallArray#, SmallMutableArray#, State#,
                 indexSmallArray#, newSmallArray#, sizeofSmallArray#, unsafeFreezeSmallArray#, writeSmallArray#)
import GHC.ST (ST(..), runST)
import Prelude hiding (length)

-- | /O(n)/ Transform this map by applying a function to every value.
mapWithKey :: (k -> v1 -> v2) -> HashMap k v1 -> HashMap k v2
mapWithKey f = go
  where
    go Empty                 = Empty
    go (Leaf h (L k v))      = leaf h k (f k v)
    go (BitmapIndexed b ary) = BitmapIndexed b $ map' go ary
    go (Full ary)            = Full $ map' go ary
    go (Collision h ary)     =
        Collision h $ map' (\ (L k v) -> let !v' = f k v in L k v') ary
{-# INLINE mapWithKey #-}

leaf :: Hash -> k -> v -> HashMap k v
leaf h k = \ !v -> Leaf h (L k v)
{-# INLINE leaf #-}

data Leaf k v = L !k !v

data HashMap k v
    = Empty
    | BitmapIndexed !Bitmap !(Array (HashMap k v))
    | Leaf !Hash !(Leaf k v)
    | Full !(Array (HashMap k v))
    | Collision !Hash !(Array (Leaf k v))

type Hash   = Word
type Bitmap = Word

data Array a = Array {
      unArray :: !(SmallArray# a)
    }

data MArray s a = MArray {
      unMArray :: !(SmallMutableArray# s a)
    }

indexM :: Array a -> Int -> ST s a
indexM ary _i@(I# i#) =
        case indexSmallArray# (unArray ary) i# of (# b #) -> return b
{-# INLINE indexM #-}

length :: Array a -> Int
length ary = I# (sizeofSmallArray# (unArray ary))
{-# INLINE length #-}

map' :: (a -> b) -> Array a -> Array b
map' f = \ ary ->
    let !n = length ary
    in run $ do
        mary <- new_ n
        go ary mary 0 n
  where
    go ary mary i n
        | i >= n    = return mary
        | otherwise = do
             x <- indexM ary i
             write mary i $! f x
             go ary mary (i+1) n
{-# INLINE map' #-}

new :: Int -> a -> ST s (MArray s a)
new (I# n#) !b =
    ST $ \s ->
        case newSmallArray# n# b s of
            (# s', ary #) -> (# s', MArray ary #)
{-# INLINE new #-}

new_ :: Int -> ST s (MArray s a)
new_ n = new n undefinedElem

run :: (forall s . ST s (MArray s e)) -> Array e
run act = runST $ act >>= unsafeFreeze
{-# INLINE run #-}

undefinedElem :: a
undefinedElem = error "Data.Strict.HashMap.Autogen.Internal.Array: Undefined element"
{-# NOINLINE undefinedElem #-}

unsafeFreeze :: MArray s a -> ST s (Array a)
unsafeFreeze mary
    = ST $ \s -> case unsafeFreezeSmallArray# (unMArray mary) s of
                   (# s', ary #) -> (# s', Array ary #)
{-# INLINE unsafeFreeze #-}

write :: MArray s a -> Int -> a -> ST s ()
write ary _i@(I# i#) !b = ST $ \ s ->
        case writeSmallArray# (unMArray ary) i# b s of
            s' -> (# s' , () #)
{-# INLINE write #-}
