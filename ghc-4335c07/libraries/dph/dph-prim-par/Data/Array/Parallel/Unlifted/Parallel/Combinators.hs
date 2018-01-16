{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel combinators for unlifted arrays. 
module Data.Array.Parallel.Unlifted.Parallel.Combinators 
        ( mapUP
        , filterUP
        , packUP
        , combineUP, combine2UP
        , zipWithUP
        , foldUP, foldlUP, fold1UP, foldl1UP
        , scanUP)
where
import Data.Array.Parallel.Base
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Distributed.What
import Data.Array.Parallel.Unlifted.Parallel.UPSel
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Parallel.Combinators." Prelude.++ s


-- | Apply a worker to all elements of an array.
mapUP :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b
mapUP f xs 
        = splitJoinD theGang 
                (mapD (What "mapUP/map") theGang (Seq.map f)) xs
{-# INLINE_UP mapUP #-}


-- | Keep elements that match the given predicate.
filterUP :: Unbox a => (a -> Bool) -> Vector a -> Vector a
filterUP f
        = joinD  theGang unbalanced
        . mapD   (What "filterUP/filter") theGang (Seq.filter f)
        . splitD theGang unbalanced
{-# INLINE_UP filterUP #-}


-- | Take elements of an array where a flag value is true, and pack them into
--   the result. 
-- 
--   * The souce and flag arrays must have the same length, but this is not checked.
--
packUP :: Unbox e => Vector e -> Vector Bool -> Vector e
packUP xs flags 
        = Seq.fsts . filterUP snd $ Seq.zip xs flags
{-# INLINE_UP packUP #-}


-- | Combine two vectors based on a selector. 
--   If the selector is true then take the element from the first vector, 
--   otherwise take it from the second.
--
--   * The data vectors must have enough elements to satisfy the flag vector, 
--     but this is not checked.
--  
combineUP :: Unbox a => Vector Bool -> Vector a -> Vector a -> Vector a
combineUP flags xs ys 
        = checkEq (here "combineUP")
                ("tags length /= sum of args length")
                (Seq.length flags) (Seq.length xs + Seq.length ys)
        $ combine2UP tags (mkUPSelRep2 tags) xs ys
        where tags = Seq.map (fromBool . not) flags
{-# INLINE combineUP #-}


-- | Combine two vectors based on a selector. 
--
--   * The data vectors must have enough elements to satisfy the selector,
--     but this is not checked.
--
combine2UP :: Unbox a => Vector Tag -> UPSelRep2 -> Vector a -> Vector a -> Vector a
combine2UP tags rep !xs !ys 
        = checkEq (here "combine2UP")
                ("tags length /= sum of args length")
                (Seq.length tags) (Seq.length xs + Seq.length ys)
        $ joinD    theGang balanced
        $ zipWithD (What "combine2UP/go") theGang go rep
        $ splitD   theGang balanced tags
        where   go ((i,j), (m,n)) ts 
                 = Seq.combine2ByTag ts 
                        (Seq.slice (here "combine2UP") xs i m)
                        (Seq.slice (here "combine2UP") ys j n)
{-# INLINE_UP combine2UP #-}


-- | Apply a worker function to correponding elements of two arrays.
zipWithUP :: (Unbox a, Unbox b, Unbox c) 
          => (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithUP f xs ys
        = splitJoinD theGang 
                (mapD (What "zipWithUP/map") theGang (Seq.map (uncurry f))) 
                (Seq.zip xs ys)
{-# INLINE_UP zipWithUP #-}


-- | Undirected fold.
--   Note that this function has more constraints on its parameters than the
--   standard fold function from the Haskell Prelude.
--
--   * The worker function must be associative.
--
--   * The provided starting element must be neutral with respect to the worker.
--     For example 0 is neutral wrt (+) and 1 is neutral wrt (*).
--
--   We need these constraints so that we can partition the fold across 
--   several threads. Each thread folds a chunk of the input vector, 
--   then we fold together all the results in the main thread.
--
foldUP  :: (Unbox a, DT a) => (a -> a -> a) -> a -> Vector a -> a
foldUP f !z xs
        = foldD (What "foldUP/f")    theGang f
        $ mapD  (What "foldUP/fold") theGang (Seq.fold f z)
        $ splitD theGang unbalanced xs
{-# INLINE_UP foldUP #-}


-- | Left fold over an array. 
--
--   * If the vector is empty then this returns the provided neural element.
--
--   * The worker function must be associative.
--
--   * The provided starting element must be neutral with respect to the worker,
--     see `foldUP` for discussion.
--
foldlUP :: (DT a, Unbox a) => (a -> a -> a) -> a -> Vector a -> a
foldlUP f z arr 
  | Seq.null arr = z
  | otherwise    = foldl1UP f arr
{-# INLINE_UP foldlUP #-}


-- | Alias for `foldl1UP`
fold1UP :: (DT a, Unbox a) => (a -> a -> a) -> Vector a -> a
fold1UP = foldl1UP
{-# INLINE_UP fold1UP #-}


-- | Left fold over an array, using the first element of the vector as the
--   neural element.
--
--   * If the vector contains no elements then you'll get a bounds-check error.
--
--   * The worker function must be associative.
--
--   * The provided starting element must be neutral with respect to the worker,
--     see `foldUP` for discussion.
--
foldl1UP :: (DT a, Unbox a) => (a -> a -> a) -> Vector a -> a
foldl1UP f arr 
        = (maybe z (f z)
        . foldD  (What "fold1UP/foldD")      theGang combine'
        . mapD   (What "fold1UP/fold1Maybe") theGang (Seq.foldl1Maybe f)
        . splitD theGang unbalanced) arr
        where
                z = Seq.index (here "fold1UP") arr 0
                combine' (Just x) (Just y) = Just (f x y)
                combine' (Just x) Nothing  = Just x
                combine' Nothing  (Just y) = Just y
                combine' Nothing  Nothing  = Nothing
{-# INLINE_UP foldl1UP #-}


-- | Prefix scan. Similar to fold, but produce an array of the intermediate states.
--
--   * The worker function must be associative.
-- 
--   * The provided starting element must be neutral with respect to the worker,
--     see `foldUP` for discussion.
--
scanUP :: (DT a, Unbox a) => (a -> a -> a) -> a -> Vector a -> Vector a
scanUP f z 
 = splitJoinD theGang go
 where  go xs 
         = let (ds,zs)  = unzipD 
                        $ mapD  (What "scanUP/scanRes") theGang (Seq.scanRes f z) xs

               zs'      = fst 
                        $ scanD (What "scanUP/scan") theGang f z zs

           in  zipWithD (What "scanUP/map") theGang (Seq.map . f) zs' ds
{-# INLINE_UP scanUP #-}

