{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Standard combinators for distributed types.
module Data.Array.Parallel.Unlifted.Distributed.Combinators 
        ( W.What (..)
        , imapD, mapD
        , zipD, unzipD
        , fstD, sndD
        , zipWithD, izipWithD
        , foldD
        , scanD
        , mapAccumLD)
where
import Data.Array.Parallel.Base ( ST, runST)
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import Data.Array.Parallel.Unlifted.Distributed.Data.Tuple
import Data.Array.Parallel.Unlifted.Distributed.Data.Maybe      ()
import qualified Data.Array.Parallel.Unlifted.Distributed.What as W


here s = "Data.Array.Parallel.Unlifted.Distributed.Combinators." ++ s


-- Mapping --------------------------------------------------------------------
--
-- Fusing maps
-- ~~~~~~~~~~~
--  The staging here is important. 
--  Our rewrite rules only operate on the imapD form, so fusion between the worker
--  functions of consecutive maps takes place before phase [0]. 
--
--  At phase [0] we then inline imapD which introduces the call to imapD' which
--  uses the gang to evaluate its (now fused) worker.
--

-- | Map a function to every instance of a distributed value.
--
--   This applies the function to every thread, but not every value held
--   by the thread. If you want that then use something like:
-- 
--   @mapD theGang (V.map (+ 1)) :: Dist (Vector Int) -> Dist (Vector Int)@
--
mapD    :: (DT a, DT b) 
        => W.What         -- ^ What is the worker function doing.
        -> Gang 
        -> (a -> b) 
        -> Dist a 
        -> Dist b

mapD wFn gang
        = imapD wFn gang . const
{-# INLINE mapD #-}
--  INLINE because this is just a convenience wrapper for imapD.
--  None of our rewrite rules are particular to mapD.


-- | Map a function across all elements of a distributed value.
--   The worker function also gets the current thread index.
--   As opposed to `imapD'` this version also deepSeqs each element before
--   passing it to the function.
imapD   :: (DT a, DT b) 
        => W.What         -- ^ What is the worker function doing.
        -> Gang 
        -> (Int -> a -> b) 
        -> Dist a -> Dist b
imapD wFn gang f d 
        = imapD' wFn gang (\i x -> x `deepSeqD` f i x) d
{-# INLINE [0] imapD #-}
--  INLINE [0] because we want to wait until phase [0] before introducing
--  the call to imapD'. Our rewrite rules operate directly on the imapD
--  formp, so once imapD is inlined no more fusion can take place.


{-# RULES

"imapD/generateD" 
  forall wMap wGen gang f g
  . imapD wMap gang f (generateD wGen gang g) 
  = generateD (W.WFMapGen wMap wGen) gang (\i -> f i (g i))

"imapD/generateD_cheap" 
  forall wMap wGen gang f g
  . imapD wMap gang f (generateD_cheap wGen gang g) 
  = generateD (W.WFMapGen wMap wGen) gang (\i -> f i (g i))

"imapD/imapD" 
  forall wMap1 wMap2 gang f g d
  . imapD wMap1 gang f (imapD wMap2 gang g d) 
  = imapD (W.WFMapMap wMap1 wMap2) gang (\i x -> f i (g i x)) d

  #-}


-- Zipping --------------------------------------------------------------------
-- | Combine two distributed values with the given function.
zipWithD :: (DT a, DT b, DT c)
        => W.What                 -- ^ What is the worker function doing.
        -> Gang 
        -> (a -> b -> c) 
        -> Dist a -> Dist b -> Dist c

zipWithD what g f dx dy 
        = mapD what g (uncurry f) (zipD dx dy)
{-# INLINE zipWithD #-}


-- | Combine two distributed values with the given function.
--   The worker function also gets the index of the current thread.
izipWithD :: (DT a, DT b, DT c)
          => W.What               -- ^ What is the worker function doing.
          -> Gang 
          -> (Int -> a -> b -> c) 
          -> Dist a -> Dist b -> Dist c

izipWithD what g f dx dy 
        = imapD what g (\i -> uncurry (f i)) (zipD dx dy)
{-# INLINE izipWithD #-}


{-# RULES
"zipD/imapD[1]" 
  forall gang f xs ys what
  . zipD (imapD what gang f xs) ys
  = imapD what gang (\i (x,y) -> (f i x, y)) (zipD xs ys)

"zipD/imapD[2]" 
  forall gang f xs ys what
  . zipD xs (imapD what gang f ys)
  = imapD what gang (\i (x,y) -> (x, f i y)) (zipD xs ys)

"zipD/generateD[1]" 
  forall gang f xs what
  . zipD (generateD what gang f) xs
  = imapD what gang (\i x -> (f i, x)) xs

"zipD/generateD[2]" 
  forall gang f xs what
  . zipD xs (generateD what gang f)
  = imapD what gang (\i x -> (x, f i)) xs

  #-}


-- MapAccumL ------------------------------------------------------------------
-- | Combination of map and fold.
mapAccumLD 
        :: forall a b acc. (DT a, DT b)
        => Gang
        -> (acc -> a      -> (acc, b))
        ->  acc -> Dist a -> (acc, Dist b)

mapAccumLD g f acc !d
  = checkGangD (here "mapAccumLD") g d 
  $ runST (do
        md   <- newMD g
        acc' <- go md 0 acc
        d'   <- unsafeFreezeMD md
        return (acc',d'))
  where
    !n = gangSize g
    
    go :: MDist b s -> Int -> acc -> ST s acc
    go md i acc'
        | i == n    = return acc'
        | otherwise
        = case f acc' (indexD (here "mapAccumLD") d i) of
                (acc'',b) -> do
                      writeMD md i b
                      go md (i+1) acc''
{-# INLINE_DIST mapAccumLD #-}
                                
