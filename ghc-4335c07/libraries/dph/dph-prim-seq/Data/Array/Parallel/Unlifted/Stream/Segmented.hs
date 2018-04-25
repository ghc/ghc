{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-matches -fno-warn-incomplete-patterns #-}
#include "fusion-phases.h"

-- TODO: 
--  The use of INLINE pragmas in some of these function isn't consistent.
--  for indexedS and combine2ByTagS, there is an INLINE_INNER on the 'next'
--  function, but replicateEachS uses a plain INLINE and fold1SS uses
--  a hard INLINE [0]. Can we make a rule that all top-level stream functions
--  in this module have INLINE_STREAM, and all 'next' functions have
--  INLINE_INNER? If not we should document the reasons for the special cases.
--
--  Fix warnings in this module.

-- | Stream functions not implemented in @Data.Vector@
module Data.Array.Parallel.Unlifted.Stream.Segmented
        ( indexedS
        , replicateEachS
        , replicateEachRS
        , interleaveS
        , combine2ByTagS
        , combineSS
        , enumFromToEachS
        , enumFromStepLenEachS
        , foldSS
        , fold1SS
        , foldValuesR
        , appendSS
        , indicesSS)
where
import Data.Array.Parallel.Base                    (Tag)
import qualified Data.Vector.Fusion.Bundle         as B
import qualified Data.Vector.Fusion.Bundle.Monadic as M
import Data.Vector.Fusion.Bundle.Monadic           (Bundle(..))
import Data.Vector.Fusion.Stream.Monadic           (Stream(..), Step(..))
import Data.Vector.Fusion.Bundle.Size              (Size(..))


-- Indexed --------------------------------------------------------------------
-- | Tag each element of an stream with its index in that stream.
--
-- @
-- indexed [42,93,13]
--  = [(0,42), (1,93), (2,13)]
-- @
indexedS :: B.Bundle v a -> B.Bundle v (Int,a)
{-# INLINE_STREAM indexedS #-}
indexedS Bundle{sElems=Stream next s,sSize=n} =
    M.fromStream (Stream next' (0,s)) n
  where
    {-# INLINE_INNER next' #-}
    next' (i,s) = do
                    r <- next s
                    case r of
                      Yield x s' -> return $ Yield (i,x) (i+1,s')
                      Skip    s' -> return $ Skip        (i,s')
                      Done       -> return Done


-- Replicate ------------------------------------------------------------------
-- | Given a stream of pairs containing a count an an element,
--   replicate element the number of times given by the count.
--
--   The first parameter sets the size hint of the resulting stream.
-- 
-- @
-- replicateEach 10 [(2,10), (5,20), (3,30)]
--   = [10,10,20,20,20,20,20,30,30,30]
-- @
replicateEachS :: Int -> B.Bundle v (Int,a) -> B.Bundle v a
{-# INLINE_STREAM replicateEachS #-}
replicateEachS n Bundle{sElems=Stream next s} =
  M.fromStream (Stream next' (0,Nothing,s)) (Exact n)
  where
    {-# INLINE next' #-}
    next' (0, _, s) =
      do
        r <- next s
        case r of
          Done           -> return Done
          Skip s'        -> return $ Skip (0, Nothing, s')
          Yield (k,x) s' -> return $ Skip (k, Just x,s')
    next' (k,Nothing,s) = return Done   -- NEVER ENTERED (See Note)
    next' (k,Just x,s)  = return $ Yield x (k-1,Just x,s)


-- | Repeat each element in the stream the given number of times.
--
-- @
-- replicateEach 2 [10,20,30]
--  = [10,10,20,20,30,30]
-- @
--
replicateEachRS :: Int -> B.Bundle v a -> B.Bundle v a
{-# INLINE_STREAM replicateEachRS #-}
replicateEachRS !n Bundle{sElems=Stream next s,sSize=sz}
  = M.fromStream (Stream next' (0,Nothing,s)) (sz `multSize` n)
  where
    next' (0,_,s) =
      do
        r <- next s
        case r of
          Done       -> return Done
          Skip    s' -> return $ Skip (0,Nothing,s')
          Yield x s' -> return $ Skip (n,Just x,s')
    next' (i,Nothing,s) = return Done -- NEVER ENTERED (See Note)
    next' (i,Just x,s) = return $ Yield x (i-1,Just x,s)


-- | Multiply a size hint by a scalar.
multSize :: Size -> Int -> Size
multSize (Exact n) k = Exact (n*k)
multSize (Max   n) k = Max   (n*k)
multSize Unknown   _ = Unknown


-- Interleave -----------------------------------------------------------------
-- | Interleave the elements of two streams. We alternate between the first
--   and second streams, stopping when we can't find a matching element.
--
-- @
-- interleave [2,3,4] [10,20,30] = [2,10,3,20,4,30]
-- interleave [2,3]   [10,20,30] = [2,10,3,20]
-- interleave [2,3,4] [10,20]    = [2,10,3,20,4]
-- @
--
interleaveS :: B.Bundle v a -> B.Bundle v a -> B.Bundle v a
{-# INLINE_STREAM interleaveS #-}
interleaveS (Bundle{sElems=Stream next1 s1,sSize=n1})
            (Bundle{sElems=Stream next2 s2,sSize=n2})
  = M.fromStream (Stream next (False,s1,s2)) (n1+n2)
  where
    {-# INLINE next #-}
    next (False,s1,s2) =
      do
        r <- next1 s1
        case r of
          Yield x s1' -> return $ Yield x (True ,s1',s2)
          Skip    s1' -> return $ Skip    (False,s1',s2)
          Done        -> return Done

    next (True,s1,s2) =
      do
        r <- next2 s2
        case r of
          Yield x s2' -> return $ Yield x (False,s1,s2')
          Skip    s2' -> return $ Skip    (True ,s1,s2')
          Done        -> return Done -- NEVER ENTERED (See Note)


-- Combine --------------------------------------------------------------------
-- | Combine two streams, using a tag stream to tell us which of the data
--   streams to take the next element from.
--
--   If there are insufficient elements in the data strams for the provided
--   tag stream then `error`.
--  
-- @
-- combine2ByTag [0,1,1,0,0,1] [1,2,3] [4,5,6]
--  = [1,4,5,2,3,6]
-- @
--
combine2ByTagS :: B.Bundle v Tag -> B.Bundle v a -> B.Bundle v a -> B.Bundle v a
{-# INLINE_STREAM combine2ByTagS #-}
combine2ByTagS (Bundle{sElems=Stream next_tag s,sSize=m})
               (Bundle{sElems=Stream next0 s0})
               (Bundle{sElems=Stream next1 s1})
  = M.fromStream (Stream next (Nothing,s,s0,s1)) m
  where
    {-# INLINE_INNER next #-}
    next (Nothing,s,s0,s1)
      = do
          r <- next_tag s
          case r of
            Done       -> return Done
            Skip    s' -> return $ Skip (Nothing,s',s0,s1)
            Yield t s' -> return $ Skip (Just t, s',s0,s1)

    next (Just 0,s,s0,s1)
      = do
          r <- next0 s0
          case r of
            Done        -> error "combine2ByTagS: stream 1 too short"
            Skip    s0' -> return $ Skip    (Just 0, s,s0',s1)
            Yield x s0' -> return $ Yield x (Nothing,s,s0',s1)

    next (Just t,s,s0,s1)
      = do
          r <- next1 s1
          case r of
            Done        -> error "combine2ByTagS: stream 2 too short"
            Skip    s1' -> return $ Skip    (Just t, s,s0,s1')
            Yield x s1' -> return $ Yield x (Nothing,s,s0,s1')


-- | Segmented Stream combine. Like `combine2ByTagS`, except that the tags select
--   entire segments of each data stream, instead of selecting one element at a time.
--
-- @
-- combineSS [True, True, False, True, False, False]
--           [2,1,3] [10,20,30,40,50,60]
--           [1,2,3] [11,22,33,44,55,66]
--  = [10,20,30,11,40,50,60,22,33,44,55,66]
-- @
--
--   This says take two elements from the first stream, then another one element 
--   from the first stream, then one element from the second stream, then three
--   elements from the first stream...
--
combineSS 
        :: B.Bundle v Bool      -- ^ tag values
        -> B.Bundle v Int       -- ^ segment lengths for first data stream
        -> B.Bundle v a         -- ^ first data stream
        -> B.Bundle v Int       -- ^ segment lengths for second data stream
        -> B.Bundle v a         -- ^ second data stream
        -> B.Bundle v a

{-# INLINE_STREAM combineSS #-}
combineSS (Bundle{sElems=Stream nextf sf})
          (Bundle{sElems=Stream nexts1 ss1}) (Bundle{sElems=Stream nextv1 vs1,sSize=nv1})
          (Bundle{sElems=Stream nexts2 ss2}) (Bundle{sElems=Stream nextv2 vs2,sSize=nv2})
  = M.fromStream (Stream next (Nothing,True,sf,ss1,vs1,ss2,vs2))
                 (nv1+nv2)
  where
    {-# INLINE next #-}
    next (Nothing,f,sf,ss1,vs1,ss2,vs2) =
      do
        r <- nextf sf
        case r of
          Done        -> return Done
          Skip sf'    -> return $ Skip (Nothing,f,sf',ss1,vs1,ss2,vs2) 
          Yield c sf'
            | c ->
              do
                r <- nexts1 ss1
                case r of
                  Done         -> return Done
                  Skip ss1'    -> return $ Skip (Nothing,f,sf,ss1',vs1,ss2,vs2) 
                  Yield n ss1' -> return $ Skip (Just n,c,sf',ss1',vs1,ss2,vs2) 

            | otherwise ->
              do
                r <- nexts2 ss2
                case r of
                  Done         -> return Done
                  Skip ss2'    -> return $ Skip (Nothing,f,sf,ss1,vs1,ss2',vs2) 
                  Yield n ss2' -> return $ Skip (Just n,c,sf',ss1,vs1,ss2',vs2)

    next (Just 0,_,sf,ss1,vs1,ss2,vs2) =
         return $ Skip (Nothing,True,sf,ss1,vs1,ss2,vs2)

    next (Just n,True,sf,ss1,vs1,ss2,vs2) =
      do
        r <- nextv1 vs1
        case r of
          Done         -> return Done
          Skip vs1'    -> return $ Skip (Just n,True,sf,ss1,vs1',ss2,vs2) 
          Yield x vs1' -> return $ Yield x (Just (n-1),True,sf,ss1,vs1',ss2,vs2)

    next (Just n,False,sf,ss1,vs1,ss2,vs2) =
      do
        r <- nextv2 vs2
        case r of
          Done         -> return Done
          Skip vs2'    -> return $ Skip (Just n,False,sf,ss1,vs1,ss2,vs2') 
          Yield x vs2' -> return $ Yield x (Just (n-1),False,sf,ss1,vs1,ss2,vs2')


-- Enum -----------------------------------------------------------------------
-- | Create a stream of integer ranges. The pairs in the input stream
--   give the first and last value of each range.
--
--   The first parameter gives the size hint for the resulting stream.
-- 
-- @
-- enumFromToEach 11 [(2,5), (10,16), (20,22)]
--  = [2,3,4,5,10,11,12,13,14,15,16,20,21,22]
-- @
--
enumFromToEachS :: Int -> B.Bundle v (Int,Int) -> B.Bundle v Int
{-# INLINE_STREAM enumFromToEachS #-}
enumFromToEachS n (Bundle{sElems=Stream next s})
  = M.fromStream (Stream next' (Nothing,s)) (Exact n)
  where
    {-# INLINE_INNER next' #-}
    next' (Nothing,s)
      = do
          r <- next s
          case r of
            Yield (k,m) s' -> return $ Skip (Just (k,m),s')
            Skip        s' -> return $ Skip (Nothing,   s')
            Done           -> return Done

    next' (Just (k,m),s)
      | k > m     = return $ Skip    (Nothing,     s)
      | otherwise = return $ Yield k (Just (k+1,m),s)


-- | Create a stream of integer ranges. The triples in the input stream
--   give the first value, increment, length of each range.
--
--   The first parameter gives the size hint for the resulting stream.
--
-- @
-- enumFromStepLenEach [(1,1,5), (10,2,4), (20,3,5)]
--  = [1,2,3,4,5,10,12,14,16,20,23,26,29,32]
-- @
--               
enumFromStepLenEachS :: Int -> B.Bundle v (Int,Int,Int) -> B.Bundle v Int 
{-# INLINE_STREAM enumFromStepLenEachS #-}
enumFromStepLenEachS len (Bundle{sElems=Stream next s})
  = M.fromStream (Stream next' (Nothing,s)) (Exact len)
  where
    {-# INLINE_INNER next' #-}
    next' (Nothing,s) 
      = do
          r <- next s
          case r of
            Yield (from,step,len) s' -> return $ Skip (Just (from,step,len),s')
            Skip                  s' -> return $ Skip (Nothing,s')
            Done                     -> return Done

    next' (Just (from,step,0),s) = return $ Skip (Nothing,s)
    next' (Just (from,step,n),s)
      = return $ Yield from (Just (from+step,step,n-1),s)


-- Fold -----------------------------------------------------------------------
-- | Segmented Stream fold. Take segments from the given stream and fold each
--   using the supplied function and initial element. 
--
-- @
-- foldSS (+) 0 [2, 3, 2] [10, 20, 30, 40, 50, 60, 70]
--  = [30,120,130]
-- @
--
foldSS  :: (a -> b -> a)        -- ^ function to perform the fold
        -> a                    -- ^ initial element of each fold
        -> B.Bundle v Int       -- ^ stream of segment lengths
        -> B.Bundle v b         -- ^ stream of input data
        -> B.Bundle v a         -- ^ stream of fold results
        
{-# INLINE_STREAM foldSS #-}
foldSS f z (Bundle{sElems=Stream nexts ss,sSize=sz}) (Bundle{sElems=Stream nextv vs}) =
  M.fromStream (Stream next (Nothing,z,ss,vs)) sz
  where
    {-# INLINE next #-}
    next (Nothing,x,ss,vs) =
      do
        r <- nexts ss
        case r of
          Done        -> return Done
          Skip    ss' -> return $ Skip (Nothing,x, ss', vs)
          Yield n ss' -> return $ Skip (Just n, z, ss', vs)

    next (Just 0,x,ss,vs) =
      return $ Yield x (Nothing,z,ss,vs)
    next (Just n,x,ss,vs) =
      do
        r <- nextv vs
        case r of
          Done        -> return Done -- NEVER ENTERED (See Note)
          Skip    vs' -> return $ Skip (Just n,x,ss,vs')
          Yield y vs' -> let r = f x y
                         in r `seq` return (Skip (Just (n-1), r, ss, vs'))


-- | Like `foldSS`, but use the first member of each chunk as the initial
--   element for the fold.
fold1SS :: (a -> a -> a) -> B.Bundle v Int -> B.Bundle v a -> B.Bundle v a
{-# INLINE_STREAM fold1SS #-}
fold1SS f (Bundle{sElems=Stream nexts ss,sSize=sz}) (Bundle{sElems=Stream nextv vs}) =
  M.fromStream (Stream next (Nothing,Nothing,ss,vs)) sz
  where
    {-# INLINE [0] next #-}
    next (Nothing,Nothing,ss,vs) =
      do
        r <- nexts ss
        case r of
          Done        -> return Done
          Skip    ss' -> return $ Skip (Nothing,Nothing,ss',vs)
          Yield n ss' -> return $ Skip (Just n ,Nothing,ss',vs)

    next (Just !n,Nothing,ss,vs) =
      do
        r <- nextv vs
        case r of
          Done        -> return Done -- NEVER ENTERED (See Note)
          Skip    vs' -> return $ Skip (Just n,    Nothing,ss,vs')
          Yield x vs' -> return $ Skip (Just (n-1),Just x, ss,vs')

    next (Just 0,Just x,ss,vs) =
      return $ Yield x (Nothing,Nothing,ss,vs)

    next (Just n,Just x,ss,vs) =
      do
        r <- nextv vs
        case r of
          Done        -> return Done  -- NEVER ENTERED (See Note)
          Skip    vs' -> return $ Skip (Just n    ,Just x      ,ss,vs')
          Yield y vs' -> let r = f x y
                         in r `seq` return (Skip (Just (n-1),Just r,ss,vs'))


-- | Segmented Stream fold, with a fixed segment length.
-- 
--   Like `foldSS` but use a fixed length for each segment.
--
foldValuesR 
        :: (a -> b -> a)        -- ^ function to perform the fold
        -> a                    -- ^ initial element for fold
        -> Int                  -- ^ length of each segment
        -> B.Bundle v b         -- ^ data stream
        -> B.Bundle v a


{-# INLINE_STREAM foldValuesR #-}
foldValuesR f z segSize (Bundle{sElems=Stream nextv vs,sSize=nv}) =
  M.fromStream (Stream next (segSize,z,vs)) (nv `divSize` segSize)
  where
    {-# INLINE next #-}  
    next (0,x,vs) = return $ Yield x (segSize,z,vs)

    next (n,x,vs) =
      do
        r <- nextv vs
        case r of
          Done        -> return Done
          Skip    vs' -> return $ Skip (n,x,vs')
          Yield y vs' -> let r = f x y
                         in r `seq` return (Skip ((n-1),r,vs'))


-- | Divide a size hint by a scalar.
divSize :: Size -> Int -> Size
divSize (Exact n) k = Exact (n `div` k)
divSize (Max   n) k = Max   (n `div` k)
divSize Unknown   _ = Unknown


-- Append ---------------------------------------------------------------------
-- | Segmented Strem append. Append corresponding segments from each stream.
--
-- @
-- appendSS [2, 1, 3] [10, 20, 30, 40, 50, 60]
--          [1, 3, 2] [11, 22, 33, 44, 55, 66]
--  = [10,20,11,30,22,33,44,40,50,60,55,66]
-- @
--
appendSS
        :: B.Bundle v Int         -- ^ segment lengths for first data stream
        -> B.Bundle v a           -- ^ first data stream
        -> B.Bundle v Int         -- ^ segment lengths for second data stream
        -> B.Bundle v a           -- ^ second data stream
        -> B.Bundle v a

{-# INLINE_STREAM appendSS #-}
appendSS (Bundle{sElems=Stream nexts1 ss1,sSize=ns1}) (Bundle{sElems=Stream nextv1 sv1,sSize=nv1})
         (Bundle{sElems=Stream nexts2 ss2,sSize=ns2}) (Bundle{sElems=Stream nextv2 sv2,sSize=nv2})
  = M.fromStream (Stream next (True,Nothing,ss1,sv1,ss2,sv2)) (nv1 + nv2)
  where
    {-# INLINE next #-}
    next (True,Nothing,ss1,sv1,ss2,sv2) =
      do
        r <- nexts1 ss1
        case r of
          Done         -> return $ Done
          Skip    ss1' -> return $ Skip (True,Nothing,ss1',sv1,ss2,sv2)
          Yield n ss1' -> return $ Skip (True,Just n ,ss1',sv1,ss2,sv2)

    next (True,Just 0,ss1,sv1,ss2,sv2)
      = return $ Skip (False,Nothing,ss1,sv1,ss2,sv2)

    next (True,Just n,ss1,sv1,ss2,sv2) =
      do
        r <- nextv1 sv1
        case r of
          Done         -> return Done  -- NEVER ENTERED (See Note)
          Skip    sv1' -> return $ Skip (True,Just n,ss1,sv1',ss2,sv2)
          Yield x sv1' -> return $ Yield x (True,Just (n-1),ss1,sv1',ss2,sv2)

    next (False,Nothing,ss1,sv1,ss2,sv2) =
      do
        r <- nexts2 ss2
        case r of
          Done         -> return Done  -- NEVER ENTERED (See Note)
          Skip    ss2' -> return $ Skip (False,Nothing,ss1,sv1,ss2',sv2)
          Yield n ss2' -> return $ Skip (False,Just n,ss1,sv1,ss2',sv2)

    next (False,Just 0,ss1,sv1,ss2,sv2)
      = return $ Skip (True,Nothing,ss1,sv1,ss2,sv2)

    next (False,Just n,ss1,sv1,ss2,sv2) =
      do
        r <- nextv2 sv2
        case r of
          Done         -> return Done  -- NEVER ENTERED (See Note)
          Skip    sv2' -> return $ Skip (False,Just n,ss1,sv1,ss2,sv2')
          Yield x sv2' -> return $ Yield x (False,Just (n-1),ss1,sv1,ss2,sv2')


-- Indices --------------------------------------------------------------------
-- | Segmented Stream indices.
-- 
-- @
-- indicesSS 15 4 [3, 5, 7]
--  = [4,5,6,0,1,2,3,4,0,1,2,3,4,5,6]
-- @
--
-- Note that we can set the starting value of the first segment independently
-- via the second argument of indicesSS. We use this when distributing arrays
-- across worker threads, as a thread's chunk may not start exactly at a 
-- segment boundary, so the index of a thread's first data element may not be
-- zero.
--
indicesSS 
        :: Int
        -> Int
        -> B.Bundle v Int
        -> B.Bundle v Int

{-# INLINE_STREAM indicesSS #-}
indicesSS n i (Bundle{sElems=Stream next s}) =
  M.fromStream (Stream next' (i,Nothing,s)) (Exact n)
  where
    {-# INLINE next' #-}
    next' (i,Nothing,s) =
      do
        r <- next s
        case r of
          Done       -> return Done
          Skip    s' -> return $ Skip (i,Nothing,s')
          Yield k s' -> return $ Skip (i,Just k,s')

    next' (i,Just k,s)
      | k > 0     = return $ Yield i (i+1,Just (k-1),s)
      | otherwise = return $ Skip    (0  ,Nothing   ,s)



-- Note: [NEVER ENTERED]
-- ~~~~~~~~~~~~~~~~~~~~~
--  Cases marked NEVER ENTERED should be unreachable, assuming there are no 
--  bugs elsewhere in the library. We used to throw an error when these
--  branches were entered, but this was confusing the simplifier. It would be 
--  better if we could put the errors back, but we'll need to check that 
--  performance does not regress when we do so.
