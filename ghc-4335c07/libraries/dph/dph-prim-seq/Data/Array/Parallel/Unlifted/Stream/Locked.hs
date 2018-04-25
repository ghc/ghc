{-# LANGUAGE CPP, NoMonomorphismRestriction #-}
#include "fusion-phases.h"
-- | Locked streamers and zippers.
--
--   The streams are 'locked together', meaning they have the same length and
--   cannot yield 'Skip' states.
-- 
--   These functions are used for processing data read directly from vectors
--   where we know the vectors all have the same length.
--   
module Data.Array.Parallel.Unlifted.Stream.Locked
        ( stream2,      lockedZip2S
        , stream3,      lockedZip3S
        , stream4,      lockedZip4S
        , stream5,      lockedZip5S
        , stream6,      lockedZip6S
        , stream7,      lockedZip7S
        , stream8,      lockedZip8S)
where
import Data.Array.Parallel.Unlifted.Stream.Swallow
import Data.Vector.Generic               as G
import Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Bundle.Monadic as B
import Data.Vector.Fusion.Bundle.Size    as S


-------------------------------------------
-- | Stream two vectors of the same length.
--   The fact that they are the same length means the generated code only 
--   needs to maintain one loop counter for all streams.
--
--   Trying to stream vectors of differing lengths is undefined.
--
stream2 :: (Monad m, Vector v a, Vector v b)
        => v a -> v b
        -> Bundle m v (a, b)

stream2 aa bb
 = lockedZip2S (G.length aa) (swallow aa) (swallow bb)
{-# INLINE_STREAM stream2 #-}


---------------------------------------------
-- | Stream three vectors of the same length.
stream3 :: (Monad m, Vector v a, Vector v b, Vector v c)
        => v a -> v b -> v c 
        -> Bundle m v (a, b, c)

stream3 aa bb cc
 = lockedZip3S (G.length aa) (swallow aa) (swallow bb) (swallow cc)
{-# INLINE_STREAM stream3 #-}


-- When we see that one of the vectors is being created G.new then push down a
-- 'swallow' wrapper to signal that the consumer (lockedZip2S) knows how many
-- elements to demand. This lets us generate better code on the producer side
-- as it doesn't need to track how many elements still need to be generated.

{-# RULES "stream3/new_1"
    forall as bs cs
    . stream3 (G.new as) bs cs
    = B.map (\((b, c), a) -> (a, b, c))
    $ lockedZip2S (G.length bs) (swallow2 bs cs) (swallow (G.new as))
  #-}

{-# RULES "stream3/new_2"
    forall as bs cs
    . stream3 as (G.new bs) cs
    = B.map (\((a, c), b) -> (a, b, c))
    $ lockedZip2S (G.length as) (swallow2 as cs) (swallow (G.new bs))
  #-}

{-# RULES "stream3/new_3"
    forall as bs cs
    . stream3 as bs (G.new cs)
    = B.map (\((a, b), c) -> (a, b, c))
    $ lockedZip2S (G.length as) (swallow2 as bs) (swallow (G.new cs))
  #-}


---------------------------------------------
-- | Stream four vectors of the same length.
stream4 :: (Monad m, Vector v a, Vector v b, Vector v c, Vector v d)
        => v a -> v b -> v c -> v d
        -> Bundle m v (a, b, c, d)

stream4 aa bb cc dd
 = lockedZip4S (G.length aa) 
        (swallow aa) (swallow bb) (swallow cc) (swallow dd)
{-# INLINE_STREAM stream4 #-}

{-# RULES "stream4/new_1"
    forall as bs cs ds
    . stream4 (G.new as) bs cs ds
    = B.map (\((b, c, d), a) -> (a, b, c, d))
    $ lockedZip2S (G.length bs) (swallow3 bs cs ds) (swallow (G.new as))
  #-}

{-# RULES "stream4/new_2"
    forall as bs cs ds
    . stream4 as (G.new bs) cs ds
    = B.map (\((a, c, d), b) -> (a, b, c, d))
    $ lockedZip2S (G.length as) (swallow3 as cs ds) (swallow (G.new bs))
  #-}

{-# RULES "stream4/new_3"
    forall as bs cs ds
    . stream4 as bs (G.new cs) ds
    = B.map (\((a, b, d), c) -> (a, b, c, d))
    $ lockedZip2S (G.length as) (swallow3 as bs ds) (swallow (G.new cs))
  #-}

{-# RULES "stream4/new_4"
    forall as bs cs ds
    . stream4 as bs cs (G.new ds)
    = B.map (\((a, b, c), d) -> (a, b, c, d))
    $ lockedZip2S (G.length as) (swallow3 as bs cs) (swallow (G.new ds))
  #-}


---------------------------------------------
-- | Stream five vectors of the same length.
stream5 :: (Monad m, Vector v a, Vector v b, Vector v c, Vector v d, Vector v e)
        => v a -> v b -> v c -> v d -> v e
        -> Bundle m v (a, b, c, d, e)

stream5 aa bb cc dd ee
 = lockedZip5S (G.length aa) 
        (swallow aa) (swallow bb) (swallow cc) (swallow dd)
        (swallow ee)
{-# INLINE_STREAM stream5 #-}

{-# RULES "stream5/new_1"
    forall as bs cs ds es
    . stream5 (G.new as) bs cs ds es
    = B.map (\((b, c, d, e), a) -> (a, b, c, d, e))
    $ lockedZip2S (G.length bs) (swallow4 bs cs ds es) (swallow (G.new as))
  #-}

{-# RULES "stream5/new_2"
    forall as bs cs ds es
    . stream5 as (G.new bs) cs ds es
    = B.map (\((a, c, d, e), b) -> (a, b, c, d, e))
    $ lockedZip2S (G.length as) (swallow4 as cs ds es) (swallow (G.new bs))
  #-}

{-# RULES "stream5/new_3"
    forall as bs cs ds es
    . stream5 as bs (G.new cs) ds es
    = B.map (\((a, b, d, e), c) -> (a, b, c, d, e))
    $ lockedZip2S (G.length as) (swallow4 as bs ds es) (swallow (G.new cs))
  #-}

{-# RULES "stream5/new_4"
    forall as bs cs ds es
    . stream5 as bs cs (G.new ds) es
    = B.map (\((a, b, c, e), d) -> (a, b, c, d, e))
    $ lockedZip2S (G.length as) (swallow4 as bs cs es) (swallow (G.new ds))
  #-}

{-# RULES "stream5/new_5"
    forall as bs cs ds es
    . stream5 as bs cs ds (G.new es)
    = B.map (\((a, b, c, d), e) -> (a, b, c, d, e))
    $ lockedZip2S (G.length as) (swallow4 as bs cs ds) (swallow (G.new es))
  #-}


---------------------------------------------
-- | Stream six vectors of the same length.
stream6 :: (Monad m, Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f)
        => v a -> v b -> v c -> v d -> v e -> v f
        -> Bundle m v (a, b, c, d, e, f)

stream6 aa bb cc dd ee ff
 = lockedZip6S (G.length aa) 
        (swallow aa) (swallow bb) (swallow cc) (swallow dd)
        (swallow ee) (swallow ff)
{-# INLINE_STREAM stream6 #-}

{-# RULES "stream6/new_1"
    forall as bs cs ds es fs
    . stream6 (G.new as) bs cs ds es fs
    = B.map (\((b, c, d, e, f), a) -> (a, b, c, d, e, f))
    $ lockedZip2S (G.length bs) (swallow5 bs cs ds es fs) (swallow (G.new as))
  #-}

{-# RULES "stream6/new_2"
    forall as bs cs ds es fs
    . stream6 as (G.new bs) cs ds es fs
    = B.map (\((a, c, d, e, f), b) -> (a, b, c, d, e, f))
    $ lockedZip2S (G.length as) (swallow5 as cs ds es fs) (swallow (G.new bs))
  #-}

{-# RULES "stream6/new_3"
    forall as bs cs ds es fs
    . stream6 as bs (G.new cs) ds es fs
    = B.map (\((a, b, d, e, f), c) -> (a, b, c, d, e, f))
    $ lockedZip2S (G.length as) (swallow5 as bs ds es fs) (swallow (G.new cs))
  #-}

{-# RULES "stream6/new_4"
    forall as bs cs ds es fs
    . stream6 as bs cs (G.new ds) es fs
    = B.map (\((a, b, c, e, f), d) -> (a, b, c, d, e, f))
    $ lockedZip2S (G.length as) (swallow5 as bs cs es fs) (swallow (G.new ds))
  #-}

{-# RULES "stream6/new_5"
    forall as bs cs ds es fs
    . stream6 as bs cs ds (G.new es) fs
    = B.map (\((a, b, c, d, f), e) -> (a, b, c, d, e, f))
    $ lockedZip2S (G.length as) (swallow5 as bs cs ds fs) (swallow (G.new es))
  #-}

{-# RULES "stream6/new_6"
    forall as bs cs ds es fs
    . stream6 as bs cs ds es (G.new fs)
    = B.map (\((a, b, c, d, e), f) -> (a, b, c, d, e, f))
    $ lockedZip2S (G.length as) (swallow5 as bs cs ds es) (swallow (G.new fs))
  #-}


---------------------------------------------
-- | Stream seven vectors of the same length.
stream7 :: (Monad m, Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v g)
        => v a -> v b -> v c -> v d -> v e -> v f -> v g
        -> Bundle m v (a, b, c, d, e, f, g)

stream7 aa bb cc dd ee ff gg
 = lockedZip7S (G.length aa) 
        (swallow aa) (swallow bb) (swallow cc) (swallow dd)
        (swallow ee) (swallow ff) (swallow gg)
{-# INLINE_STREAM stream7 #-}

{-# RULES "stream7/new_1"
    forall as bs cs ds es fs gs
    . stream7 (G.new as) bs cs ds es fs gs
    = B.map (\((b, c, d, e, f, g), a) -> (a, b, c, d, e, f, g))
    $ lockedZip2S (G.length bs) (swallow6 bs cs ds es fs gs) (swallow (G.new as))
  #-}

{-# RULES "stream7/new_2"
    forall as bs cs ds es fs gs
    . stream7 as (G.new bs) cs ds es fs gs
    = B.map (\((a, c, d, e, f, g), b) -> (a, b, c, d, e, f, g))
    $ lockedZip2S (G.length as) (swallow6 as cs ds es fs gs) (swallow (G.new bs))
  #-}

{-# RULES "stream7/new_3"
    forall as bs cs ds es fs gs
    . stream7 as bs (G.new cs) ds es fs gs
    = B.map (\((a, b, d, e, f, g), c) -> (a, b, c, d, e, f, g))
    $ lockedZip2S (G.length as) (swallow6 as bs ds es fs gs) (swallow (G.new cs))
  #-}

{-# RULES "stream7/new_4"
    forall as bs cs ds es fs gs
    . stream7 as bs cs (G.new ds) es fs gs
    = B.map (\((a, b, c, e, f, g), d) -> (a, b, c, d, e, f, g))
    $ lockedZip2S (G.length as) (swallow6 as bs cs es fs gs) (swallow (G.new ds))
  #-}

{-# RULES "stream7/new_5"
    forall as bs cs ds es fs gs
    . stream7 as bs cs ds (G.new es) fs gs
    = B.map (\((a, b, c, d, f, g), e) -> (a, b, c, d, e, f, g))
    $ lockedZip2S (G.length as) (swallow6 as bs cs ds fs gs) (swallow (G.new es))
  #-}

{-# RULES "stream7/new_6"
    forall as bs cs ds es fs gs
    . stream7 as bs cs ds es (G.new fs) gs
    = B.map (\((a, b, c, d, e, g), f) -> (a, b, c, d, e, f, g))
    $ lockedZip2S (G.length as) (swallow6 as bs cs ds es gs) (swallow (G.new fs))
  #-}

{-# RULES "stream7/new_7"
    forall as bs cs ds es fs gs
    . stream7 as bs cs ds es fs (G.new gs)
    = B.map (\((a, b, c, d, e, f), g) -> (a, b, c, d, e, f, g))
    $ lockedZip2S (G.length as) (swallow6 as bs cs ds es fs) (swallow (G.new gs))
  #-}


---------------------------------------------
-- | Stream seven vectors of the same length.
stream8 :: (Monad m, Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v g, Vector v h)
        => v a -> v b -> v c -> v d -> v e -> v f -> v g -> v h
        -> Bundle m v (a, b, c, d, e, f, g, h)

stream8 aa bb cc dd ee ff gg hh
 = lockedZip8S (G.length aa) 
        (swallow aa) (swallow bb) (swallow cc) (swallow dd)
        (swallow ee) (swallow ff) (swallow gg) (swallow hh)
{-# INLINE_STREAM stream8 #-}

{-# RULES "stream8/new_1"
    forall as bs cs ds es fs gs hs
    . stream8 (G.new as) bs cs ds es fs gs hs
    = B.map (\((b, c, d, e, f, g, h), a) -> (a, b, c, d, e, f, g, h))
    $ lockedZip2S (G.length bs) (swallow7 bs cs ds es fs gs hs) (swallow (G.new as))
  #-}

{-# RULES "stream8/new_2"
    forall as bs cs ds es fs gs hs
    . stream8 as (G.new bs) cs ds es fs gs hs
    = B.map (\((a, c, d, e, f, g, h), b) -> (a, b, c, d, e, f, g, h))
    $ lockedZip2S (G.length as) (swallow7 as cs ds es fs gs hs) (swallow (G.new bs))
  #-}

{-# RULES "stream8/new_3"
    forall as bs cs ds es fs gs hs
    . stream8 as bs (G.new cs) ds es fs gs hs
    = B.map (\((a, b, d, e, f, g, h), c) -> (a, b, c, d, e, f, g, h))
    $ lockedZip2S (G.length as) (swallow7 as bs ds es fs gs hs) (swallow (G.new cs))
  #-}

{-# RULES "stream8/new_4"
    forall as bs cs ds es fs gs hs
    . stream8 as bs cs (G.new ds) es fs gs hs
    = B.map (\((a, b, c, e, f, g, h), d) -> (a, b, c, d, e, f, g, h))
    $ lockedZip2S (G.length as) (swallow7 as bs cs es fs gs hs) (swallow (G.new ds))
  #-}

{-# RULES "stream8/new_5"
    forall as bs cs ds es fs gs hs
    . stream8 as bs cs ds (G.new es) fs gs hs
    = B.map (\((a, b, c, d, f, g, h), e) -> (a, b, c, d, e, f, g, h))
    $ lockedZip2S (G.length as) (swallow7 as bs cs ds fs gs hs) (swallow (G.new es))
  #-}

{-# RULES "stream8/new_6"
    forall as bs cs ds es fs gs hs
    . stream8 as bs cs ds es (G.new fs) gs hs
    = B.map (\((a, b, c, d, e, g, h), f) -> (a, b, c, d, e, f, g, h))
    $ lockedZip2S (G.length as) (swallow7 as bs cs ds es gs hs) (swallow (G.new fs))
  #-}

{-# RULES "stream8/new_7"
    forall as bs cs ds es fs gs hs
    . stream8 as bs cs ds es fs (G.new gs) hs
    = B.map (\((a, b, c, d, e, f, h), g) -> (a, b, c, d, e, f, g, h))
    $ lockedZip2S (G.length as) (swallow7 as bs cs ds es fs hs) (swallow (G.new gs))
  #-}

{-# RULES "stream8/new_8"
    forall as bs cs ds es fs gs hs
    . stream8 as bs cs ds es fs gs (G.new hs)
    = B.map (\((a, b, c, d, e, f, h), g) -> (a, b, c, d, e, f, g, h))
    $ lockedZip2S (G.length as) (swallow7 as bs cs ds es fs gs) (swallow (G.new hs))
  #-}




-- Locked zips ----------------------------------------------------------------
-- | Zip the first 'n' elements of two streams.
lockedZip2S
        :: Monad m 
        => Int
        -> Bundle m v a -> Bundle m v b 
        -> Bundle m v (a, b)

lockedZip2S len
        (Bundle {sElems=Stream mkStep1 sa1})
        (Bundle {sElems=Stream mkStep2 sa2})
 = B.fromStream (Stream step (sa1, sa2, 0)) (S.Exact len)
 where 
        {-# INLINE_INNER step #-}
        step (s1, s2, i)
         = i `seq`
           do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                return $ case (step1, step2) of
                          (Yield x1 s1', Yield x2 s2')
                            | i < len   -> Yield (x1, x2) (s1', s2', i + 1)
                          _             -> Done
{-# INLINE_STREAM lockedZip2S #-}


-------------------------------------------------
-- | Zip the first 'n' elements of three streams.
lockedZip3S
        :: Monad m 
        => Int
        -> Bundle m v a -> Bundle m v b -> Bundle m v c
        -> Bundle m v (a, b, c)

lockedZip3S len
        (Bundle {sElems=Stream mkStep1 sa1})
        (Bundle {sElems=Stream mkStep2 sa2})
        (Bundle {sElems=Stream mkStep3 sa3})
 = B.fromStream (Stream step (sa1, sa2, sa3, 0)) (S.Exact len)
 where 
        {-# INLINE_INNER step #-}
        step (s1, s2, s3, i)
         = i `seq`
           do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                step3   <- mkStep3 s3
                return $ case (step1, step2, step3) of
                          (Yield x1 s1', Yield x2 s2', Yield x3 s3')  
                           | i < len    -> Yield (x1, x2, x3) (s1', s2', s3', i + 1)

                          _ -> Done
{-# INLINE_STREAM lockedZip3S #-}


-------------------------------------------------
-- | Zip the first 'n' elements of four streams.
lockedZip4S
        :: Monad m 
        => Int
        -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
        -> Bundle m v (a, b, c, d)

lockedZip4S len
        (Bundle {sElems=Stream mkStep1 sa1})
        (Bundle {sElems=Stream mkStep2 sa2})
        (Bundle {sElems=Stream mkStep3 sa3})
        (Bundle {sElems=Stream mkStep4 sa4})
 = B.fromStream (Stream step (sa1, sa2, sa3, sa4, 0)) (S.Exact len)
 where 
        {-# INLINE_INNER step #-}
        step (s1, s2, s3, s4, i)
         = i `seq`
           do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                step3   <- mkStep3 s3
                step4   <- mkStep4 s4
                return $ case (step1, step2, step3, step4) of
                          (Yield x1 s1', Yield x2 s2', Yield x3 s3', Yield x4 s4')  
                           | i < len    -> Yield (x1, x2, x3, x4) (s1', s2', s3', s4', i + 1)

                          _ -> Done
{-# INLINE_STREAM lockedZip4S #-}


-------------------------------------------------
-- | Zip the first 'n' elements of five streams.
lockedZip5S
        :: Monad m 
        => Int
        -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d -> Bundle m v e
        -> Bundle m v (a, b, c, d, e)

lockedZip5S len
        (Bundle {sElems=Stream mkStep1 sa1})
        (Bundle {sElems=Stream mkStep2 sa2})
        (Bundle {sElems=Stream mkStep3 sa3})
        (Bundle {sElems=Stream mkStep4 sa4})
        (Bundle {sElems=Stream mkStep5 sa5})
 = B.fromStream (Stream step (sa1, sa2, sa3, sa4, sa5, 0)) (S.Exact len)
 where 
        {-# INLINE_INNER step #-}
        step (s1, s2, s3, s4, s5, i)
         = i `seq`
           do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                step3   <- mkStep3 s3
                step4   <- mkStep4 s4
                step5   <- mkStep5 s5
                return $ case (step1, step2, step3, step4, step5) of
                          (Yield x1 s1', Yield x2 s2', Yield x3 s3', Yield x4 s4', Yield x5 s5')  
                           | i < len    -> Yield (x1, x2, x3, x4, x5) (s1', s2', s3', s4', s5', i + 1)

                          _ -> Done
{-# INLINE_STREAM lockedZip5S #-}


-------------------------------------------------
-- | Zip the first 'n' elements of six streams.
lockedZip6S
        :: Monad m 
        => Int
        -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d -> Bundle m v e -> Bundle m v f
        -> Bundle m v (a, b, c, d, e, f)

lockedZip6S len
        (Bundle {sElems=Stream mkStep1 sa1})
        (Bundle {sElems=Stream mkStep2 sa2})
        (Bundle {sElems=Stream mkStep3 sa3})
        (Bundle {sElems=Stream mkStep4 sa4})
        (Bundle {sElems=Stream mkStep5 sa5})
        (Bundle {sElems=Stream mkStep6 sa6})
 = B.fromStream (Stream step (sa1, sa2, sa3, sa4, sa5, sa6, 0)) (S.Exact len)
 where 
        {-# INLINE_INNER step #-}
        step (s1, s2, s3, s4, s5, s6, i)
         = i `seq`
           do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                step3   <- mkStep3 s3
                step4   <- mkStep4 s4
                step5   <- mkStep5 s5
                step6   <- mkStep6 s6
                return $ case (step1, step2, step3, step4, step5, step6) of
                          (Yield x1 s1', Yield x2 s2', Yield x3 s3', Yield x4 s4', Yield x5 s5', Yield x6 s6')  
                           | i < len    -> Yield (x1, x2, x3, x4, x5, x6) (s1', s2', s3', s4', s5', s6', i + 1)

                          _ -> Done
{-# INLINE_STREAM lockedZip6S #-}


-------------------------------------------------
-- | Zip the first 'n' elements of seven streams.
lockedZip7S
        :: Monad m 
        => Int
        -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d -> Bundle m v e -> Bundle m v f -> Bundle m v g
        -> Bundle m v (a, b, c, d, e, f, g)

lockedZip7S len
        (Bundle {sElems=Stream mkStep1 sa1})
        (Bundle {sElems=Stream mkStep2 sa2})
        (Bundle {sElems=Stream mkStep3 sa3})
        (Bundle {sElems=Stream mkStep4 sa4})
        (Bundle {sElems=Stream mkStep5 sa5})
        (Bundle {sElems=Stream mkStep6 sa6})
        (Bundle {sElems=Stream mkStep7 sa7})
 = B.fromStream (Stream step (sa1, sa2, sa3, sa4, sa5, sa6, sa7, 0)) (S.Exact len)
 where 
        {-# INLINE_INNER step #-}
        step (s1, s2, s3, s4, s5, s6, s7, i)
         = i `seq`
           do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                step3   <- mkStep3 s3
                step4   <- mkStep4 s4
                step5   <- mkStep5 s5
                step6   <- mkStep6 s6
                step7   <- mkStep7 s7
                return $ case (step1, step2, step3, step4, step5, step6, step7) of
                          (Yield x1 s1', Yield x2 s2', Yield x3 s3', Yield x4 s4', Yield x5 s5', Yield x6 s6', Yield x7 s7')  
                           | i < len    -> Yield (x1, x2, x3, x4, x5, x6, x7) (s1', s2', s3', s4', s5', s6', s7', i + 1)

                          _ -> Done
{-# INLINE_STREAM lockedZip7S #-}


-------------------------------------------------
-- | Zip the first 'n' elements of eight streams.
lockedZip8S
        :: Monad m 
        => Int
        -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d -> Bundle m v e -> Bundle m v f -> Bundle m v g -> Bundle m v h
        -> Bundle m v (a, b, c, d, e, f, g, h)

lockedZip8S len
        (Bundle {sElems=Stream mkStep1 sa1})
        (Bundle {sElems=Stream mkStep2 sa2})
        (Bundle {sElems=Stream mkStep3 sa3})
        (Bundle {sElems=Stream mkStep4 sa4})
        (Bundle {sElems=Stream mkStep5 sa5})
        (Bundle {sElems=Stream mkStep6 sa6})
        (Bundle {sElems=Stream mkStep7 sa7})
        (Bundle {sElems=Stream mkStep8 sa8})
 = B.fromStream (Stream step (sa1, sa2, sa3, sa4, sa5, sa6, sa7, sa8, 0)) (S.Exact len)
 where 
        {-# INLINE_INNER step #-}
        step (s1, s2, s3, s4, s5, s6, s7, s8, i)
         = i `seq`
           do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                step3   <- mkStep3 s3
                step4   <- mkStep4 s4
                step5   <- mkStep5 s5
                step6   <- mkStep6 s6
                step7   <- mkStep7 s7
                step8   <- mkStep8 s8
                return $ case (step1, step2, step3, step4, step5, step6, step7, step8) of
                          (Yield x1 s1', Yield x2 s2', Yield x3 s3', Yield x4 s4', Yield x5 s5', Yield x6 s6', Yield x7 s7', Yield x8 s8')
                           | i < len    -> Yield (x1, x2, x3, x4, x5, x6, x7, x8) (s1', s2', s3', s4', s5', s6', s7', s8', i + 1)

                          _ -> Done
{-# INLINE_STREAM lockedZip8S #-}
