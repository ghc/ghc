{-# LANGUAGE CPP, NoMonomorphismRestriction #-}
#include "fusion-phases.h"
module Data.Array.Parallel.Unlifted.Stream.Swallow
        ( swallow
        , swallow2
        , swallow3
        , swallow4
        , swallow5
        , swallow6
        , swallow7
        , swallow8

        , repeatS
        , swallowS)
where
import Data.Vector.Generic               as G
import Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Bundle.Monadic as B
import Data.Vector.Fusion.Bundle.Size    ( Size(..) )
import Data.Vector.Fusion.Util           as S
import qualified Data.Vector.Generic.New as New


-- Swallow --------------------------------------------------------------------
-- | Swallow a vector.
--   This is like `stream`, except there is no end-of-vector check. We rely on
--   the consumer to know how many elements to demand. Using this function 
--   instead of 'stream' avoids the associated loop counters in fused code.
--
--   Trying to swallow more elements than are in the vector is undefined.
--
swallow :: Monad m => Vector v a => v a -> Bundle m v a
swallow v 
 = v 
 `seq` n 
 `seq` (B.fromStream (S.unfoldr get 0) Unknown)
 where
        n   = G.length v

        {-# INLINE_INNER get #-}
        get i       
         | Box a        <- basicUnsafeIndexM v i 
         = Just (a, i + 1)
{-# INLINE_STREAM swallow #-}


-- | Swallow two vectors.
swallow2 
        :: (Monad m, Vector v a, Vector v b)
        => v a -> v b 
        -> Bundle m v (a, b)

swallow2 aa bb
 = aa `seq` bb 
 `seq` n 
 `seq` (B.fromStream (S.unfoldr get 0) Unknown)
 where  n        = G.length aa

        {-# INLINE_INNER get #-}
        get i   
         | Box a        <- basicUnsafeIndexM aa i
         , Box b        <- basicUnsafeIndexM bb i
         = Just ((a, b), i + 1)
{-# INLINE_STREAM swallow2 #-}


-- | Swallow three vectors.
swallow3
        :: (Monad m, Vector v a, Vector v b, Vector v c)
        => v a -> v b -> v c
        -> Bundle m v (a, b, c)

swallow3 aa bb cc
 = aa `seq` bb `seq` cc
 `seq` n 
 `seq` (B.fromStream (S.unfoldr get 0) Unknown)
 where  n        = G.length aa

        {-# INLINE_INNER get #-}
        get i   
         | Box a        <- basicUnsafeIndexM aa i
         , Box b        <- basicUnsafeIndexM bb i
         , Box c        <- basicUnsafeIndexM cc i
         = Just ((a, b, c), i + 1)
{-# INLINE_STREAM swallow3 #-}


-- | Swallow four vectors.
swallow4
        :: (Monad m, Vector v a, Vector v b, Vector v c, Vector v d)
        => v a -> v b -> v c -> v d
        -> Bundle m v (a, b, c, d)

swallow4 aa bb cc dd
 = aa `seq` bb `seq` cc `seq` dd
 `seq` n 
 `seq` (B.fromStream (S.unfoldr get 0) Unknown)
 where  n        = G.length aa

        {-# INLINE_INNER get #-}
        get i   
         | Box a        <- basicUnsafeIndexM aa i
         , Box b        <- basicUnsafeIndexM bb i
         , Box c        <- basicUnsafeIndexM cc i
         , Box d        <- basicUnsafeIndexM dd i
         = Just ((a, b, c, d), i + 1)
{-# INLINE_STREAM swallow4 #-}


-- | Swallow five vectors.
swallow5
        :: (Monad m, Vector v a, Vector v b, Vector v c, Vector v d, Vector v e)
        => v a -> v b -> v c -> v d -> v e
        -> Bundle m v (a, b, c, d, e)

swallow5 aa bb cc dd ee
 = aa `seq` bb `seq` cc `seq` dd `seq` ee
 `seq` n 
 `seq` (B.fromStream (S.unfoldr get 0) Unknown)
 where  n        = G.length aa

        {-# INLINE_INNER get #-}
        get i   
         | Box a        <- basicUnsafeIndexM aa i
         , Box b        <- basicUnsafeIndexM bb i
         , Box c        <- basicUnsafeIndexM cc i
         , Box d        <- basicUnsafeIndexM dd i
         , Box e        <- basicUnsafeIndexM ee i
         = Just ((a, b, c, d, e), i + 1)
{-# INLINE_STREAM swallow5 #-}


-- | Swallow six vectors.
swallow6
        :: (Monad m, Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f)
        => v a -> v b -> v c -> v d -> v e -> v f
        -> Bundle m v (a, b, c, d, e, f)

swallow6 aa bb cc dd ee ff
 = aa `seq` bb `seq` cc `seq` dd `seq` ee `seq` ff
 `seq` n 
 `seq` (B.fromStream (S.unfoldr get 0) Unknown)
 where  n        = G.length aa

        {-# INLINE_INNER get #-}
        get i   
         | Box a        <- basicUnsafeIndexM aa i
         , Box b        <- basicUnsafeIndexM bb i
         , Box c        <- basicUnsafeIndexM cc i
         , Box d        <- basicUnsafeIndexM dd i
         , Box e        <- basicUnsafeIndexM ee i
         , Box f        <- basicUnsafeIndexM ff i
         = Just ((a, b, c, d, e, f), i + 1)
{-# INLINE_STREAM swallow6 #-}


-- | Swallow seven vectors.
swallow7
        :: (Monad m, Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v g)
        => v a -> v b -> v c -> v d -> v e -> v f -> v g
        -> Bundle m v (a, b, c, d, e, f, g)

swallow7 aa bb cc dd ee ff gg
 = aa `seq` bb `seq` cc `seq` dd `seq` ee `seq` ff `seq` gg
 `seq` n 
 `seq` (B.fromStream (S.unfoldr get 0) Unknown)
 where  n        = G.length aa

        {-# INLINE_INNER get #-}
        get i   
         | Box a        <- basicUnsafeIndexM aa i
         , Box b        <- basicUnsafeIndexM bb i
         , Box c        <- basicUnsafeIndexM cc i
         , Box d        <- basicUnsafeIndexM dd i
         , Box e        <- basicUnsafeIndexM ee i
         , Box f        <- basicUnsafeIndexM ff i
         , Box g        <- basicUnsafeIndexM gg i
         = Just ((a, b, c, d, e, f, g), i + 1)
{-# INLINE_STREAM swallow7 #-}


-- | Swallow eight vectors.
swallow8
        :: (Monad m, Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v g, Vector v h)
        => v a -> v b -> v c -> v d -> v e -> v f -> v g -> v h
        -> Bundle m v (a, b, c, d, e, f, g, h)

swallow8 aa bb cc dd ee ff gg hh
 = aa `seq` bb `seq` cc `seq` dd `seq` ee `seq` ff `seq` gg `seq` hh
 `seq` n 
 `seq` (B.fromStream (S.unfoldr get 0) Unknown)
 where  n        = G.length aa

        {-# INLINE_INNER get #-}
        get i   
         | Box a        <- basicUnsafeIndexM aa i
         , Box b        <- basicUnsafeIndexM bb i
         , Box c        <- basicUnsafeIndexM cc i
         , Box d        <- basicUnsafeIndexM dd i
         , Box e        <- basicUnsafeIndexM ee i
         , Box f        <- basicUnsafeIndexM ff i
         , Box g        <- basicUnsafeIndexM gg i
         , Box h        <- basicUnsafeIndexM hh i
         = Just ((a, b, c, d, e, f, g, h), i + 1)
{-# INLINE_STREAM swallow8 #-}


-- Repeat ---------------------------------------------------------------------
repeatS :: Monad m => m a -> Bundle m v a
repeatS x
 = B.fromStream (Stream step ()) Unknown
 where
        {-# INLINE_INNER step #-}
        step _
         = do   v <- x
                return $ Yield v ()
{-# INLINE_STREAM repeatS #-}


-- | Swallow a whole stream.
--   Indicates that the consumer knows how many elements to demand.
swallowS :: Bundle m v a -> Bundle m v a
swallowS s = s
{-# INLINE [1] swallowS #-}


-- If we're swallowing a whole vector created from a stream,
--  then just swallow the stream directly.
--  This is similar to the "stream/unstream" rule.
{-# RULES "swallow/new/unstream"
    forall s
    . swallow (new (New.unstream s)) = swallowS s
  #-}


-- If the context knows how many elements to demand, 
-- then rewrite replicate to repeat, so that the code for the generator 
-- doesn't need to keep a loop counter for how many elements still need to
-- be generated.
{-# RULES "swallowS/replicate"
    forall len x
    . swallowS (B.replicateM len x) = repeatS x
    #-}
