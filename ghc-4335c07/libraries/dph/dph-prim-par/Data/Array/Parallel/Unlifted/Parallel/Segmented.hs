{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel combinators for segmented unboxed arrays
module Data.Array.Parallel.Unlifted.Parallel.Segmented
        ( replicateRSUP
        , appendSUP
        , appendSUP_old
        , appendSUPV
        , foldRUP
        , sumRUP)
where
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Distributed.What
import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.UPSegd                     (UPSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd                    (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   as Seq
import qualified Data.Array.Parallel.Unlifted.Vectors                   as Vs
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd           as UPSegd
import qualified Data.Array.Parallel.Unlifted.Sequential                as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd          as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd         as USSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPVSegd          as UPVSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSSegd          as UPSSegd
import Data.Vector.Fusion.Bundle.Monadic ( Bundle(..), fromStream )
import Data.Vector.Fusion.Stream.Monadic ( Stream(..), Step(..) )
import Data.Vector.Fusion.Bundle.Size    ( Size(..) )
import qualified Data.Vector.Fusion.Bundle                              as S

import GHC.Exts -- for unboxed primops
import GHC.Base ( isTrue# )

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Parallel.Segmented." Prelude.++ s


-- replicate ------------------------------------------------------------------
-- | Segmented replication.
--   Each element in the vector is replicated the given number of times.
--
--   @replicateRSUP 2 [1, 2, 3, 4, 5] = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]@
--

--   TODO: make this efficient
replicateRSUP :: Unbox a => Int -> Vector a -> Vector a
replicateRSUP n xs
        = UPSegd.replicateWithP (UPSegd.fromLengths (replicateUP (Seq.length xs) n)) xs
{-# INLINE_UP replicateRSUP #-}


-- Append ---------------------------------------------------------------------
-- | Segmented append.
appendSUP
        :: Unbox a
        => UPSegd   -- ^ segment descriptor of result
        -> UPSegd   -- ^ left-hand segd
        -> Vector a -- ^ left-hand data
        -> UPSegd   -- ^ right-hand segd
        -> Vector a -- ^ right-hand data
        -> Vector a

appendSUP segd !xd !xs !yd !ys
  = joinD theGang balanced
  . mapD  (What "appendSUP/append") theGang append
  $ UPSegd.takeDistributed segd
  where append ((segd',seg_off),el_off)
         = Seq.unstream
         $ appendSegS (UPSegd.takeUSegd xd) xs
                      (UPSegd.takeUSegd yd) ys
                      (USegd.takeElements segd')
                      seg_off el_off
{-# INLINE_UP appendSUP #-}


-- append ---------------------------------------------------------------------
appendSegS
        :: Unbox a
        => USegd        -- ^ Segment descriptor of first array.
        -> Vector a     -- ^ Data of first array
        -> USegd        -- ^ Segment descriptor of second array.
        -> Vector a     -- ^ Data of second array.
        -> Int          -- ^ How many elements to return
        -> Int          -- ^ Segment offset
        -> Int          -- ^ Element offset
        -> S.Bundle v a

appendSegS !xd !xs !yd !ys !n seg_off el_off
  = fromStream (Stream next state) (Exact n)
  where
    !xlens = USegd.takeLengths xd
    !ylens = USegd.takeLengths yd

    -- Two index functions because of monomorphism restriction
    {-# INLINE index1 #-}
    index1  = Seq.index (here "appendSegS")

    {-# INLINE index2 #-}
    index2  = Seq.index (here "appendSegS")

    {-# INLINE unbox #-}
    unbox (I# i) = i

    state
      -- Nothing to return
      | n == 0 = ASDo
            { as_takefrom = 0#
            , as_seg_off  = 0#
            , as_xs_index = 0#
            , as_ys_index = 0#
            , as_next_swap= 0#
            , as_remain   = 0# }

      -- Start returning data from xs
      | el_off < xlens `index1` seg_off
      = let xi   = (USegd.takeIndices xd `index1` seg_off) + el_off
            yi   =  USegd.takeIndices yd `index1` seg_off
            swap = (USegd.takeLengths xd `index1` seg_off) - el_off
        in  ASDo
            -- start reading from xs, then read from ys at end of this xs segment
            { as_takefrom = 0#
            , as_seg_off  = unbox seg_off
            , as_xs_index = unbox xi
            , as_ys_index = unbox yi
            , as_next_swap= unbox swap
            , as_remain   = unbox n }

      -- Start with ys
      | otherwise
      = let -- NOTE: *not* indicesUSegd xd ! (seg_off+1) since seg_off+1
            -- might be out of bounds
            xi      = (USegd.takeIndices xd `index1` seg_off) + (USegd.takeLengths xd `index1` seg_off)
            el_off' = el_off - USegd.takeLengths xd `index1` seg_off
            yi      = (USegd.takeIndices yd `index1` seg_off) + el_off'
            swap    = (USegd.takeLengths yd `index1` seg_off) - el_off'
        in  ASDo
            { as_takefrom = 1#
            , as_seg_off  = unbox seg_off
            , as_xs_index = unbox xi
            , as_ys_index = unbox yi
            , as_next_swap= unbox swap
            , as_remain   = unbox n }

    {-# INLINE next #-}
    next ASDo{as_remain=0#} = return Done

    -- Reading from xs
    next s@ASDo{as_takefrom=0#}
      -- Done reading xs, so read the rest of this segment from ys.
      | isTrue# (as_next_swap s  ==# 0#)
      = return $ Skip (s{as_takefrom=1#, as_next_swap= unbox (ylens `index1` I# (as_seg_off s))})

      -- Grab a value from xs
      | otherwise  = return $ Yield (xs `index2` I# (as_xs_index s)) (inc s)

    -- Reading from ys; takefrom nonzero
    next s
      -- Done reading ys, so we need to look at the next segment's xs
      | isTrue# (as_next_swap s  ==# 0#)
      = let seg' = as_seg_off s +# 1#
        in  return $ Skip (s {as_takefrom=0#, as_seg_off=seg', as_next_swap= unbox (xlens `index1` I# seg')})

      -- Grab a value from ys
      | otherwise = return $ Yield (ys `index2` I# (as_ys_index s)) (inc s)

    {-# INLINE inc #-}
    -- Move data pointer forward, and decrease remaining and swap
    inc s@ASDo{as_takefrom=0#, as_xs_index=xi, as_next_swap=swap, as_remain=n'}
      = s{as_xs_index=xi +# 1#, as_next_swap=swap -# 1#, as_remain=n' -# 1#}

    -- Takefrom is nonzero: reading from ys
    inc s@ASDo{as_ys_index=yi, as_next_swap=swap, as_remain=n'}
      = s{as_ys_index=yi +# 1#, as_next_swap=swap -# 1#, as_remain=n' -# 1#}
{-# INLINE_STREAM appendSegS #-}

data AppendState
        = ASDo
        { as_takefrom :: Int#   -- ^ 0 = xs, nonzero = ys
        , as_seg_off  :: Int#   -- ^ current segment
        , as_xs_index :: Int#   -- ^ pointer into xs data
        , as_ys_index :: Int#   -- ^ pointer into ys data
        , as_next_swap:: Int#   -- ^ toggle takefrom in this many elements
        , as_remain   :: Int#   -- ^ how many left
        }

-- virtual scattered append
appendSUPV
        :: (Vs.Unboxes a, Unbox a)
        => UPSegd            -- ^ segment descriptor of result
        -> UPVSegd.UPVSegd   -- ^ left-hand segd
        -> Vs.Vectors a      -- ^ left-hand data
        -> UPVSegd.UPVSegd   -- ^ right-hand segd
        -> Vs.Vectors a      -- ^ right-hand data
        -> Vector a

appendSUPV segd !xd !xs !yd !ys
  = joinD theGang balanced
  . mapD  (What "appendSUPV/append") theGang append
  $ UPSegd.takeDistributed segd
  where append ((segd',seg_off),el_off)
         = Seq.unstream
         $ appendUPVSegS xd xs
                      yd ys
                      (USegd.takeElements segd')
                      seg_off el_off
{-# INLINE_UP appendSUPV #-}
appendUPVSegS
        :: Vs.Unboxes a
        => UPVSegd.UPVSegd  -- ^ Segment descriptor of first array.
        -> Vs.Vectors a     -- ^ Data of first array
        -> UPVSegd.UPVSegd  -- ^ Segment descriptor of second array.
        -> Vs.Vectors a     -- ^ Data of second array.
        -> Int              -- ^ How many elements to return
        -> Int              -- ^ Segment offset
        -> Int              -- ^ Element offset
        -> S.Bundle v a

appendUPVSegS !xd !xs !yd !ys !n seg_off el_off
  = fromStream (Stream next state) (Exact n)
  where
    !xvsegs= UPVSegd.takeVSegidsRedundant xd
    !yvsegs= UPVSegd.takeVSegidsRedundant yd

    !xssegd= UPSSegd.takeUSSegd $ UPVSegd.takeUPSSegdRedundant xd
    !yssegd= UPSSegd.takeUSSegd $ UPVSegd.takeUPSSegdRedundant yd

    !xsegd = USSegd.takeUSegd xssegd
    !ysegd = USSegd.takeUSegd yssegd

    -- get physical segment id
    {-#INLINE xpseg #-}
    xpseg s = index1 xvsegs "xpseg" s
    {-#INLINE ypseg #-}
    ypseg s = index1 yvsegs "ypseg" s

    !xseglens = USegd.takeLengths xsegd
    !yseglens = USegd.takeLengths ysegd

    !xsrc     = USSegd.takeSources xssegd
    !ysrc     = USSegd.takeSources yssegd

    !xstrt    = USSegd.takeStarts xssegd
    !ystrt    = USSegd.takeStarts yssegd

    -- physical lengths
    {-#INLINE xplen #-}
    xplen s = index1 xseglens "xplen1" (xpseg s)
    {-#INLINE yplen #-}
    yplen s = index1 yseglens "yplen1" (ypseg s)

    -- get actual data
    {-# INLINE gdata #-}
    gdata gs st
              = let !src  = avs_ssrc     st
                    !strt = avs_sstart   st
                    !ix   = avs_index    st
                in  index2 gs (I# src) (I# (strt +# ix))

    -- get scattered segment source and starts
    {-# INLINE getscatter #-}
    getscatter gpseg gsrcs gstrts segid
              = let !phys = gpseg segid                                  in
                let !src  = index1 gsrcs  "src" phys                         in
                let !strt = index1 gstrts "strt" phys                         in
                    (src, strt)

    {-# INLINE index1 #-}
    --index1 v i = Seq.index (here "appendUVSegS") v i

    index1 v h i = Seq.index (here $ "appendUVSegS:" Prelude.++ h) v i

    {-# INLINE index2 #-}
    index2 v i1 i2 = Vs.index2 (here "appendUVSegS") v i1 i2


    {-# INLINE unbox #-}
    unbox (I# i) = i

    state
      -- Nothing to return
      | n == 0 = ASUPVDo
            { avs_takefrom = 0#
            , avs_seg_off  = 0#
            , avs_index    = 0#
            , avs_next_swap= 0#
            , avs_remain   = 0#
            , avs_sstart   = 0#
            , avs_ssrc     = 0# }

      -- Start returning data from xs
      | el_off < xplen seg_off
      = let (src,strt)      = getscatter xpseg xsrc xstrt seg_off
            swap            = (xplen seg_off) - el_off
        in  ASUPVDo
            -- start reading from xs, then read from ys at end of this xs segment
            { avs_takefrom = 0#
            , avs_seg_off  = unbox seg_off
            , avs_index    = unbox el_off
            , avs_next_swap= unbox swap
            , avs_remain   = unbox n
            , avs_sstart   = unbox strt
            , avs_ssrc     = unbox src }

      -- Start with ys
      | otherwise
      = let (src,strt)      = getscatter ypseg ysrc ystrt seg_off
            el_off'         = el_off        - xplen seg_off
            swap            = (yplen seg_off) - el_off'
        in  ASUPVDo
            { avs_takefrom = 1#
            , avs_seg_off  = unbox seg_off
            , avs_index    = unbox el_off'
            , avs_next_swap= unbox swap
            , avs_remain   = unbox n
            , avs_sstart   = unbox strt
            , avs_ssrc     = unbox src }

    {-# INLINE next #-}
    next ASUPVDo{avs_remain=0#} = return Done

    -- Reading from xs
    next s@ASUPVDo{avs_takefrom=0#}
      -- Done reading xs, so read the rest of this segment from ys.
      | isTrue# (avs_next_swap s  ==# 0#)  =
        let     seg'            = I# (avs_seg_off s)
                (src,strt)      = getscatter ypseg ysrc ystrt seg'
        in      return $ Skip $
                s {
                  avs_takefrom  = 1#
                , avs_index     = 0#
                , avs_next_swap = unbox (yplen seg')
                , avs_sstart    = unbox strt
                , avs_ssrc      = unbox src }
      -- Grab a value from xs
      | otherwise  = return $ Yield (gdata xs s) (inc s)

    -- Reading from ys, so avs_takefrom=1#
    next s
      -- Done reading ys, so we need to look at the next segment's xs
      | isTrue# (avs_next_swap s  ==# 0#)
      = let     seg'            = I# (avs_seg_off s +# 1#)
                (src,strt)      = getscatter xpseg xsrc xstrt seg'
        in      return $ Skip $
                s {
                  avs_takefrom  = 0#
                , avs_seg_off   = unbox seg'
                , avs_index     = 0#
                , avs_next_swap = unbox (xplen seg')
                , avs_sstart    = unbox strt
                , avs_ssrc      = unbox src }

      -- Grab a value from ys
      | otherwise = return $ Yield (gdata ys s) (inc s)

    {-# INLINE inc #-}
    -- Move data pointer forward and decrease remaining and swap
    inc s@ASUPVDo{avs_index=ix, avs_next_swap=swap, avs_remain=n'}
      = s{avs_index=ix +# 1#, avs_next_swap=swap -# 1#, avs_remain=n' -# 1#}
{-# INLINE_STREAM appendUPVSegS #-}

data AppendUPVState
        = ASUPVDo
        { avs_takefrom :: Int#   -- ^ 0 = xs, 1/else = ys
        , avs_seg_off  :: Int#   -- ^ current segment
        , avs_index    :: Int#   -- ^ index into current segment
        , avs_next_swap:: Int#   -- ^ toggle takefrom in this many elements
        , avs_remain   :: Int#   -- ^ how many left
        , avs_sstart   :: Int#   -- ^ scattered segment start
        , avs_ssrc     :: Int#   -- ^ scattered segment source
        }

-- Append ---------------------------------------------------------------------
-- | Segmented append.
-- -old
appendSUP_old
        :: Unbox a
        => UPSegd
        -> UPSegd -> Vector a
        -> UPSegd -> Vector a
        -> Vector a

appendSUP_old segd !xd !xs !yd !ys
  = joinD theGang balanced
  . mapD  (What "appendSUP_old/append") theGang append
  $ UPSegd.takeDistributed segd
  where append ((segd',seg_off),el_off)
         = Seq.unstream
         $ appendSegS_old (UPSegd.takeUSegd xd) xs
                      (UPSegd.takeUSegd yd) ys
                      (USegd.takeElements segd')
                      seg_off el_off
{-# INLINE_UP appendSUP_old #-}


-- append ---------------------------------------------------------------------
appendSegS_old
        :: Unbox a
        => USegd        -- ^ Segment descriptor of first array.
        -> Vector a     -- ^ Data of first array
        -> USegd        -- ^ Segment descriptor of second array.
        -> Vector a     -- ^ Data of second array.
        -> Int
        -> Int
        -> Int
        -> S.Bundle v a

appendSegS_old !xd !xs !yd !ys !n seg_off el_off
  = fromStream (Stream next state) (Exact n)
  where
    !xlens = USegd.takeLengths xd
    !ylens = USegd.takeLengths yd

    {-# INLINE index1 #-}
    index1  = Seq.index (here "appendSegS")

    {-# INLINE index2 #-}
    index2  = Seq.index (here "appendSegS")

    state
      | n == 0 = Nothing
      | el_off < xlens `index1` seg_off
      = let i = (USegd.takeIndices xd `index1` seg_off) + el_off
            j =  USegd.takeIndices yd `index1` seg_off
            k = (USegd.takeLengths xd `index1` seg_off) - el_off
        in  Just (False, seg_off, i, j, k, n)

      | otherwise
      = let -- NOTE: *not* indicesUSegd xd ! (seg_off+1) since seg_off+1
            -- might be out of bounds
            i       = (USegd.takeIndices xd `index1` seg_off) + (USegd.takeLengths xd `index1` seg_off)
            el_off' = el_off - USegd.takeLengths xd `index1` seg_off
            j       = (USegd.takeIndices yd `index1` seg_off) + el_off'
            k       = (USegd.takeLengths yd `index1` seg_off) - el_off'
        in  Just (True, seg_off, i, j, k, n)

    {-# INLINE next #-}
    next Nothing = return Done

    next (Just (False, seg, i, j, k, n'))
      | n' == 0    = return Done
      | k  == 0    = return $ Skip (Just (True, seg, i, j, ylens `index1` seg, n'))
      | otherwise  = return $ Yield (xs `index2` i) (Just (False, seg, i+1, j, k-1, n'-1))

    next (Just (True, seg, i, j, k, n'))
      | n' == 0    = return Done
      | k  == 0
      = let !seg' = seg+1
        in  return $ Skip (Just (False, seg', i, j, xlens `index1` seg', n'))

      | otherwise = return $ Yield (ys `index2` j) (Just (True, seg, i, j+1, k-1, n'-1))
{-# INLINE_STREAM appendSegS_old #-}


-- foldR ----------------------------------------------------------------------
-- | Regular segmented fold.
foldRUP :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> Int -> Vector a -> Vector b
foldRUP f z !segSize xs
  = joinD theGang unbalanced
          (mapD (What "foldRUP/foldRU") theGang
              (Seq.foldlRU f z segSize)
              (splitAsD theGang (mapD (What "foldRUP/segSize") theGang (*segSize) dlen) xs))
  where
    noOfSegs    = Seq.length xs `div` segSize
    dlen        = splitLenD theGang noOfSegs
{-# INLINE_UP foldRUP #-}


-- sumR -----------------------------------------------------------------------
-- | Regular segmented sum.
sumRUP :: (Num e, Unbox e) => Int -> Vector e -> Vector e
sumRUP = foldRUP (+) 0
{-# INLINE_UP sumRUP #-}


