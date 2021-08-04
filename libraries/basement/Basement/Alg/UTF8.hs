{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Basement.Alg.UTF8
    ( nextAscii
    , nextAsciiDigit
    , expectAscii
    , next
    , nextSkip
    , nextWith
    , prev
    , prevSkip
    , writeASCII
    , writeUTF8
    , toList
    , all
    , any
    , foldr
    , length
    , reverse
    ) where

import           GHC.Types
import           GHC.Word
import           GHC.Prim
import           Data.Bits
import           Data.Proxy
import           Basement.Alg.Class
import           Basement.Compat.Base hiding (toList)
import           Basement.Compat.Primitive
import           Basement.Monad
import           Basement.Numerical.Additive
import           Basement.Numerical.Subtractive
import           Basement.Types.OffsetSize
import           Basement.Types.Char7 (Char7(..))
import           Basement.PrimType
import           Basement.UTF8.Helper
import           Basement.UTF8.Table
import           Basement.UTF8.Types

nextAscii :: Indexable container Word8 => container -> Offset Word8 -> StepASCII
nextAscii ba n = StepASCII w
  where
    !w = index ba n
{-# INLINE nextAscii #-}

-- | nextAsciiBa specialized to get a digit between 0 and 9 (included)
nextAsciiDigit :: Indexable container Word8 => container -> Offset Word8 -> StepDigit
nextAsciiDigit ba n = StepDigit (index ba n - 0x30)
{-# INLINE nextAsciiDigit #-}

expectAscii :: Indexable container Word8 => container -> Offset Word8 -> Word8 -> Bool
expectAscii ba n v = index ba n == v
{-# INLINE expectAscii #-}

next :: Indexable container Word8 => container -> Offset8 -> Step
next ba n =
    case getNbBytes h of
        0 -> Step (toChar1 h) (n + Offset 1)
        1 -> Step (toChar2 h (index ba (n + Offset 1))) (n + Offset 2)
        2 -> Step (toChar3 h (index ba (n + Offset 1))
                             (index ba (n + Offset 2))) (n + Offset 3)
        3 -> Step (toChar4 h (index ba (n + Offset 1))
                             (index ba (n + Offset 2))
                             (index ba (n + Offset 3))) (n + Offset 4)
        r -> error ("next: internal error: invalid input: offset=" <> show n <> " table=" <> show r <> " h=" <> show (stepAsciiRawValue h))
  where
    !h = nextAscii ba n
{-# INLINE next #-}

nextSkip :: Indexable container Word8 => container -> Offset Word8 -> Offset Word8
nextSkip ba n = n + 1 + Offset (getNbBytes (nextAscii ba n))
{-# INLINE nextSkip #-}

-- | special case for only non ascii next'er function
nextWith :: Indexable container Word8
         => StepASCII
         -> container
         -> Offset8
         -> Step
nextWith h ba n =
    case getNbBytes h of
        1 -> Step (toChar2 h (index ba n)) (n + Offset 1)
        2 -> Step (toChar3 h (index ba n) (index ba (n + Offset 1))) (n + Offset 2)
        3 -> Step (toChar4 h (index ba n)
                             (index ba (n + Offset 1))
                             (index ba (n + Offset 2))) (n + Offset 3)
        r -> error ("nextWith: internal error: invalid input: offset=" <> show n <> " table=" <> show r <> " h=" <> show (stepAsciiRawValue h))
{-# INLINE nextWith #-}

-- Given a non null offset, give the previous character and the offset of this character
-- will fail bad if apply at the beginning of string or an empty string.
prev :: Indexable container Word8 => container -> Offset Word8 -> StepBack
prev ba offset =
    case index ba prevOfs1 of
        (W8# v1) | isContinuation# v1 -> atLeast2 (maskContinuation# v1)
                 | otherwise          -> StepBack (toChar# v1) prevOfs1
  where
    sz1 = CountOf 1
    !prevOfs1 = offset `offsetMinusE` sz1
    prevOfs2 = prevOfs1 `offsetMinusE` sz1
    prevOfs3 = prevOfs2 `offsetMinusE` sz1
    prevOfs4 = prevOfs3 `offsetMinusE` sz1
    atLeast2 !v  =
        case index ba prevOfs2 of
            (W8# v2) | isContinuation# v2 -> atLeast3 (or# (uncheckedShiftL# (maskContinuation# v2) 6#) v)
                     | otherwise          -> StepBack (toChar# (or# (uncheckedShiftL# (maskHeader2# v2) 6#) v)) prevOfs2
    atLeast3 !v =
        case index ba prevOfs3 of
            (W8# v3) | isContinuation# v3 -> atLeast4 (or# (uncheckedShiftL# (maskContinuation# v3) 12#) v)
                     | otherwise          -> StepBack (toChar# (or# (uncheckedShiftL# (maskHeader3# v3) 12#) v)) prevOfs3
    atLeast4 !v =
        case index ba prevOfs4 of
            (W8# v4) -> StepBack (toChar# (or# (uncheckedShiftL# (maskHeader4# v4) 18#) v)) prevOfs4

prevSkip :: Indexable container Word8 => container -> Offset Word8 -> Offset Word8
prevSkip ba offset = loop (offset `offsetMinusE` sz1)
  where
    sz1 = CountOf 1
    loop o
        | isContinuation (index ba o) = loop (o `offsetMinusE` sz1)
        | otherwise                       = o

writeASCII :: (PrimMonad prim, RandomAccess container prim Word8)
           => container -> Offset8 -> Char7 -> prim ()
writeASCII mba !i (Char7 c) = write mba i c
{-# INLINE writeASCII #-}

writeUTF8 :: (PrimMonad prim, RandomAccess container prim Word8)
          => container -> Offset8 -> Char -> prim Offset8
writeUTF8 mba !i !c
    | bool# (ltWord# x 0x80##   ) = encode1
    | bool# (ltWord# x 0x800##  ) = encode2
    | bool# (ltWord# x 0x10000##) = encode3
    | otherwise                   = encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = write mba i (W8# x) >> pure (i + Offset 1)
    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        write mba i     (W8# x1)
        write mba (i+1) (W8# x2)
        pure (i + Offset 2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        write mba i            (W8# x1)
        write mba (i+Offset 1) (W8# x2)
        write mba (i+Offset 2) (W8# x3)
        pure (i + Offset 3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        write mba i            (W8# x1)
        write mba (i+Offset 1) (W8# x2)
        write mba (i+Offset 2) (W8# x3)
        write mba (i+Offset 3) (W8# x4)
        pure (i + Offset 4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##
{-# INLINE writeUTF8 #-}

toList :: Indexable container Word8 => container -> Offset Word8 -> Offset Word8 -> [Char]
toList ba !start !end = loop start
  where
    loop !idx
        | idx == end = []
        | otherwise  = c : loop idx'
      where (Step c idx') = next ba idx

all :: Indexable container Word8
    => (Char -> Bool) -> container -> Offset Word8 -> Offset Word8 -> Bool
all predicate ba start end = loop start
  where
    loop !idx
        | idx == end  = True
        | predicate c = loop idx'
        | otherwise   = False
      where (Step c idx') = next ba idx
{-# INLINE all #-}

any :: Indexable container Word8
    => (Char -> Bool) -> container -> Offset Word8 -> Offset Word8 -> Bool
any predicate ba start end = loop start
  where
    loop !idx
        | idx == end  = False
        | predicate c = True
        | otherwise   = loop idx'
      where (Step c idx') = next ba idx
{-# INLINE any #-}

foldr :: Indexable container Word8
      => container -> Offset Word8 -> Offset Word8 -> (Char -> a -> a) -> a -> a
foldr dat start end f acc = loop start
  where
    loop !i
        | i == end  = acc
        | otherwise =
            let (Step c i') = next dat i
             in c `f` loop i'
{-# INLINE foldr #-}

length :: (Indexable container Word8, Indexable container Word64)
       => container -> Offset Word8 -> Offset Word8 -> CountOf Char
length dat start end
    | start == end = 0
    | otherwise    = processStart 0 start
  where
    end64 :: Offset Word64
    end64 = offsetInElements end

    prx64 :: Proxy Word64
    prx64 = Proxy

    mask64_80 :: Word64
    mask64_80 = 0x8080808080808080

    processStart :: CountOf Char -> Offset Word8 -> CountOf Char
    processStart !c !i
        | i == end                = c
        | offsetIsAligned prx64 i = processAligned c (offsetInElements i)
        | otherwise               =
            let h    = index dat i
                cont = (h .&. 0xc0) == 0x80
                c'   = if cont then c else c+1
             in processStart c' (i+1)
    processAligned :: CountOf Char -> Offset Word64 -> CountOf Char
    processAligned !c !i
        | i >= end64 = processEnd c (offsetInBytes i)
        | otherwise  =
            let !h   = index dat i -- Word64
                !h80 = h .&. mask64_80
             in if h80 == 0
                 then processAligned (c+8) (i+1)
                 else let !nbAscii = if h80 == mask64_80 then 0 else CountOf (8 - popCount h80)
                          !nbHigh  = CountOf $ popCount (h .&. (h80 `unsafeShiftR` 1))
                       in processAligned (c + nbAscii + nbHigh) (i+1)
    processEnd !c !i
        | i == end  = c
        | otherwise =
            let h    = index dat i
                cont = (h .&. 0xc0) == 0x80
                c'   = if cont then c else c+1
             in processStart c' (i+1)
{-# INLINE length #-}

reverse :: (PrimMonad prim, Indexable container Word8)
        => MutableByteArray# (PrimState prim) -- ^ Destination buffer
        -> Offset Word8                       -- ^ Destination start
        -> container                          -- ^ Source buffer
        -> Offset Word8                       -- ^ Source start
        -> Offset Word8                       -- ^ Source end
        -> prim ()
reverse dst dstOfs src start end
    | start == end = pure ()
    | otherwise    = loop (dstOfs `offsetPlusE` (offsetAsSize (end `offsetSub` start)) `offsetSub` 1) start
  where
    loop !d !s
        | s == end        = pure ()
        | headerIsAscii h = primMbaWrite dst d (stepAsciiRawValue h) >> loop (d `offsetSub` 1) (s + 1)
        | otherwise       = do
            case getNbBytes h of
                1 -> do
                    primMbaWrite dst (d `offsetSub` 1) (stepAsciiRawValue h)
                    primMbaWrite dst d                 (index src (s + 1))
                    loop (d `offsetSub` 2) (s + 2)
                2 -> do
                    primMbaWrite dst (d `offsetSub` 2) (stepAsciiRawValue h)
                    primMbaWrite dst (d `offsetSub` 1) (index src (s + 1))
                    primMbaWrite dst d                 (index src (s + 2))
                    loop (d `offsetSub` 3) (s + 3)
                3 -> do
                    primMbaWrite dst (d `offsetSub` 3) (stepAsciiRawValue h)
                    primMbaWrite dst (d `offsetSub` 2) (index src (s + 1))
                    primMbaWrite dst (d `offsetSub` 1) (index src (s + 2))
                    primMbaWrite dst d                 (index src (s + 3))
                    loop (d `offsetSub` 4) (s + 4)
                _ -> error "impossible"
      where h = nextAscii src s
{-# INLINE reverse #-}
