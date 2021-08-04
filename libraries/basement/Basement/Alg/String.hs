{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP                        #-}
module Basement.Alg.String
    ( copyFilter
    , validate
    , findIndexPredicate
    , revFindIndexPredicate
    ) where

import           GHC.Prim
import           GHC.ST
import           Basement.Alg.Class
import           Basement.Alg.UTF8
import           Basement.Compat.Base
import           Basement.Numerical.Additive
import           Basement.Types.OffsetSize
import           Basement.PrimType
import           Basement.Block (MutableBlock(..))

import           Basement.UTF8.Helper
import           Basement.UTF8.Table
import           Basement.UTF8.Types

copyFilter :: forall s container . Indexable container Word8
           => (Char -> Bool)
           -> CountOf Word8
           -> MutableByteArray# s
           -> container
           -> Offset Word8
           -> ST s (CountOf Word8)
copyFilter predicate !sz dst src start = loop (Offset 0) start
  where
    !end = start `offsetPlusE` sz
    loop !d !s
        | s == end  = pure (offsetAsSize d)
        | otherwise =
            let !h = nextAscii src s
             in case headerIsAscii h of
                    True | predicate (toChar1 h) -> primMbaWrite dst d (stepAsciiRawValue h) >> loop (d + Offset 1) (s + Offset 1)
                         | otherwise             -> loop d (s + Offset 1)
                    False ->
                        case next src s of
                            Step c s' | predicate c -> writeUTF8 (MutableBlock dst :: MutableBlock Word8 s) d c >>= \d' -> loop d' s'
                                      | otherwise   -> loop d s'
{-# INLINE copyFilter #-}

validate :: Indexable container Word8
         => Offset Word8
         -> container
         -> Offset Word8
         -> (Offset Word8, Maybe ValidationFailure)
validate end ba ofsStart = loop4 ofsStart
  where
    loop4 !ofs
        | ofs4 < end =
            let h1 = nextAscii ba ofs
                h2 = nextAscii ba (ofs+1)
                h3 = nextAscii ba (ofs+2)
                h4 = nextAscii ba (ofs+3)
             in if headerIsAscii h1 && headerIsAscii h2 && headerIsAscii h3 && headerIsAscii h4
                    then loop4 ofs4
                    else loop ofs
        | otherwise     = loop ofs
      where
        !ofs4 = ofs+4
    loop !ofs
        | ofs == end      = (end, Nothing)
        | headerIsAscii h = loop (ofs + Offset 1)
        | otherwise       = multi (CountOf $ getNbBytes h) ofs
      where
        h = nextAscii ba ofs

    multi (CountOf 0xff) pos = (pos, Just InvalidHeader)
    multi nbConts pos
        | (posNext `offsetPlusE` nbConts) > end = (pos, Just MissingByte)
        | otherwise =
            case nbConts of
                CountOf 1 ->
                    let c1 = index ba posNext
                    in if isContinuation c1
                        then loop (pos + Offset 2)
                        else (pos, Just InvalidContinuation)
                CountOf 2 ->
                    let c1 = index ba posNext
                        c2 = index ba (pos + Offset 2)
                     in if isContinuation2 c1 c2
                            then loop (pos + Offset 3)
                            else (pos, Just InvalidContinuation)
                CountOf _ ->
                    let c1 = index ba posNext
                        c2 = index ba (pos + Offset 2)
                        c3 = index ba (pos + Offset 3)
                     in if isContinuation3 c1 c2 c3
                            then loop (pos + Offset 4)
                            else (pos, Just InvalidContinuation)
      where posNext = pos + Offset 1
{-# INLINE validate #-}

findIndexPredicate :: Indexable container Word8
                   => (Char -> Bool)
                   -> container
                   -> Offset Word8
                   -> Offset Word8
                   -> Offset Word8
findIndexPredicate predicate ba !startIndex !endIndex = loop startIndex
  where
    loop !i
        | i < endIndex && not (predicate c) = loop (i')
        | otherwise                         = i
      where
        Step c i' = next ba i
{-# INLINE findIndexPredicate #-}

revFindIndexPredicate :: Indexable container Word8
                      => (Char -> Bool)
                      -> container
                      -> Offset Word8
                      -> Offset Word8
                      -> Offset Word8
revFindIndexPredicate predicate ba startIndex endIndex
    | endIndex > startIndex = loop endIndex
    | otherwise             = endIndex
  where
    loop !i
        | predicate c     = i'
        | i' > startIndex = loop i'
        | otherwise       = endIndex
      where 
        StepBack c i' = prev ba i
{-# INLINE revFindIndexPredicate #-}
