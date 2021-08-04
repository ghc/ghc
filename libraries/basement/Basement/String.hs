-- |
-- Module      : Basement.String
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A String type backed by a UTF8 encoded byte array and all the necessary
-- functions to manipulate the string.
--
-- You can think of String as a specialization of a byte array that
-- have element of type Char.
--
-- The String data must contain UTF8 valid data.
--
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Basement.String
    ( String(..)
    , MutableString(..)
    , create
    , replicate
    , length
    -- * Binary conversion
    , Encoding(..)
    , fromBytes
    , fromChunkBytes
    , fromBytesUnsafe
    , fromBytesLenient
    , toBytes
    , mutableValidate
    , copy
    , ValidationFailure(..)
    , index
    , null
    , drop
    , take
    , splitAt
    , revDrop
    , revTake
    , revSplitAt
    , splitOn
    , sub
    , elem
    , indices
    , intersperse
    , span
    , spanEnd
    , break
    , breakEnd
    , breakElem
    , breakLine
    , dropWhile
    , singleton
    , charMap
    , snoc
    , cons
    , unsnoc
    , uncons
    , find
    , findIndex
    , sortBy
    , filter
    , reverse
    , replace
    , builderAppend
    , builderBuild
    , builderBuild_
    , readInteger
    , readIntegral
    , readNatural
    , readDouble
    , readRational
    , readFloatingExact
    , upper
    , lower
    , caseFold
    , isPrefixOf
    , isSuffixOf
    , isInfixOf
    , stripPrefix
    , stripSuffix
    , all
    , any
    -- * Legacy utility
    , lines
    , words
    , toBase64
    , toBase64URL
    , toBase64OpenBSD
    ) where

import           Basement.UArray           (UArray)
import qualified Basement.UArray           as Vec
import qualified Basement.UArray           as C
import qualified Basement.UArray.Mutable   as MVec
import           Basement.Block.Mutable (Block(..), MutableBlock(..))
import qualified Basement.Block.Mutable    as MBLK
import           Basement.Compat.Bifunctor
import           Basement.Compat.Base
import           Basement.Compat.Natural
import           Basement.Compat.MonadTrans
import           Basement.Compat.Primitive
import           Basement.Types.OffsetSize
import           Basement.Numerical.Additive
import           Basement.Numerical.Subtractive
import           Basement.Numerical.Multiplicative
import           Basement.Numerical.Number
import           Basement.Cast
import           Basement.Monad
import           Basement.PrimType
import           Basement.FinalPtr
import           Basement.IntegralConv
import           Basement.Floating
import           Basement.MutableBuilder
import           Basement.String.CaseMapping (upperMapping, lowerMapping, foldMapping)
import           Basement.UTF8.Table
import           Basement.UTF8.Helper
import           Basement.UTF8.Base
import           Basement.UTF8.Types
import           Basement.UArray.Base as C (onBackendPrim, onBackend, onBackendPure, offset, ValidRange(..), offsetsValidRange, MUArray(..), MUArrayBackend(..))
import           Basement.Alg.Class (Indexable)
import qualified Basement.Alg.UTF8 as UTF8
import qualified Basement.Alg.String as Alg
import           Basement.Types.Char7 (Char7(..), c7Upper, c7Lower)
import qualified Basement.Types.Char7 as Char7
import           GHC.Prim
import           GHC.ST
import           GHC.Types
import           GHC.Word
#if MIN_VERSION_base(4,9,0)
import           GHC.Char
#endif

 -- temporary
import qualified Data.List
import           Data.Ratio
import           Data.Char (toUpper, toLower)
import qualified Prelude

import qualified Basement.String.Encoding.Encoding   as Encoder
import qualified Basement.String.Encoding.ASCII7     as Encoder
import qualified Basement.String.Encoding.UTF16      as Encoder
import qualified Basement.String.Encoding.UTF32      as Encoder
import qualified Basement.String.Encoding.ISO_8859_1 as Encoder

-- | UTF8 Encoder
data EncoderUTF8 = EncoderUTF8

instance Encoder.Encoding EncoderUTF8 where
    type Unit EncoderUTF8 = Word8
    type Error EncoderUTF8 = ValidationFailure
    encodingNext  _ = \ofs -> Right . nextWithIndexer ofs
    encodingWrite _ = writeWithBuilder

-- | Validate a bytearray for UTF8'ness
--
-- On success Nothing is returned
-- On Failure the position along with the failure reason
validate :: UArray Word8
         -> Offset8
         -> CountOf Word8
         -> (Offset8, Maybe ValidationFailure)
validate array ofsStart sz = C.unsafeDewrap goBa goAddr array
  where
    unTranslateOffset start = first (\e -> e `offsetSub` start)
    goBa ba start =
        unTranslateOffset start $ Alg.validate (start+end) ba (start + ofsStart)
    goAddr ptr@(Ptr !_) start =
        pure $ unTranslateOffset start $ Alg.validate (start+end) ptr (ofsStart + start)
    end = ofsStart `offsetPlusE` sz

-- | Similar to 'validate' but works on a 'MutableByteArray'
mutableValidate :: PrimMonad prim
                => MVec.MUArray Word8 (PrimState prim)
                -> Offset Word8
                -> CountOf Word8
                -> prim (Offset Word8, Maybe ValidationFailure)
mutableValidate mba ofsStart sz = do
    loop ofsStart
  where
    end = ofsStart `offsetPlusE` sz

    loop ofs
        | ofs > end  = error "mutableValidate: internal error: went pass offset"
        | ofs == end = return (end, Nothing)
        | otherwise  = do
            r <- one ofs
            case r of
                (nextOfs, Nothing)  -> loop nextOfs
                (pos, Just failure) -> return (pos, Just failure)

    one pos = do
        h <- StepASCII <$> Vec.unsafeRead mba pos
        let nbConts = getNbBytes h
        if nbConts == 0xff
            then return (pos, Just InvalidHeader)
            else if pos + 1 + Offset nbConts > end
                then return (pos, Just MissingByte)
                else do
                    case nbConts of
                        0 -> return (pos + 1, Nothing)
                        1 -> do
                            c1 <- Vec.unsafeRead mba (pos + 1)
                            if isContinuation c1
                                then return (pos + 2, Nothing)
                                else return (pos, Just InvalidContinuation)
                        2 -> do
                            c1 <- Vec.unsafeRead mba (pos + 1)
                            c2 <- Vec.unsafeRead mba (pos + 2)
                            if isContinuation c1 && isContinuation c2
                                then return (pos + 3, Nothing)
                                else return (pos, Just InvalidContinuation)
                        3 -> do
                            c1 <- Vec.unsafeRead mba (pos + 1)
                            c2 <- Vec.unsafeRead mba (pos + 2)
                            c3 <- Vec.unsafeRead mba (pos + 3)
                            if isContinuation c1 && isContinuation c2 && isContinuation c3
                                then return (pos + 4, Nothing)
                                else return (pos, Just InvalidContinuation)
                        _ -> error "internal error"

nextWithIndexer :: (Offset Word8 -> Word8)
                -> Offset Word8
                -> (Char, Offset Word8)
nextWithIndexer getter off =
    case getNbBytes# h of
        0# -> (toChar h, off + 1)
        1# -> (toChar (decode2 (getter $ off + 1)), off + 2)
        2# -> (toChar (decode3 (getter $ off + 1) (getter $ off + 2)), off + 3)
        3# -> (toChar (decode4 (getter $ off + 1) (getter $ off + 2) (getter $ off + 3))
              , off + 4)
        r -> error ("next: internal error: invalid input: " <> show (I# r) <> " " <> show (W# h))
  where
    !(W8# h) = getter off

    toChar :: Word# -> Char
    toChar w = C# (chr# (word2Int# w))

    decode2 :: Word8 -> Word#
    decode2 (W8# c1) =
        or# (uncheckedShiftL# (and# h 0x1f##) 6#)
            (and# c1 0x3f##)

    decode3 :: Word8 -> Word8 -> Word#
    decode3 (W8# c1) (W8# c2) =
        or# (uncheckedShiftL# (and# h 0xf##) 12#)
            (or# (uncheckedShiftL# (and# c1 0x3f##) 6#)
                 (and# c2 0x3f##))

    decode4 :: Word8 -> Word8 -> Word8 -> Word#
    decode4 (W8# c1) (W8# c2) (W8# c3) =
        or# (uncheckedShiftL# (and# h 0x7##) 18#)
            (or# (uncheckedShiftL# (and# c1 0x3f##) 12#)
                (or# (uncheckedShiftL# (and# c2 0x3f##) 6#)
                    (and# c3 0x3f##))
            )

writeWithBuilder :: (PrimMonad st, Monad st)
                 => Char
                 -> Builder (UArray Word8) (MVec.MUArray Word8) Word8 st err ()
writeWithBuilder c
    | bool# (ltWord# x 0x80##   ) = encode1
    | bool# (ltWord# x 0x800##  ) = encode2
    | bool# (ltWord# x 0x10000##) = encode3
    | otherwise = encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = Vec.builderAppend (W8# x)

    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        Vec.builderAppend (W8# x1) >> Vec.builderAppend (W8# x2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        Vec.builderAppend (W8# x1) >> Vec.builderAppend (W8# x2) >> Vec.builderAppend (W8# x3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        Vec.builderAppend (W8# x1) >> Vec.builderAppend (W8# x2) >> Vec.builderAppend (W8# x3) >> Vec.builderAppend (W8# x4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##

writeUTF8Char :: PrimMonad prim => MutableString (PrimState prim) -> Offset8 -> UTF8Char -> prim ()
writeUTF8Char (MutableString mba) i (UTF8_1 x1) =
    Vec.unsafeWrite mba i     x1
writeUTF8Char (MutableString mba) i (UTF8_2 x1 x2) = do
    Vec.unsafeWrite mba i     x1
    Vec.unsafeWrite mba (i+1) x2
writeUTF8Char (MutableString mba) i (UTF8_3 x1 x2 x3) = do
    Vec.unsafeWrite mba i     x1
    Vec.unsafeWrite mba (i+1) x2
    Vec.unsafeWrite mba (i+2) x3
writeUTF8Char (MutableString mba) i (UTF8_4 x1 x2 x3 x4) = do
    Vec.unsafeWrite mba i     x1
    Vec.unsafeWrite mba (i+1) x2
    Vec.unsafeWrite mba (i+2) x3
    Vec.unsafeWrite mba (i+3) x4
{-# INLINE writeUTF8Char #-}

unsafeFreezeShrink :: PrimMonad prim => MutableString (PrimState prim) -> CountOf Word8 -> prim String
unsafeFreezeShrink (MutableString mba) s = String <$> Vec.unsafeFreezeShrink mba s
{-# INLINE unsafeFreezeShrink #-}

------------------------------------------------------------------------
-- real functions

-- | Check if a String is null
null :: String -> Bool
null (String ba) = C.length ba == 0

-- we don't know in constant time the count of character in string,
-- however if we estimate bounds of what N characters would
-- take in space (between N and N*4). If the count is thus bigger than
-- the number of bytes, then we know for sure that it's going to
-- be out of bounds
countCharMoreThanBytes :: CountOf Char -> UArray Word8 -> Bool
countCharMoreThanBytes (CountOf chars) ba = chars >= bytes
  where (CountOf bytes) = C.length ba

-- | Create a string composed of a number @n of Chars (Unicode code points).
--
-- if the input @s contains less characters than required, then the input string is returned.
take :: CountOf Char -> String -> String
take n s@(String ba)
    | n <= 0                      = mempty
    | countCharMoreThanBytes n ba = s
    | otherwise                   = String $ Vec.unsafeTake (offsetAsSize $ indexN n s) ba

-- | Create a string with the remaining Chars after dropping @n Chars from the beginning
drop :: CountOf Char -> String -> String
drop n s@(String ba)
    | n <= 0                      = s
    | countCharMoreThanBytes n ba = mempty
    | otherwise                   = String $ Vec.drop (offsetAsSize $ indexN n s) ba

-- | Split a string at the Offset specified (in Char) returning both
-- the leading part and the remaining part.
splitAt :: CountOf Char -> String -> (String, String)
splitAt n s@(String ba)
    | n <= 0                      = (mempty, s)
    | countCharMoreThanBytes n ba = (s, mempty)
    | otherwise                   =
        let (v1,v2) = C.splitAt (offsetAsSize $ indexN n s) ba
         in (String v1, String v2)

-- | Return the offset (in bytes) of the N'th sequence in an UTF8 String
indexN :: CountOf Char -> String -> Offset Word8
indexN !n (String ba) = Vec.unsafeDewrap goVec goAddr ba
  where
    goVec :: Block Word8 -> Offset Word8 -> Offset Word8
    goVec (Block !ma) !start = loop start 0
      where
        !len = start `offsetPlusE` Vec.length ba
        loop :: Offset Word8 -> Offset Char -> Offset Word8
        loop !idx !i
            | idx >= len || i .==# n = sizeAsOffset (idx - start)
            | otherwise              = loop (idx `offsetPlusE` d) (i + Offset 1)
          where d = skipNextHeaderValue (primBaIndex ma idx)
    {-# INLINE goVec #-}

    goAddr :: Ptr Word8 -> Offset Word8 -> ST s (Offset Word8)
    goAddr (Ptr ptr) !start = return $ loop start (Offset 0)
      where
        !len = start `offsetPlusE` Vec.length ba
        loop :: Offset Word8 -> Offset Char -> Offset Word8
        loop !idx !i
            | idx >= len || i .==# n = sizeAsOffset (idx - start)
            | otherwise              = loop (idx `offsetPlusE` d) (i + Offset 1)
          where d = skipNextHeaderValue (primAddrIndex ptr idx)
    {-# INLINE goAddr #-}
{-# INLINE indexN #-}

-- inverse a CountOf that is specified from the end (e.g. take n Chars from the end)
--
-- rev{Take,Drop,SplitAt} TODO optimise:
-- we can process the string from the end using a skipPrev instead of getting the length
countFromStart :: String -> CountOf Char -> CountOf Char
countFromStart s sz@(CountOf sz')
    | sz >= len = CountOf 0
    | otherwise = CountOf (len' - sz')
  where len@(CountOf len') = length s

-- | Similar to 'take' but from the end
revTake :: CountOf Char -> String -> String
revTake n v = drop (countFromStart v n) v

-- | Similar to 'drop' but from the end
revDrop :: CountOf Char -> String -> String
revDrop n v = take (countFromStart v n) v

-- | Similar to 'splitAt' but from the end
revSplitAt :: CountOf Char -> String -> (String, String)
revSplitAt n v = (drop idx v, take idx v) where idx = countFromStart v n

-- | Split on the input string using the predicate as separator
--
-- e.g.
--
-- > splitOn (== ',') ","          == ["",""]
-- > splitOn (== ',') ",abc,"      == ["","abc",""]
-- > splitOn (== ':') "abc"        == ["abc"]
-- > splitOn (== ':') "abc::def"   == ["abc","","def"]
-- > splitOn (== ':') "::abc::def" == ["","","abc","","def"]
--
splitOn :: (Char -> Bool) -> String -> [String]
splitOn predicate s
    | sz == CountOf 0 = [mempty]
    | otherwise    = loop azero azero
  where
    !sz = size s
    end = azero `offsetPlusE` sz
    loop prevIdx idx
        | idx == end = [sub s prevIdx idx]
        | otherwise =
            let !(Step c idx') = next s idx
             in if predicate c
                    then sub s prevIdx idx : loop idx' idx'
                    else loop prevIdx idx'

-- | Internal call to make a substring given offset in bytes.
--
-- This is unsafe considering that one can create a substring
-- starting and/or ending on the middle of a UTF8 sequence.
sub :: String -> Offset8 -> Offset8 -> String
sub (String ba) start end = String $ Vec.sub ba start end

-- | Internal call to split at a given index in offset of bytes.
--
-- This is unsafe considering that one can split in the middle of a
-- UTF8 sequence, so use with care.
splitIndex :: Offset8 -> String -> (String, String)
splitIndex idx (String ba) = (String v1, String v2)
  where (v1,v2) = C.splitAt (offsetAsSize idx) ba

-- | Break a string into 2 strings at the location where the predicate return True
break :: (Char -> Bool) -> String -> (String, String)
break predicate s@(String ba) = runST $ Vec.unsafeIndexer ba go
  where
    !sz = size s
    end = azero `offsetPlusE` sz

    go :: (Offset Word8 -> Word8) -> ST st (String, String)
    go getIdx = loop (Offset 0)
      where
        !nextI = nextWithIndexer getIdx
        loop idx
            | idx == end = return (s, mempty)
            | otherwise  = do
                let (c, idx') = nextI idx
                case predicate c of
                    True  -> return $ splitIndex idx s
                    False -> loop idx'
        {-# INLINE loop #-}
{-# INLINE [2] break #-}

breakEnd :: (Char -> Bool) -> String -> (String, String)
breakEnd predicate s@(String arr)
    | k == end  = (s, mempty)
    | otherwise = splitIndex (k `offsetSub` start) s
  where
    k = C.onBackend goVec (\_ -> pure . goAddr) arr
    (C.ValidRange !start !end) = offsetsValidRange arr
    goVec ba@(Block !_) = let k = Alg.revFindIndexPredicate predicate ba start end
                        in if k == end then end else UTF8.nextSkip ba k
    goAddr ptr@(Ptr !_) =
        let k = Alg.revFindIndexPredicate predicate ptr start end
         in if k == end then end else UTF8.nextSkip ptr k
{-# INLINE [2] breakEnd #-}

#if MIN_VERSION_base(4,9,0)
{-# RULES "break (== 'c')" [3] forall c . break (eqChar c) = breakElem c #-}
#else
{-# RULES "break (== 'c')" [3] forall c . break (== c) = breakElem c #-}
#endif

-- | Break a string into 2 strings at the first occurence of the character
breakElem :: Char -> String -> (String, String)
breakElem !el s@(String ba)
    | sz == 0   = (mempty, mempty)
    | otherwise =
        case asUTF8Char el of
            UTF8_1 w -> let !(v1,v2) = Vec.breakElem w ba in (String v1, String v2)
            _        -> runST $ Vec.unsafeIndexer ba go
  where
    sz = size s
    end = azero `offsetPlusE` sz

    go :: (Offset Word8 -> Word8) -> ST st (String, String)
    go getIdx = loop (Offset 0)
      where
        !nextI = nextWithIndexer getIdx
        loop idx
            | idx == end = return (s, mempty)
            | otherwise  = do
                let (c, idx') = nextI idx
                case el == c of
                    True  -> return $ splitIndex idx s
                    False -> loop idx'

-- | Same as break but cut on a line feed with an optional carriage return.
--
-- This is the same operation as 'breakElem LF' dropping the last character of the
-- string if it's a CR.
--
-- Also for efficiency reason (streaming), it returns if the last character was a CR character.
breakLine :: String -> Either Bool (String, String)
breakLine (String arr) = bimap String String <$> Vec.breakLine arr

-- | Apply a @predicate@ to the string to return the longest prefix that satisfy the predicate and
-- the remaining
span :: (Char -> Bool) -> String -> (String, String)
span predicate s = break (not . predicate) s

-- | Apply a @predicate@ to the string to return the longest suffix that satisfy the predicate and
-- the remaining
spanEnd :: (Char -> Bool) -> String -> (String, String)
spanEnd predicate s = breakEnd (not . predicate) s

-- | Drop character from the beginning while the predicate is true
dropWhile :: (Char -> Bool) -> String -> String
dropWhile predicate = snd . break (not . predicate)

-- | Return whereas the string contains a specific character or not
elem :: Char -> String -> Bool
elem !el s@(String ba) =
    case asUTF8Char el of
        UTF8_1 w -> Vec.elem w ba
        _        -> runST $ Vec.unsafeIndexer ba go
  where
    sz = size s
    end = azero `offsetPlusE` sz

    go :: (Offset Word8 -> Word8) -> ST st Bool
    go getIdx = loop (Offset 0)
      where
        !nextI = nextWithIndexer getIdx
        loop !idx
            | idx == end = return False
            | otherwise  = do
                let (c, idx') = nextI idx
                case el == c of
                    True  -> return True
                    False -> loop idx'

-- | Intersperse the character @sep@ between each character in the string
--
-- > intersperse ' ' "Hello Foundation"
-- "H e l l o   F o u n d a t i o n"
intersperse :: Char -> String -> String
intersperse sep src = case length src - 1 of
    Nothing   -> src
    Just 0    -> src
    Just gaps -> runST $ unsafeCopyFrom src dstBytes go
        where
          lastSrcI :: Offset Char
          lastSrcI = 0 `offsetPlusE` gaps
          dstBytes = (size src :: CountOf Word8) + (gaps `scale` charToBytes (fromEnum sep))

          go :: String -> Offset Char -> Offset8 -> MutableString s -> Offset8 -> ST s (Offset8, Offset8)
          go src' srcI srcIdx dst dstIdx
              | srcI == lastSrcI = do
                  nextDstIdx <- write dst dstIdx c
                  return (nextSrcIdx, nextDstIdx)
              | otherwise        = do
                  nextDstIdx  <- write dst dstIdx c
                  nextDstIdx' <- write dst nextDstIdx sep
                  return (nextSrcIdx, nextDstIdx')
            where
              !(Step c nextSrcIdx) = next src' srcIdx

-- | Allocate a new @String@ with a fill function that has access to the characters of
--   the source @String@.
unsafeCopyFrom :: String -- ^ Source string
               -> CountOf Word8  -- ^ Length of the destination string in bytes
               -> (String -> Offset Char -> Offset8 -> MutableString s -> Offset8 -> ST s (Offset8, Offset8))
               -- ^ Function called for each character in the source String
               -> ST s String -- ^ Returns the filled new string
unsafeCopyFrom src dstBytes f = new dstBytes >>= fill (Offset 0) (Offset 0) (Offset 0) f >>= freeze
  where
    srcLen = length src
    end = Offset 0 `offsetPlusE` srcLen
    fill srcI srcIdx dstIdx f' dst'
        | srcI == end = return dst'
        | otherwise = do (nextSrcIdx, nextDstIdx) <- f' src srcI srcIdx dst' dstIdx
                         fill (srcI + Offset 1) nextSrcIdx nextDstIdx f' dst'

-- | Length of a String using CountOf
--
-- this size is available in o(n)
length :: String -> CountOf Char
length (String arr)
    | start == end = 0
    | otherwise    = C.onBackend goVec (\_ -> pure . goAddr) arr
  where
    (C.ValidRange !start !end) = offsetsValidRange arr
    goVec ma = UTF8.length ma start end
    goAddr ptr = UTF8.length ptr start end

-- | Replicate a character @c@ @n@ times to create a string of length @n@
replicate :: CountOf Char -> Char -> String
replicate (CountOf n) c = runST (new nbBytes >>= fill)
  where
    nbBytes   = scale (cast n :: Word) sz
    sz = charToBytes (fromEnum c)
    fill :: PrimMonad prim => MutableString (PrimState prim) -> prim String
    fill ms = loop (Offset 0)
      where
        loop idx
            | idx .==# nbBytes = freeze ms
            | otherwise        = write ms idx c >>= loop

-- | Copy the String
--
-- The slice of memory is copied to a new slice, making the new string
-- independent from the original string..
copy :: String -> String
copy (String s) = String (Vec.copy s)

-- | Create a single element String
singleton :: Char -> String
singleton c = runST $ do
    ms <- new nbBytes
    _  <- write ms (Offset 0) c
    freeze ms
  where
    !nbBytes = charToBytes (fromEnum c)

-- | Unsafely create a string of up to @sz@ bytes.
--
-- The callback @f@ needs to return the number of bytes filled in the underlaying
-- bytes buffer. No check is made on the callback return values, and if it's not
-- contained without the bounds, bad things will happen.
create :: PrimMonad prim => CountOf Word8 -> (MutableString (PrimState prim) -> prim (Offset Word8)) -> prim String
create sz f = do
    ms     <- new sz
    filled <- f ms
    if filled .==# sz
        then freeze ms
        else do
            s <- freeze ms
            let (String ba) = s
            pure $ String $ C.take (offsetAsSize filled) ba

-- | Monomorphically map the character in a string and return the transformed one
charMap :: (Char -> Char) -> String -> String
charMap f src
    | srcSz == 0 = mempty
    | otherwise  =
        let !(elems, nbBytes) = allocateAndFill [] (Offset 0) (CountOf 0)
         in runST $ do
                dest <- new nbBytes
                copyLoop dest elems (Offset 0 `offsetPlusE` nbBytes)
                freeze dest
  where
    !srcSz = size src
    srcEnd = azero `offsetPlusE` srcSz

    allocateAndFill :: [(String, CountOf Word8)]
                    -> Offset8
                    -> CountOf Word8
                    -> ([(String,CountOf Word8)], CountOf Word8)
    allocateAndFill acc idx bytesWritten
        | idx == srcEnd = (acc, bytesWritten)
        | otherwise     =
            let (el@(_,addBytes), idx') = runST $ do
                    -- make sure we allocate at least 4 bytes for the destination for the last few bytes
                    -- otherwise allocating less would bring the danger of spinning endlessly
                    -- and never succeeding.
                    let !diffBytes = srcEnd - idx
                        !allocatedBytes = if diffBytes <= CountOf 4 then CountOf 4 else diffBytes
                    ms <- new allocatedBytes
                    (dstIdx, srcIdx) <- fill ms allocatedBytes idx
                    s <- freeze ms
                    return ((s, dstIdx), srcIdx)
             in allocateAndFill (el : acc) idx' (bytesWritten + addBytes)

    fill :: PrimMonad prim
         => MutableString (PrimState prim)
         -> CountOf Word8
         -> Offset8
         -> prim (CountOf Word8, Offset8)
    fill mba dsz srcIdxOrig =
        loop (Offset 0) srcIdxOrig
      where
        endDst = (Offset 0) `offsetPlusE` dsz
        loop dstIdx srcIdx
            | srcIdx == srcEnd = return (offsetAsSize dstIdx, srcIdx)
            | dstIdx == endDst = return (offsetAsSize dstIdx, srcIdx)
            | otherwise        =
                let !(Step c srcIdx') = next src srcIdx
                    c' = f c -- the mapped char
                    !nbBytes = charToBytes (fromEnum c')
                 in -- check if we have room in the destination buffer
                    if dstIdx `offsetPlusE` nbBytes <= sizeAsOffset dsz
                        then do dstIdx' <- write mba dstIdx c'
                                loop dstIdx' srcIdx'
                        else return (offsetAsSize dstIdx, srcIdx)

    copyLoop _   []     (Offset 0) = return ()
    copyLoop _   []     n          = error ("charMap invalid: " <> show n)
    copyLoop ms@(MutableString mba) ((String ba, sz):xs) end = do
        let start = end `offsetMinusE` sz
        Vec.unsafeCopyAtRO mba start ba (Offset 0) sz
        copyLoop ms xs start

-- | Append a Char to the end of the String and return this new String
snoc :: String -> Char -> String
snoc s@(String ba) c
    | len == CountOf 0 = singleton c
    | otherwise     = runST $ do
        ms <- new (len + nbBytes)
        let (MutableString mba) = ms
        Vec.unsafeCopyAtRO mba (Offset 0) ba (Offset 0) len
        _ <- write ms (azero `offsetPlusE` len) c
        freeze ms
  where
    !len     = size s
    !nbBytes = charToBytes (fromEnum c)

-- | Prepend a Char to the beginning of the String and return this new String
cons :: Char -> String -> String
cons c s@(String ba)
  | len == CountOf 0 = singleton c
  | otherwise     = runST $ do
      ms <- new (len + nbBytes)
      let (MutableString mba) = ms
      idx <- write ms (Offset 0) c
      Vec.unsafeCopyAtRO mba idx ba (Offset 0) len
      freeze ms
  where
    !len     = size s
    !nbBytes = charToBytes (fromEnum c)

-- | Extract the String stripped of the last character and the last character if not empty
--
-- If empty, Nothing is returned
unsnoc :: String -> Maybe (String, Char)
unsnoc s@(String arr)
    | sz == 0   = Nothing
    | otherwise =
        let !(StepBack c idx) = prev s (sizeAsOffset sz)
         in Just (String $ Vec.take (offsetAsSize idx) arr, c)
  where
    sz = size s

-- | Extract the First character of a string, and the String stripped of the first character.
--
-- If empty, Nothing is returned
uncons :: String -> Maybe (Char, String)
uncons s@(String ba)
    | null s    = Nothing
    | otherwise =
        let !(Step c idx) = next s azero
         in Just (c, String $ Vec.drop (offsetAsSize idx) ba)

-- | Look for a predicate in the String and return the matched character, if any.
find :: (Char -> Bool) -> String -> Maybe Char
find predicate s = loop (Offset 0)
  where
    !sz = size s
    end = Offset 0 `offsetPlusE` sz
    loop idx
        | idx == end = Nothing
        | otherwise =
            let !(Step c idx') = next s idx
             in case predicate c of
                    True  -> Just c
                    False -> loop idx'

-- | Sort the character in a String using a specific sort function
--
-- TODO: optimise not going through a list
sortBy :: (Char -> Char -> Ordering) -> String -> String
sortBy sortF s = fromList $ Data.List.sortBy sortF $ toList s -- FIXME for tests

-- | Filter characters of a string using the predicate
filter :: (Char -> Bool) -> String -> String
filter predicate (String arr) = runST $ do
    (finalSize, dst) <- newNative sz $ \(MutableBlock mba) ->
        C.onBackendPrim (\ba@(Block !_) -> Alg.copyFilter predicate sz mba ba start)
                        (\fptr -> withFinalPtr fptr $ \ptr@(Ptr !_) -> Alg.copyFilter predicate sz mba ptr start)
                        arr
    freezeShrink finalSize dst
  where
    !sz    = C.length arr
    !start = C.offset arr

-- | Reverse a string
reverse :: String -> String
reverse (String arr) = runST $ do
    s <- newNative_ (C.length arr) $ \(MutableBlock mba) ->
            C.onBackendPrim
                (\ba@(Block !_) -> UTF8.reverse mba 0 ba start end)
                (\fptr -> withFinalPtr fptr $ \ptr@(Ptr !_) -> UTF8.reverse mba 0 ptr start end)
                arr
    freeze s
  where
    !(C.ValidRange start end) = C.offsetsValidRange arr

-- | Finds where are the insertion points when we search for a `needle`
-- within an `haystack`.
indices :: String -> String -> [Offset8]
indices (String ned) (String hy) = Vec.indices ned hy

-- | Replace all the occurrencies of `needle` with `replacement` in
-- the `haystack` string.
replace :: String -> String -> String -> String
replace (String needle) (String replacement) (String haystack) =
  String $ Vec.replace needle replacement haystack

-- | Return the nth character in a String
--
-- Compared to an array, the string need to be scanned from the beginning
-- since the UTF8 encoding is variable.
index :: String -> Offset Char -> Maybe Char
index s n
    | ofs >= end = Nothing
    | otherwise  =
        let (Step !c _) = next s ofs
         in Just c
  where
    !nbBytes = size s
    end = 0 `offsetPlusE` nbBytes
    ofs = indexN (offsetAsSize n) s

-- | Return the index in unit of Char of the first occurence of the predicate returning True
--
-- If not found, Nothing is returned
findIndex :: (Char -> Bool) -> String -> Maybe (Offset Char)
findIndex predicate s = loop 0 0
  where
    !sz = size s
    loop ofs idx
        | idx .==# sz = Nothing
        | otherwise   =
            let !(Step c idx') = next s idx
             in case predicate c of
                    True  -> Just ofs
                    False -> loop (ofs+1) idx'

-- | Various String Encoding that can be use to convert to and from bytes
data Encoding
    = ASCII7
    | UTF8
    | UTF16
    | UTF32
    | ISO_8859_1
    deriving (Typeable, Data, Eq, Ord, Show, Enum, Bounded)

fromEncoderBytes :: ( Encoder.Encoding encoding
                    , PrimType (Encoder.Unit encoding)
                    )
                 => encoding
                 -> UArray Word8
                 -> (String, Maybe ValidationFailure, UArray Word8)
fromEncoderBytes enc bytes =
    case runST $ Encoder.convertFromTo enc EncoderUTF8 (Vec.recast bytes) of
        -- TODO: Don't swallow up specific error (second element of pair)
        -- TODO: Confused why all this recasting is necessary. I "typed hole"-ed my way to get this function to compile.  Feels like there should be a cleaner method.
        Left (off, _) ->
            let (b1, b2) = Vec.splitAt (offsetAsSize off) (Vec.recast bytes)
            in (String $ Vec.recast b1, Just BuildingFailure, Vec.recast b2)
        Right converted -> (String converted, Nothing, mempty)

-- | Convert a ByteArray to a string assuming a specific encoding.
--
-- It returns a 3-tuple of:
--
-- * The string that has been succesfully converted without any error
-- * An optional validation error
-- * The remaining buffer that hasn't been processed (either as a result of an error, or because the encoded sequence is not fully available)
--
-- Considering a stream of data that is fetched chunk by chunk, it's valid to assume
-- that some sequence might fall in a chunk boundary. When converting chunks,
-- if the error is Nothing and the remaining buffer is not empty, then this buffer
-- need to be prepended to the next chunk
fromBytes :: Encoding -> UArray Word8 -> (String, Maybe ValidationFailure, UArray Word8)
fromBytes ASCII7     bytes = fromEncoderBytes Encoder.ASCII7     bytes
fromBytes ISO_8859_1 bytes = fromEncoderBytes Encoder.ISO_8859_1 bytes
fromBytes UTF16      bytes = fromEncoderBytes Encoder.UTF16      bytes
fromBytes UTF32      bytes = fromEncoderBytes Encoder.UTF32      bytes
fromBytes UTF8       bytes
    | C.null bytes = (mempty, Nothing, mempty)
    | otherwise    =
        case validate bytes (Offset 0) (C.length bytes) of
            (_, Nothing)  -> (fromBytesUnsafe bytes, Nothing, mempty)
            (pos, Just vf) ->
                let (b1, b2) = C.splitAt (offsetAsSize pos) bytes
                 in (fromBytesUnsafe b1, toErr vf, b2)
  where
    toErr MissingByte         = Nothing
    toErr InvalidHeader       = Just InvalidHeader
    toErr InvalidContinuation = Just InvalidContinuation
    toErr BuildingFailure     = Just BuildingFailure

-- | Convert a UTF8 array of bytes to a String.
--
-- If there's any error in the stream, it will automatically
-- insert replacement bytes to replace invalid sequences.
--
-- In the case of sequence that fall in the middle of 2 chunks,
-- the remaining buffer is supposed to be preprended to the
-- next chunk, and resume the parsing.
fromBytesLenient :: UArray Word8 -> (String, UArray Word8)
fromBytesLenient bytes
    | C.null bytes = (mempty, mempty)
    | otherwise    =
        case validate bytes (Offset 0) (C.length bytes) of
            (_, Nothing)                   -> (fromBytesUnsafe bytes, mempty)
            -- TODO: Should anything be done in the 'BuildingFailure' case?
            (_, Just BuildingFailure) -> error "fromBytesLenient: FIXME!"
            (pos, Just MissingByte) ->
                let (b1,b2) = C.splitAt (offsetAsSize pos) bytes
                 in (fromBytesUnsafe b1, b2)
            (pos, Just InvalidHeader) ->
                let (b1,b2) = C.splitAt (offsetAsSize pos) bytes
                    (_,b3)  = C.splitAt 1 b2
                    (s3, r) = fromBytesLenient b3
                 in (mconcat [fromBytesUnsafe b1,replacement, s3], r)
            (pos, Just InvalidContinuation) ->
                let (b1,b2) = C.splitAt (offsetAsSize pos) bytes
                    (_,b3)  = C.splitAt 1 b2
                    (s3, r) = fromBytesLenient b3
                 in (mconcat [fromBytesUnsafe b1,replacement, s3], r)
  where
    -- This is the replacement character U+FFFD used for any invalid header or continuation
    replacement :: String
    !replacement = fromBytesUnsafe $ fromList [0xef,0xbf,0xbd]

-- | Decode a stream of binary chunks containing UTF8 encoding in a list of valid String
--
-- Chunk not necessarily contains a valid string, as
-- a UTF8 sequence could be split over 2 chunks.
fromChunkBytes :: [UArray Word8] -> [String]
fromChunkBytes l = loop l
  where
    loop []         = []
    loop [bytes]    =
        case validate bytes (Offset 0) (C.length bytes) of
            (_, Nothing)  -> [fromBytesUnsafe bytes]
            (_, Just err) -> doErr err
    loop (bytes:cs@(c1:c2)) =
        case validate bytes (Offset 0) (C.length bytes) of
            (_, Nothing) -> fromBytesUnsafe bytes : loop cs
            (pos, Just MissingByte) ->
                let (b1,b2) = C.splitAt (offsetAsSize pos) bytes
                 in fromBytesUnsafe b1 : loop ((b2 `mappend` c1) : c2)
            (_, Just err) -> doErr err
    doErr err = error ("fromChunkBytes: " <> show err)

-- | Convert a Byte Array representing UTF8 data directly to a string without checking for UTF8 validity
--
-- If the input contains invalid sequences, it will trigger runtime async errors when processing data.
--
-- In doubt, use 'fromBytes'
fromBytesUnsafe :: UArray Word8 -> String
fromBytesUnsafe = String

toEncoderBytes :: ( Encoder.Encoding encoding
                  , PrimType (Encoder.Unit encoding)
                  , Exception (Encoder.Error encoding)
                  )
               => encoding
               -> UArray Word8
               -> UArray Word8
toEncoderBytes enc bytes = Vec.recast $
  case runST $ Encoder.convertFromTo EncoderUTF8 enc bytes of
    Left _ -> error "toEncoderBytes: FIXME!"
    Right converted -> converted

-- | Convert a String to a bytearray in a specific encoding
--
-- if the encoding is UTF8, the underlying buffer is returned without extra allocation or any processing
--
-- In any other encoding, some allocation and processing are done to convert.
toBytes :: Encoding -> String -> UArray Word8
toBytes UTF8       (String bytes) = bytes
toBytes ASCII7     (String bytes) = toEncoderBytes Encoder.ASCII7     bytes
toBytes ISO_8859_1 (String bytes) = toEncoderBytes Encoder.ISO_8859_1 bytes
toBytes UTF16      (String bytes) = toEncoderBytes Encoder.UTF16      bytes
toBytes UTF32      (String bytes) = toEncoderBytes Encoder.UTF32      bytes

-- | Split lines in a string using newline as separation.
--
-- Note that carriage return preceding a newline are also strip for
-- maximum compatibility between Windows and Unix system.
lines :: String -> [String]
lines s =
    case breakLine s of
        Left _         -> [s]
        Right (line,r) -> line : lines r

-- | Split words in a string using spaces as separation
--
-- > words "Hello Foundation"
-- [ "Hello", "Foundation" ]
words :: String -> [String]
words = fmap fromList . Prelude.words . toList

-- | Append a character to a String builder
builderAppend :: PrimMonad state => Char -> Builder String MutableString Word8 state err ()
builderAppend c = Builder $ State $ \(i, st, e) ->
    if offsetAsSize i + nbBytes >= chunkSize st
        then do
            cur      <- unsafeFreezeShrink (curChunk st) (offsetAsSize i)
            newChunk <- new (chunkSize st)
            writeUTF8Char newChunk (Offset 0) utf8Char
            return ((), (sizeAsOffset nbBytes, st { prevChunks     = cur : prevChunks st
                                                  , prevChunksSize = offsetAsSize i + prevChunksSize st
                                                  , curChunk       = newChunk
                                                  }, e))
        else do
            writeUTF8Char (curChunk st) i utf8Char
            return ((), (i + sizeAsOffset nbBytes, st, e))
  where
    utf8Char = asUTF8Char c
    nbBytes  = numBytes utf8Char

-- | Create a new String builder using chunks of @sizeChunksI@
builderBuild :: PrimMonad m => Int -> Builder String MutableString Word8 m err () -> m (Either err String)
builderBuild sizeChunksI sb
    | sizeChunksI <= 3 = builderBuild 64 sb
    | otherwise        = do
        firstChunk <- new sizeChunks
        (i, st, e) <- snd <$> runState (runBuilder sb) (Offset 0, BuildingState [] (CountOf 0) firstChunk sizeChunks, Nothing)
        case e of
          Just err -> return (Left err)
          Nothing -> do
            cur <- unsafeFreezeShrink (curChunk st) (offsetAsSize i)
            -- Build final array
            let totalSize = prevChunksSize st + offsetAsSize i
            final <- Vec.new totalSize >>= fillFromEnd totalSize (cur : prevChunks st) >>= Vec.unsafeFreeze
            return . Right . String $ final
  where
    sizeChunks = CountOf sizeChunksI

    fillFromEnd _    []            mba = return mba
    fillFromEnd !end (String x:xs) mba = do
        let sz = Vec.length x
        let start = end `sizeSub` sz
        Vec.unsafeCopyAtRO mba (sizeAsOffset start) x (Offset 0) sz
        fillFromEnd start xs mba

builderBuild_ :: PrimMonad m => Int -> Builder String MutableString Word8 m () () -> m String
builderBuild_ sizeChunksI sb = either (\() -> internalError "impossible output") id <$> builderBuild sizeChunksI sb

stringDewrap :: (Block Word8 -> Offset Word8 -> a)
             -> (Ptr Word8 -> Offset Word8 -> ST s a)
             -> String
             -> a
stringDewrap withBa withPtr (String ba) = C.unsafeDewrap withBa withPtr ba
{-# INLINE stringDewrap #-}

-- | Read an Integer from a String
--
-- Consume an optional minus sign and many digits until end of string.
readIntegral :: (HasNegation i, IntegralUpsize Word8 i, Additive i, Multiplicative i, IsIntegral i) => String -> Maybe i
readIntegral str
    | sz == 0   = Nothing
    | otherwise = stringDewrap withBa (\ptr@(Ptr !_) -> pure . withPtr ptr) str
  where
    !sz = size str
    withBa ba ofs =
        let negativeSign = UTF8.expectAscii ba ofs 0x2d
            startOfs     = if negativeSign then succ ofs else ofs
         in case decimalDigitsBA 0 ba endOfs startOfs of
                (# acc, True, endOfs' #) | endOfs' > startOfs -> Just $! if negativeSign then negate acc else acc
                _                                             -> Nothing
      where !endOfs = ofs `offsetPlusE` sz
    withPtr addr ofs =
        let negativeSign = UTF8.expectAscii addr ofs 0x2d
            startOfs     = if negativeSign then succ ofs else ofs
         in case decimalDigitsPtr 0 addr endOfs startOfs of
                (# acc, True, endOfs' #) | endOfs' > startOfs -> Just $! if negativeSign then negate acc else acc
                _                                             -> Nothing
      where !endOfs = ofs `offsetPlusE` sz
{-# SPECIALISE readIntegral :: String -> Maybe Integer #-}
{-# SPECIALISE readIntegral :: String -> Maybe Int #-}

readInteger :: String -> Maybe Integer
readInteger = readIntegral

-- | Read a Natural from a String
--
-- Consume many digits until end of string.
readNatural :: String -> Maybe Natural
readNatural str
    | sz == 0   = Nothing
    | otherwise = stringDewrap withBa (\ptr@(Ptr !_) -> pure . withPtr ptr) str
  where
    !sz = size str
    withBa ba stringStart =
        case decimalDigitsBA 0 ba eofs stringStart of
            (# acc, True, endOfs #) | endOfs > stringStart -> Just acc
            _                                              -> Nothing
      where eofs = stringStart `offsetPlusE` sz
    withPtr addr stringStart =
        case decimalDigitsPtr 0 addr eofs stringStart of
            (# acc, True, endOfs #) | endOfs > stringStart -> Just acc
            _                                              -> Nothing
      where eofs = stringStart `offsetPlusE` sz

-- | Try to read a Double
readDouble :: String -> Maybe Double
readDouble s =
    readFloatingExact s $ \isNegative integral floatingDigits mExponant ->
        Just $ applySign isNegative $ case (floatingDigits, mExponant) of
            (0, Nothing)              ->                         naturalToDouble integral
            (0, Just exponent)        -> withExponant exponent $ naturalToDouble integral
            (floating, Nothing)       ->                         applyFloating floating $ naturalToDouble integral
            (floating, Just exponent) -> withExponant exponent $ applyFloating floating $ naturalToDouble integral
  where
    applySign True = negate
    applySign False = id
    withExponant e v = v * doubleExponant 10 e
    applyFloating digits n = n / (10 Prelude.^ digits)

-- | Try to read a floating number as a Rational
--
-- Note that for safety reason, only exponent between -10000 and 10000 is allowed
-- as otherwise DoS/OOM is very likely. if you don't want this behavior,
-- switching to a scientific type (not provided yet) that represent the
-- exponent separately is the advised solution.
readRational :: String -> Maybe Prelude.Rational
readRational s =
    readFloatingExact s $ \isNegative integral floatingDigits mExponant ->
        case mExponant of
            Just exponent
                | exponent < -10000 || exponent > 10000 -> Nothing
                | otherwise                             -> Just $ modF isNegative integral % (10 Prelude.^ (cast floatingDigits - exponent))
            Nothing                                     -> Just $ modF isNegative integral % (10 Prelude.^ floatingDigits)
  where
    modF True  = negate . integralUpsize
    modF False = integralUpsize


type ReadFloatingCallback a = Bool      -- sign
                           -> Natural   -- integral part
                           -> Word      -- number of digits in floating section
                           -> Maybe Int -- optional integer representing exponent in base 10
                           -> Maybe a

-- | Read an Floating like number of the form:
--
--   [ '-' ] <numbers> [ '.' <numbers> ] [ ( 'e' | 'E' ) [ '-' ] <number> ]
--
-- Call a function with:
--
-- * A boolean representing if the number is negative
-- * The digits part represented as a single natural number (123.456 is represented as 123456)
-- * The number of digits in the fractional part (e.g. 123.456 => 3)
-- * The exponent if any
--
-- The code is structured as a simple state machine that:
--
-- * Optionally Consume a '-' sign
-- * Consume number for the integral part
-- * Optionally
--   * Consume '.'
--   * Consume remaining digits if not already end of string
-- * Optionally Consume a 'e' or 'E' follow by an optional '-' and a number
--
readFloatingExact :: String -> ReadFloatingCallback a -> Maybe a
readFloatingExact str f
    | sz == 0   = Nothing
    | otherwise = stringDewrap withBa withPtr str
  where
    !sz = size str

    withBa ba stringStart =
        let !isNegative = UTF8.expectAscii ba stringStart 0x2d
         in consumeIntegral isNegative (if isNegative then stringStart+1 else stringStart)
      where
        eofs = stringStart `offsetPlusE` sz
        consumeIntegral !isNegative startOfs =
            case decimalDigitsBA 0 ba eofs startOfs of
                (# acc, True , endOfs #) | endOfs > startOfs -> f isNegative acc 0 Nothing -- end of stream and no '.'
                (# acc, False, endOfs #) | endOfs > startOfs ->
                    if UTF8.expectAscii ba endOfs 0x2e
                        then consumeFloat isNegative acc (endOfs + 1)
                        else consumeExponant isNegative acc 0 endOfs
                _                                            -> Nothing

        consumeFloat isNegative integral startOfs =
            case decimalDigitsBA integral ba eofs startOfs of
                (# acc, True, endOfs #) | endOfs > startOfs -> let (CountOf !diff) = endOfs - startOfs
                                                                in f isNegative acc (cast diff) Nothing
                (# acc, False, endOfs #) | endOfs > startOfs -> let (CountOf !diff) = endOfs - startOfs
                                                                in consumeExponant isNegative acc (cast diff) endOfs
                _                                           -> Nothing

        consumeExponant !isNegative !integral !floatingDigits !startOfs
            | startOfs == eofs = f isNegative integral floatingDigits Nothing
            | otherwise        =
                -- consume 'E' or 'e'
                case UTF8.nextAscii ba startOfs of
                    StepASCII 0x45 -> consumeExponantSign (startOfs+1)
                    StepASCII 0x65 -> consumeExponantSign (startOfs+1)
                    _              -> Nothing
          where
            consumeExponantSign ofs
                | ofs == eofs = Nothing
                | otherwise   = let exponentNegative = UTF8.expectAscii ba ofs 0x2d
                                 in consumeExponantNumber exponentNegative (if exponentNegative then ofs + 1 else ofs)

            consumeExponantNumber exponentNegative ofs =
                case decimalDigitsBA 0 ba eofs ofs of
                    (# acc, True, endOfs #) | endOfs > ofs -> f isNegative integral floatingDigits (Just $! if exponentNegative then negate acc else acc)
                    _                                      -> Nothing
    withPtr ptr@(Ptr !_) stringStart = pure $
        let !isNegative = UTF8.expectAscii ptr stringStart 0x2d
         in consumeIntegral isNegative (if isNegative then stringStart+1 else stringStart)
      where
        eofs = stringStart `offsetPlusE` sz
        consumeIntegral !isNegative startOfs =
            case decimalDigitsPtr 0 ptr eofs startOfs of
                (# acc, True , endOfs #) | endOfs > startOfs -> f isNegative acc 0 Nothing -- end of stream and no '.'
                (# acc, False, endOfs #) | endOfs > startOfs ->
                    if UTF8.expectAscii ptr endOfs 0x2e
                        then consumeFloat isNegative acc (endOfs + 1)
                        else consumeExponant isNegative acc 0 endOfs
                _                                            -> Nothing

        consumeFloat isNegative integral startOfs =
            case decimalDigitsPtr integral ptr eofs startOfs of
                (# acc, True, endOfs #) | endOfs > startOfs -> let (CountOf !diff) = endOfs - startOfs
                                                                in f isNegative acc (cast diff) Nothing
                (# acc, False, endOfs #) | endOfs > startOfs -> let (CountOf !diff) = endOfs - startOfs
                                                                in consumeExponant isNegative acc (cast diff) endOfs
                _                                           -> Nothing

        consumeExponant !isNegative !integral !floatingDigits !startOfs
            | startOfs == eofs = f isNegative integral floatingDigits Nothing
            | otherwise        =
                -- consume 'E' or 'e'
                case UTF8.nextAscii ptr startOfs of
                    StepASCII 0x45 -> consumeExponantSign (startOfs+1)
                    StepASCII 0x65 -> consumeExponantSign (startOfs+1)
                    _              -> Nothing
          where
            consumeExponantSign ofs
                | ofs == eofs = Nothing
                | otherwise   = let exponentNegative = UTF8.expectAscii ptr ofs 0x2d
                                 in consumeExponantNumber exponentNegative (if exponentNegative then ofs + 1 else ofs)

            consumeExponantNumber exponentNegative ofs =
                case decimalDigitsPtr 0 ptr eofs ofs of
                    (# acc, True, endOfs #) | endOfs > ofs -> f isNegative integral floatingDigits (Just $! if exponentNegative then negate acc else acc)
                    _                                      -> Nothing

-- | Take decimal digits and accumulate it in `acc`
--
-- The loop starts at the offset specified and finish either when:
--
-- * It reach the end of the string
-- * It reach a non-ASCII character
-- * It reach an ASCII character that is not a digit (0 to 9)
--
-- Otherwise each iterations:
--
-- * Transform the ASCII digits into a number
-- * scale the accumulator by 10
-- * Add the number (between 0 and 9) to the accumulator
--
-- It then returns:
--
-- * The new accumulated value
-- * Whether it stop by end of string or not
-- * The end offset when the loop stopped
--
-- If end offset == start offset then no digits have been consumed by
-- this function
decimalDigitsBA :: (IntegralUpsize Word8 acc, Additive acc, Multiplicative acc, Integral acc)
                => acc
                -> Block Word8
                -> Offset Word8 -- end offset
                -> Offset Word8 -- start offset
                -> (# acc, Bool, Offset Word8 #)
decimalDigitsBA startAcc ba !endOfs !startOfs = loop startAcc startOfs
  where
    loop !acc !ofs
        | ofs == endOfs = (# acc, True, ofs #)
        | otherwise     =
            case UTF8.nextAsciiDigit ba ofs of
                sg@(StepDigit d) | isValidStepDigit sg -> loop (10 * acc + integralUpsize d) (succ ofs)
                                 | otherwise           -> (# acc, False, ofs #)
{-# SPECIALIZE decimalDigitsBA :: Integer -> Block Word8 -> Offset Word8 -> Offset Word8 -> (# Integer, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsBA :: Natural -> Block Word8 -> Offset Word8 -> Offset Word8 -> (# Natural, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsBA :: Int -> Block Word8 -> Offset Word8 -> Offset Word8 -> (# Int, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsBA :: Word -> Block Word8 -> Offset Word8 -> Offset Word8 -> (# Word, Bool, Offset Word8 #) #-}

-- | same as decimalDigitsBA specialized for ptr #
decimalDigitsPtr :: (IntegralUpsize Word8 acc, Additive acc, Multiplicative acc, Integral acc)
                 => acc
                 -> Ptr Word8
                 -> Offset Word8 -- end offset
                 -> Offset Word8 -- start offset
                 -> (# acc, Bool, Offset Word8 #)
decimalDigitsPtr startAcc ptr !endOfs !startOfs = loop startAcc startOfs
  where
    loop !acc !ofs
        | ofs == endOfs = (# acc, True, ofs #)
        | otherwise     =
            case UTF8.nextAsciiDigit ptr ofs of
                sg@(StepDigit d) | isValidStepDigit sg -> loop (10 * acc + integralUpsize d) (succ ofs)
                                 | otherwise           -> (# acc, False, ofs #)
{-# SPECIALIZE decimalDigitsPtr :: Integer -> Ptr Word8 -> Offset Word8 -> Offset Word8 -> (# Integer, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsPtr :: Natural -> Ptr Word8 -> Offset Word8 -> Offset Word8 -> (# Natural, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsPtr :: Int -> Ptr Word8 -> Offset Word8 -> Offset Word8 -> (# Int, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsPtr :: Word -> Ptr Word8 -> Offset Word8 -> Offset Word8 -> (# Word, Bool, Offset Word8 #) #-}

-- | Convert a 'String' 'Char' by 'Char' using a case mapping function.
caseConvert :: (Char7 -> Char7) -> (Char -> CM) -> String -> String
caseConvert opASCII op s@(String arr) = runST $ do
  mba <- MBLK.new iLen
  nL <- C.onBackendPrim
        (\blk  -> go mba blk (Offset 0) start)
        (\fptr -> withFinalPtr fptr $ \ptr -> go mba ptr (Offset 0) start)
        arr
  freeze . MutableString $ MVec.MUArray 0 nL (C.MUArrayMBA mba)
  where
    !(C.ValidRange start end) = C.offsetsValidRange arr
    !iLen = 1 + C.length arr
    go :: (Indexable container Word8, PrimMonad prim)
       => MutableBlock Word8 (PrimState prim)
       -> container
       -> Offset Word8
       -> Offset Word8
       -> prim (CountOf Word8)
    go !dst !src = loop dst iLen 0
      where
        eSize !e = if e == '\0' then 0 else charToBytes (fromEnum e)
        loop !dst !allocLen !nLen !dstIdx !srcIdx
          | srcIdx == end    = return nLen
          | nLen == allocLen = realloc
          | headerIsAscii h  = do
                UTF8.writeASCII dst dstIdx (opASCII $ Char7 $ stepAsciiRawValue h)
                loop dst allocLen (nLen + 1) (dstIdx+Offset 1) (srcIdx+Offset 1)
          | otherwise = do
              let !(CM c1 c2 c3) = op c
                  !(Step c nextSrcIdx) = UTF8.nextWith h src (srcIdx+Offset 1)
              nextDstIdx <- UTF8.writeUTF8 dst dstIdx c1
              if c2 == '\0' -- We keep the most common case loop as short as possible.
                then loop dst allocLen (nLen + charToBytes (fromEnum c1)) nextDstIdx nextSrcIdx
                else do
                  let !cSize = eSize c1 + eSize c2 + eSize c3
                  nextDstIdx <- UTF8.writeUTF8 dst nextDstIdx c2
                  nextDstIdx <- if c3 == '\0' then return nextDstIdx else UTF8.writeUTF8 dst nextDstIdx c3
                  loop dst allocLen (nLen + cSize) nextDstIdx nextSrcIdx
          where
            {-# NOINLINE realloc #-}
            realloc = do
              let nAll = allocLen + allocLen + 1
              nDst <- MBLK.new nAll
              MBLK.unsafeCopyElements nDst 0 dst 0 nLen
              loop nDst nAll nLen dstIdx srcIdx
            h = UTF8.nextAscii src srcIdx

-- | Convert a 'String' to the upper-case equivalent.
upper :: String -> String
upper = caseConvert c7Upper upperMapping

-- | Convert a 'String' to the upper-case equivalent.
lower :: String -> String
lower = caseConvert c7Lower lowerMapping

-- | Convert a 'String' to the unicode case fold equivalent.
--
-- Case folding is mostly used for caseless comparison of strings.
caseFold :: String -> String
caseFold = caseConvert c7Upper foldMapping

-- | Check whether the first string is a prefix of the second string.
isPrefixOf :: String -> String -> Bool
isPrefixOf (String needle) (String haystack) = C.isPrefixOf needle haystack

-- | Check whether the first string is a suffix of the second string.
isSuffixOf :: String -> String -> Bool
isSuffixOf (String needle) (String haystack)
    | needleLen > hayLen = False
    | otherwise          = needle == C.revTake needleLen haystack
  where
    needleLen = C.length needle
    hayLen    = C.length haystack

-- | Check whether the first string is contains within the second string.
--
-- TODO: implemented the naive way and thus terribly inefficient, reimplement properly
isInfixOf :: String -> String -> Bool
isInfixOf (String needle) (String haystack)
    = loop (hayLen - needleLen) haystack
    where
      needleLen = C.length needle
      hayLen    = C.length haystack
      loop Nothing    _         = False
      loop (Just cnt) haystack' = needle == C.take needleLen haystack' || loop (cnt-1) (C.drop 1 haystack')

-- | Try to strip a prefix from the start of a String.
--
-- If the prefix is not starting the string, then Nothing is returned,
-- otherwise the striped string is returned
stripPrefix :: String -> String -> Maybe String
stripPrefix (String suffix) (String arr)
    | C.isPrefixOf suffix arr = Just $ String $ C.drop (C.length suffix) arr
    | otherwise               = Nothing

-- | Try to strip a suffix from the end of a String.
--
-- If the suffix is not ending the string, then Nothing is returned,
-- otherwise the striped string is returned
stripSuffix :: String -> String -> Maybe String
stripSuffix (String prefix) (String arr)
    | C.isSuffixOf prefix arr = Just $ String $ C.revDrop (C.length prefix) arr
    | otherwise               = Nothing

all :: (Char -> Bool) -> String -> Bool
all predicate (String arr) = C.onBackend goBA (\_ -> pure . goAddr) arr
  where
    !(C.ValidRange start end) = C.offsetsValidRange arr
    goBA ba   = UTF8.all predicate ba start end
    goAddr addr = UTF8.all predicate addr start end

any :: (Char -> Bool) -> String -> Bool
any predicate (String arr) = C.onBackend goBA (\_ -> pure . goAddr) arr
  where
    !(C.ValidRange start end) = C.offsetsValidRange arr
    goBA ba   = UTF8.any predicate ba start end
    goAddr addr = UTF8.any predicate addr start end

-- | Transform string @src@ to base64 binary representation.
toBase64 :: String -> String
toBase64 (String src) = fromBytesUnsafe . Vec.toBase64Internal set src $ True
  where
    !set = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#

-- | Transform string @src@ to URL-safe base64 binary representation.
-- The result will be either padded or unpadded, depending on the boolean
-- @padded@ argument.
toBase64URL :: Bool -> String -> String
toBase64URL padded (String src) = fromBytesUnsafe . Vec.toBase64Internal set src $ padded
  where
    !set = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"#

-- | Transform string @src@ to OpenBSD base64 binary representation.
toBase64OpenBSD :: String -> String
toBase64OpenBSD (String src) = fromBytesUnsafe . Vec.toBase64Internal set src $ False
  where
    !set = "./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"#
