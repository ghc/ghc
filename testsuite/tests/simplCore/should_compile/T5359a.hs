{-# LANGUAGE BangPatterns, RankNTypes, MagicHash, UnboxedTuples #-}

module T5359a (linesT) where

import GHC.Base
import GHC.Word
import GHC.ST (ST(..), runST)

nullT :: Text -> Bool
nullT (Text _ _ len) = len <= 0
{-# INLINE [1] nullT #-}

spanT :: (Char -> Bool) -> Text -> (Text, Text)
spanT p t@(Text arr off len) = (textP arr off k, textP arr (off+k) (len-k))
  where k = loop 0
        loop !i | i >= len || not (p c) = i
                | otherwise             = loop (i+d)
            where Iter c d              = iter t i
{-# INLINE spanT #-}

linesT :: Text -> [Text]
linesT ps | nullT ps  = []
          | otherwise = h : if nullT t
                            then []
                            else linesT (unsafeTail t)
    where (h,t) = spanT (/= '\n') ps
{-# INLINE linesT #-}

unsafeTail :: Text -> Text
unsafeTail t@(Text arr off len) = Text arr (off+d) (len-d)
  where d = iter_ t 0
{-# INLINE unsafeTail #-}

data Iter = Iter {-# UNPACK #-} !Char {-# UNPACK #-} !Int

iter :: Text -> Int -> Iter
iter (Text arr _ _) i = Iter (unsafeChrT m) 1
  where m = unsafeIndex arr i
{-# INLINE iter #-}

iter_ :: Text -> Int -> Int
iter_ (Text arr off _) i | m < 0xD800 || m > 0xDBFF = 1
                         | otherwise                = 2
  where m = unsafeIndex arr (off+i)
{-# INLINE iter_ #-}

data Text = Text {-# UNPACK #-}!Array {-# UNPACK #-}!Int {-# UNPACK #-}!Int

text :: Array -> Int -> Int -> Text
text arr off len = Text arr off len
{-# INLINE text #-}

emptyT :: Text
emptyT = Text empty 0 0
{-# INLINE [1] emptyT #-}

textP :: Array -> Int -> Int -> Text
textP arr off len | len == 0  = emptyT
                  | otherwise = text arr off len
{-# INLINE textP #-}

unsafeChrT :: Word16 -> Char
unsafeChrT (W16# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChrT #-}

data Array = Array ByteArray#

data MArray s = MArray (MutableByteArray# s)

new :: forall s. Int -> ST s (MArray s)
new n@(I# len#)
  | n < 0 || n /= 0 = error $ "Data.Text.Array.new: size overflow"
  | otherwise = ST $ \s1# ->
       case newByteArray# len# s1# of
         (# s2#, marr# #) -> (# s2#, MArray marr# #)
{-# INLINE new #-}

unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze (MArray maBA) = ST $ \s# -> (# s#, Array (unsafeCoerce# maBA) #)
{-# INLINE unsafeFreeze #-}

unsafeIndex :: Array -> Int -> Word16
unsafeIndex (Array aBA) (I# i#) =
    case indexWord16Array# aBA i# of r# -> (W16# r#)
{-# INLINE unsafeIndex #-}

empty :: Array
empty = runST (new 0 >>= unsafeFreeze)
