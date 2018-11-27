{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Main (enumFromCallbackCatch, consume, next, main) where

import Control.Monad
import Foreign
import GHC.ForeignPtr
import GHC.Base (realWorld#)
import Data.Word (Word8)
import Foreign.Storable (peek)
import GHC.IO

data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) {-# UNPACK #-} !Int

instance Show ByteString where
  showsPrec p ps r = showsPrec p (unpackAppendCharsStrict ps []) r

unpackAppendCharsStrict :: ByteString -> [Char] -> [Char]
unpackAppendCharsStrict (PS fp len) xs =
    unsafeDupablePerformIO $ withForeignPtr fp $ \base ->
      loop (base `plusPtr` (-1)) (base `plusPtr` 960) xs
  where
    loop !sentinal !p acc
      | p == sentinal = return acc
      | otherwise     = do x <- peek p
                           loop sentinal (p `plusPtr` (-1)) (w2c x:acc)

w2c :: Word8 -> Char
w2c = toEnum . fromEnum

packCStringLen :: Int -> IO ByteString
packCStringLen l = do
  p <- callocBytes bufsize
  fp <- newForeignPtr finalizerFree p
  return $! PS fp l
{-# NOINLINE packCStringLen #-}

bufsize :: Int
bufsize = 8192

{-# NOINLINE readFromPtr #-}
readFromPtr :: IO ByteString
readFromPtr = do
    bs <- packCStringLen bufsize
    length (show bs) `seq` return bs

newtype Iteratee s = Iteratee { runIter :: forall r.
          ((s -> Iteratee s) -> IO r) ->
          IO r}

enumFromCallbackCatch :: IO ()
enumFromCallbackCatch = produce 500 consume
  where
    produce 0 (Iteratee f) = return ()
    produce n (Iteratee f) = f onCont
      where onCont k = do bs <- readFromPtr; produce (n-1) (k bs)

consume = Iteratee $ \onCont -> onCont next
next x = Iteratee $ \onCont -> print x >> onCont (\_ -> consume)

main :: IO ()
main = do
  _ <- enumFromCallbackCatch
  pure ()
