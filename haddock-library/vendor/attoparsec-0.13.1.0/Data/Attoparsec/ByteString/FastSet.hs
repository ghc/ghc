{-# LANGUAGE BangPatterns, MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.ByteString.FastSet
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Fast set membership tests for 'Word8' and 8-bit 'Char' values.  The
-- set representation is unboxed for efficiency.  For small sets, we
-- test for membership using a binary search.  For larger sets, we use
-- a lookup table.
--
-----------------------------------------------------------------------------
module Data.Attoparsec.ByteString.FastSet
    (
    -- * Data type
      FastSet
    -- * Construction
    , fromList
    , set
    -- * Lookup
    , memberChar
    , memberWord8
    -- * Debugging
    , fromSet
    -- * Handy interface
    , charClass
    ) where

import Data.Bits ((.&.), (.|.))
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Base (Int(I#), iShiftRA#, narrow8Word#, shiftL#)
import GHC.Word (Word8(W8#))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe as U

data FastSet = Sorted { fromSet :: !B.ByteString }
             | Table  { fromSet :: !B.ByteString }
    deriving (Eq, Ord)

instance Show FastSet where
    show (Sorted s) = "FastSet Sorted " ++ show (B8.unpack s)
    show (Table _) = "FastSet Table"

-- | The lower bound on the size of a lookup table.  We choose this to
-- balance table density against performance.
tableCutoff :: Int
tableCutoff = 8

-- | Create a set.
set :: B.ByteString -> FastSet
set s | B.length s < tableCutoff = Sorted . B.sort $ s
      | otherwise                = Table . mkTable $ s

fromList :: [Word8] -> FastSet
fromList = set . B.pack

data I = I {-# UNPACK #-} !Int {-# UNPACK #-} !Word8

shiftR :: Int -> Int -> Int
shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)

shiftL :: Word8 -> Int -> Word8
shiftL (W8# x#) (I# i#) = W8# (narrow8Word# (x# `shiftL#` i#))

index :: Int -> I
index i = I (i `shiftR` 3) (1 `shiftL` (i .&. 7))
{-# INLINE index #-}

-- | Check the set for membership.
memberWord8 :: Word8 -> FastSet -> Bool
memberWord8 w (Table t)  =
    let I byte bit = index (fromIntegral w)
    in  U.unsafeIndex t byte .&. bit /= 0
memberWord8 w (Sorted s) = search 0 (B.length s - 1)
    where search lo hi
              | hi < lo = False
              | otherwise =
                  let mid = (lo + hi) `quot` 2
                  in case compare w (U.unsafeIndex s mid) of
                       GT -> search (mid + 1) hi
                       LT -> search lo (mid - 1)
                       _ -> True

-- | Check the set for membership.  Only works with 8-bit characters:
-- characters above code point 255 will give wrong answers.
memberChar :: Char -> FastSet -> Bool
memberChar c = memberWord8 (I.c2w c)
{-# INLINE memberChar #-}

mkTable :: B.ByteString -> B.ByteString
mkTable s = I.unsafeCreate 32 $ \t -> do
            _ <- I.memset t 0 32
            U.unsafeUseAsCStringLen s $ \(p, l) ->
              let loop n | n == l = return ()
                         | otherwise = do
                    c <- peekByteOff p n :: IO Word8
                    let I byte bit = index (fromIntegral c)
                    prev <- peekByteOff t byte :: IO Word8
                    pokeByteOff t byte (prev .|. bit)
                    loop (n + 1)
              in loop 0

charClass :: String -> FastSet
charClass = set . B8.pack . go
    where go (a:'-':b:xs) = [a..b] ++ go xs
          go (x:xs) = x : go xs
          go _ = ""
