-- |
-- Module      : Data.ByteArray.Pack.Internal
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 Nicolas DI PRIMA
--
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ByteArray.Pack.Internal
    ( Result(..)
    , Packer(..)
    , actionPacker
    , actionPackerWithRemain
    ) where

import           Foreign.Ptr (Ptr)
import           Data.ByteArray.MemView
import           Data.Memory.Internal.Imports

-- | Packing result:
--
-- * PackerMore: the next state of Packing with an arbitrary value
-- * PackerFail: an error happened
data Result a =
      PackerMore a MemView
    | PackerFail String
    deriving (Show)

-- | Simple ByteArray Packer
newtype Packer a = Packer { runPacker_ :: MemView -> IO (Result a) }

instance Functor Packer where
    fmap = fmapPacker

instance Applicative Packer where
    pure  = returnPacker
    (<*>) = appendPacker

instance Monad Packer where
    return = returnPacker
    (>>=)  = bindPacker

fmapPacker :: (a -> b) -> Packer a -> Packer b
fmapPacker f p = Packer $ \cache -> do
    rv <- runPacker_ p cache
    return $ case rv of
        PackerMore v cache' -> PackerMore (f v) cache'
        PackerFail err      -> PackerFail err
{-# INLINE fmapPacker #-}

returnPacker :: a -> Packer a
returnPacker v = Packer $ \cache -> return $ PackerMore v cache
{-# INLINE returnPacker #-}

bindPacker :: Packer a -> (a -> Packer b) -> Packer b
bindPacker p fp = Packer $ \cache -> do
    rv <- runPacker_ p cache
    case rv of
        PackerMore v cache' -> runPacker_ (fp v) cache'
        PackerFail err      -> return $ PackerFail err
{-# INLINE bindPacker #-}

appendPacker :: Packer (a -> b) -> Packer a -> Packer b
appendPacker p1f p2 = p1f >>= \p1 -> p2 >>= \v -> return (p1 v)
{-# INLINE appendPacker #-}

-- | run a sized action
actionPacker :: Int -> (Ptr Word8 -> IO a) -> Packer a
actionPacker s action = Packer $ \m@(MemView ptr size) ->
    case compare size s of
        LT -> return $ PackerFail "Not enough space in destination"
        _  -> do
            v <- action ptr
            return $ PackerMore v (m `memViewPlus` s)
{-# INLINE actionPacker #-}

-- | run a sized action
actionPackerWithRemain :: Int -> (Ptr Word8 -> Int -> IO (Int, a)) -> Packer a
actionPackerWithRemain s action = Packer $ \m@(MemView ptr size) ->
    case compare size s of
        LT -> return $ PackerFail "Not enough space in destination"
        _  -> do
            (remain, v) <- action ptr size
            return $ if remain > s
                then PackerFail "remaining bytes higher than the destination's size"
                else PackerMore v (m `memViewPlus` (s+remain))
{-# INLINE actionPackerWithRemain #-}
