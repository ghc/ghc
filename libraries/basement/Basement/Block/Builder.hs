-- |
-- Module      : Basement.Block.Builder
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- Block builder

{-# LANGUAGE Rank2Types #-}

module Basement.Block.Builder
    ( Builder
    , run

    -- * Emit functions
    , emit
    , emitPrim
    , emitString
    , emitUTF8Char

    -- * unsafe
    , unsafeRunString
    ) where

import qualified Basement.Alg.UTF8 as UTF8
import           Basement.UTF8.Helper          (charToBytes)
import           Basement.Numerical.Conversion (charToInt)
import           Basement.Block.Base (Block(..), MutableBlock(..))
import qualified Basement.Block.Base as B
import           Basement.Cast
import           Basement.Compat.Base
import           Basement.Compat.Semigroup
import           Basement.Monad
import           Basement.FinalPtr (FinalPtr, withFinalPtr)
import           Basement.Numerical.Additive
import           Basement.String                (String(..))
import qualified Basement.String as S
import           Basement.Types.OffsetSize
import           Basement.PrimType (PrimType(..), primMbaWrite)
import           Basement.UArray.Base (UArray(..))
import qualified Basement.UArray.Base as A

import           GHC.ST
import           Data.Proxy

newtype Action = Action
    { runAction_ :: forall prim . PrimMonad prim
                 => MutableBlock Word8 (PrimState prim)
                 -> Offset Word8
                 -> prim (Offset Word8)
    }

data Builder = Builder {-# UNPACK #-} !(CountOf Word8)
                                      !Action

instance Semigroup Builder where
    (<>) = append
    {-# INLINABLE (<>) #-}
instance Monoid Builder where
    mempty = empty
    {-# INLINE mempty #-}
    mappend = append
    {-# INLINABLE mappend #-}
    mconcat = concat
    {-# INLINABLE mconcat #-}

-- | create an empty builder
--
-- this does nothing, build nothing, take no space (in the resulted block)
empty :: Builder
empty = Builder 0 (Action $ \_ !off -> pure off)
{-# INLINE empty #-}

-- | concatenate the 2 given bulider
append :: Builder -> Builder -> Builder
append (Builder size1 (Action action1)) (Builder size2 (Action action2)) =
    Builder size action
  where
    action = Action $ \arr off -> do
      off' <- action1 arr off
      action2 arr off'
    size = size1 + size2
{-# INLINABLE append #-}

-- | concatenate the list of builder
concat :: [Builder] -> Builder
concat = loop 0 (Action $ \_ !off -> pure off)
  where
    loop !sz acc          []                              = Builder sz acc
    loop !sz (Action acc) (Builder !s (Action action):xs) =
       loop (sz + s) (Action $ \arr off -> acc arr off >>= action arr) xs
{-# INLINABLE concat #-}

-- | run the given builder and return the generated block
run :: PrimMonad prim => Builder -> prim (Block Word8)
run (Builder sz action) = do
    mb <- B.new sz
    off <- runAction_ action mb 0
    B.unsafeShrink mb (offsetAsSize off) >>= B.unsafeFreeze

-- | run the given builder and return a UTF8String
--
-- this action is unsafe as there is no guarantee upon the validity of the
-- content of the built block.
unsafeRunString :: PrimMonad prim => Builder -> prim String
unsafeRunString b = do
    str <- run b
    pure $ String $ A.UArray 0 (B.length str) (A.UArrayBA str)

-- | add a Block in the builder
emit :: Block a -> Builder
emit b = Builder size $ Action $ \arr off ->
    B.unsafeCopyBytesRO arr off b' 0 size *> pure (off + sizeAsOffset size)
  where
    b' :: Block Word8
    b' = cast b
    size :: CountOf Word8
    size = B.length b'

emitPrim :: (PrimType ty, ty ~ Word8) => ty -> Builder
emitPrim a = Builder size $ Action $ \(MutableBlock arr) off ->
    primMbaWrite arr off a *> pure (off + sizeAsOffset size)
  where
    size = getSize Proxy a
    getSize :: PrimType ty => Proxy ty -> ty -> CountOf Word8
    getSize p _ = primSizeInBytes p

-- | add a string in the builder
emitString :: String -> Builder
emitString (String str) = Builder size $ Action $ \arr off ->
    A.onBackendPrim (onBA arr off) (onAddr arr off) str *> pure (off + sizeAsOffset size)
  where
    size = A.length str
    onBA :: PrimMonad prim
         => MutableBlock Word8 (PrimState prim)
         -> Offset Word8
         -> Block Word8
         -> prim ()
    onBA   arr off ba   = B.unsafeCopyBytesRO arr off ba 0 size
    onAddr :: PrimMonad prim
           => MutableBlock Word8 (PrimState prim)
           -> Offset Word8
           -> FinalPtr Word8
           -> prim ()
    onAddr arr off fptr = withFinalPtr fptr $ \ptr -> B.unsafeCopyBytesPtr arr off ptr size

-- | emit a UTF8 char in the builder
--
-- this function may be replaced by `emit :: Encoding -> Char -> Builder`
emitUTF8Char :: Char -> Builder
emitUTF8Char c = Builder (charToBytes $ charToInt c) $ Action $ \block@(MutableBlock !_) off ->
    UTF8.writeUTF8 block off c
