-- |
-- Module      : Basement.String.Builder
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- String builder

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Basement.String.Builder
    ( Builder
    , run
    , runUnsafe

    -- * Emit functions
    , emit
    , emitChar

    -- * unsafe
    , unsafeStringBuilder
    ) where


import qualified Basement.Block.Base as Block (length)
import qualified Basement.Block.Builder as Block
import           Basement.Compat.Base
import           Basement.Compat.Semigroup
import           Basement.Monad
import           Basement.String (String, ValidationFailure, Encoding (UTF8), fromBytes)
import           Basement.UArray.Base (UArray)
import qualified Basement.UArray.Base as A

newtype Builder = Builder Block.Builder
  deriving (Semigroup, Monoid)

unsafeStringBuilder :: Block.Builder -> Builder
unsafeStringBuilder = Builder
{-# INLINE unsafeStringBuilder #-}

run :: PrimMonad prim => Builder -> prim (String, Maybe ValidationFailure, UArray Word8)
run (Builder builder) = do
    block <- Block.run builder
    let array = A.UArray 0 (Block.length block) (A.UArrayBA block)
    pure $ fromBytes UTF8 array

-- | run the given builder and return the generated String
--
-- prefer `run`
runUnsafe :: PrimMonad prim => Builder -> prim String
runUnsafe (Builder builder) = Block.unsafeRunString builder

-- | add a string in the builder
emit :: String -> Builder
emit = Builder . Block.emitString

-- | emit a UTF8 char in the builder
emitChar :: Char -> Builder
emitChar = Builder . Block.emitUTF8Char
