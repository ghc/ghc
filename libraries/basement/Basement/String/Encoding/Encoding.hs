-- |
-- Module      : Basement.String.Encoding.Encoding
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE FlexibleContexts #-}

module Basement.String.Encoding.Encoding
    ( Encoding(..)
    , convertFromTo
    ) where

import           Basement.Compat.Base
import           Basement.Types.OffsetSize
import           Basement.Monad
import           Basement.PrimType
import           Basement.MutableBuilder
import           Basement.Numerical.Additive
import           Basement.UArray (UArray)
import           Basement.UArray.Mutable (MUArray)
import qualified Basement.UArray as Vec

class Encoding encoding where
    -- | the unit element use for the encoding.
    -- i.e. Word8 for ASCII7 or UTF8, Word16 for UTF16...
    --
    type Unit encoding

    -- | define the type of error handling you want to use for the
    -- next function.
    --
    -- > type Error UTF8 = Either UTF8_Invalid
    --
    type Error encoding

    -- | consume an `Unit encoding` and return the Unicode point and the position
    -- of the next possible `Unit encoding`
    --
    encodingNext :: encoding
                      -- ^ only used for type deduction
                -> (Offset (Unit encoding) -> Unit encoding)
                      -- ^ method to access a given `Unit encoding`
                      -- (see `unsafeIndexer`)
                -> Offset (Unit encoding)
                      -- ^ offset of the `Unit encoding` where starts the
                      -- encoding of a given unicode
                -> Either (Error encoding) (Char, Offset (Unit encoding)) -- ^ either successfully validated the `Unit encoding`
                      -- and returned the next offset or fail with an
                      -- `Error encoding`

    -- Write a unicode point encoded into one or multiple `Unit encoding`
    --
    -- > build 64 $ sequence_ (write UTF8) "this is a simple list of char..."
    --
    encodingWrite :: (PrimMonad st, Monad st)
                  => encoding
                      -- ^ only used for type deduction
                  -> Char
                      -- ^ the unicode character to encode
                  -> Builder (UArray (Unit encoding))
                             (MUArray (Unit encoding))
                             (Unit encoding) st err ()

-- | helper to convert a given Array in a given encoding into an array
-- with another encoding.
--
-- This is a helper to convert from one String encoding to another.
-- This function is (quite) slow and needs some work.
--
-- ```
-- let s16 = ... -- string in UTF16
-- -- create s8, a UTF8 String
-- let s8  = runST $ convertWith UTF16 UTF8 (toBytes s16)
--
-- print s8
-- ```
--
convertFromTo :: ( PrimMonad st, Monad st
                 , Encoding input, PrimType (Unit input)
                 , Encoding output, PrimType (Unit output)
                 )
              => input
                -- ^ Input's encoding type
              -> output
                -- ^ Output's encoding type
              -> UArray (Unit input)
                -- ^ the input raw array
              -> st (Either (Offset (Unit input), Error input) (UArray (Unit output)))
convertFromTo inputEncodingTy outputEncodingTy bytes
    | Vec.null bytes = return . return $ mempty
    | otherwise      = Vec.unsafeIndexer bytes $ \t -> Vec.builderBuild 64 (loop azero t)
  where
    lastUnit = Vec.length bytes

    loop off getter
      | off .==# lastUnit = return ()
      | otherwise = case encodingNext inputEncodingTy getter off of
          Left err -> mFail (off, err)
          Right (c, noff) -> encodingWrite outputEncodingTy c >> loop noff getter
