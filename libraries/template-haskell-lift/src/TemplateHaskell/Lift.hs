{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}

module TemplateHaskell.Lift
  ( Q
  , Code
  , Quote
  , Exp
  , Lift(..)
  , defaultLiftTyped
  , liftAddrCompat
  , liftIntCompat
  ) where

import GHC.Exts (Int(..))
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peek)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)
#if __GLASGOW_HASKELL__ >= 912
import GHC.Internal.TH.Lift
import GHC.Internal.TH.Syntax
#else
import Language.Haskell.TH.Syntax
#endif

-- template-haskell >= 2.17
#if  __GLASGOW_HASKELL__ >= 901
defaultLiftTyped x = unsafeCodeCoerce (lift x)
-- template-haskell >= 2.16
#else
defaultLiftTyped x = unsafeTExpCoerce (lift x)
#endif

-- Quote is introduced in GHC-9.1/template-haskell-2.17
#if __GLASGOW_HASKELL__ < 901
liftAddrCompat :: ForeignPtr Word8 -> Word -> Word -> Q Exp
#else
liftAddrCompat :: Quote m => ForeignPtr Word8 -> Word -> Word -> m Exp
#endif
liftAddrCompat fptr off len =
#if  __GLASGOW_HASKELL__ >= 810
     pure $ LitE $ BytesPrimL $ Bytes fptr off len
#else
     do
       let
         loop !ptr 0 xs = pure $ reverse xs
         loop !ptr !len xs = do
           x <- peek ptr
           loop (ptr `plusPtr` 1) (len -1) (x:xs)
       let words = unsafePerformIO $ withForeignPtr fptr $ \ptr -> loop (ptr `plusPtr` (fromIntegral off)) len []
       pure $ LitE $ StringPrimL $ words
#endif

liftIntCompat n = pure $ AppE (ConE 'I#) (LitE (IntPrimL n))
