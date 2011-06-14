{-# OPTIONS_GHC -fwarn-unused-imports  #-}
module RedundantImport where

-- this import is redundant, but GHC does not spot it
import qualified Foreign.Storable 

import Foreign.Storable (Storable, sizeOf, alignment, peek, poke, )
import Foreign.Ptr (castPtr, )

newtype T a = Cons a

instance Storable a => Storable (T a) where
   sizeOf (Cons a) = sizeOf a
   alignment (Cons a) = alignment a
   peek = fmap Cons . peek . castPtr
   poke p (Cons a) = poke (castPtr p) a
