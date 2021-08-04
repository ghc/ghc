-- |
-- Module      : Data.ByteArray.Pack
-- License     : BSD-Style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Simple Byte Array packer
--
-- Simple example:
--
-- > > flip pack 20 $ putWord8 0x41 >> putByteString "BCD" >> putWord8 0x20 >> putStorable (42 :: Word32)
-- > Right (ABCD *\NUL\NUL\NUL")
--
-- Original code from <https://hackage.haskell.org/package/bspack>
-- generalized and adapted to run on 'memory', and spellchecked / tweaked. (2015-05)
-- Copyright (c) 2014 Nicolas DI PRIMA
--
module Data.ByteArray.Pack
    ( Packer
    , Result(..)
    , fill
    , pack
      -- * Operations
      -- ** put
    , putWord8
    , putWord16
    , putWord32
    , putStorable
    , putBytes
    , fillList
    , fillUpWith
      -- ** skip
    , skip
    , skipStorable
    ) where

import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Data.Memory.Internal.Imports ()
import           Data.Memory.Internal.Compat
import           Data.Memory.PtrMethods
import           Data.ByteArray.Pack.Internal
import           Data.ByteArray (ByteArray, ByteArrayAccess, MemView(..))
import qualified Data.ByteArray as B

-- | Fill a given sized buffer with the result of the Packer action
fill :: ByteArray byteArray => Int -> Packer a -> Either String byteArray
fill len packing = unsafeDoIO $ do
    (val, out) <- B.allocRet len $ \ptr -> runPacker_ packing (MemView ptr len)
    case val of 
        PackerMore _ (MemView _ r)
            | r == 0    -> return $ Right out
            | otherwise -> return $ Left ("remaining unpacked bytes " ++ show r ++ " at the end of buffer")
        PackerFail err  -> return $ Left err

-- | Pack the given packer into the given bytestring
pack :: ByteArray byteArray => Packer a -> Int -> Either String byteArray
pack packing len = fill len packing
{-# DEPRECATED pack "use fill instead" #-}

fillUpWithWord8' :: Word8 -> Packer ()
fillUpWithWord8' w = Packer $ \(MemView ptr size) -> do
    memSet ptr w size
    return $ PackerMore () (MemView (ptr `plusPtr` size) 0)

-- | Put a storable from the current position in the stream
putStorable :: Storable storable => storable -> Packer ()
putStorable s = actionPacker (sizeOf s) (\ptr -> poke (castPtr ptr) s)

-- | Put a Byte Array from the current position in the stream
--
-- If the ByteArray is null, then do nothing
putBytes :: ByteArrayAccess ba => ba -> Packer ()
putBytes bs
    | neededLength == 0 = return ()
    | otherwise         =
        actionPacker neededLength $ \dstPtr -> B.withByteArray bs $ \srcPtr ->
            memCopy dstPtr srcPtr neededLength
  where
    neededLength = B.length bs

-- | Skip some bytes from the current position in the stream
skip :: Int -> Packer ()
skip n = actionPacker n (\_ -> return ())

-- | Skip the size of a storable from the current position in the stream
skipStorable :: Storable storable => storable -> Packer ()
skipStorable = skip . sizeOf

-- | Fill up from the current position in the stream to the end
--
-- It is equivalent to:
--
-- > fillUpWith s == fillList (repeat s)
--
fillUpWith :: Storable storable => storable -> Packer ()
fillUpWith s = fillList $ repeat s
{-# RULES "fillUpWithWord8" forall s . fillUpWith s = fillUpWithWord8' s #-}
{-# NOINLINE fillUpWith #-}

-- | Will put the given storable list from the current position in the stream
-- to the end.
--
-- This function will fail with not enough storage if the given storable can't
-- be written (not enough space)
--
-- Example:
--
-- > > pack (fillList $ [1..] :: Word8) 9
-- > "\1\2\3\4\5\6\7\8\9"
-- > > pack (fillList $ [1..] :: Word32) 4
-- > "\1\0\0\0"
-- > > pack (fillList $ [1..] :: Word32) 64
-- > .. <..succesful..>
-- > > pack (fillList $ [1..] :: Word32) 1
-- > .. <.. not enough space ..>
-- > > pack (fillList $ [1..] :: Word32) 131
-- > .. <.. not enough space ..>
--
fillList :: Storable storable => [storable] -> Packer ()
fillList []     = return ()
fillList (x:xs) = putStorable x >> fillList xs

------------------------------------------------------------------------------
-- Common packer                                                            --
------------------------------------------------------------------------------

-- | put Word8 in the current position in the stream
putWord8 :: Word8 -> Packer ()
putWord8 = putStorable
{-# INLINE putWord8 #-}

-- | put Word16 in the current position in the stream
-- /!\ use Host Endianness
putWord16 :: Word16 -> Packer ()
putWord16 = putStorable
{-# INLINE putWord16 #-}

-- | put Word32 in the current position in the stream
-- /!\ use Host Endianness
putWord32 :: Word32 -> Packer ()
putWord32 = putStorable
{-# INLINE putWord32 #-}
