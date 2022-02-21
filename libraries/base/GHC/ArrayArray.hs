{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ArrayArray
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Legacy interface for arrays of arrays.
-- Deprecated, because the 'Array#' type can now store arrays directly.
-- Consider simply using 'Array#' instead of 'ArrayArray#'.
--
-- Use GHC.Exts instead of importing this module directly.
--
----------------------------------------------------------------------------

module GHC.ArrayArray
  ( ArrayArray#(..), MutableArrayArray#(..)
  , newArrayArray#
  , unsafeFreezeArrayArray#
  , sizeofArrayArray#
  , sizeofMutableArrayArray#
  , indexByteArrayArray#
  , indexArrayArrayArray#
  , readByteArrayArray#
  , readMutableByteArrayArray#
  , readArrayArrayArray#
  , readMutableArrayArrayArray#
  , writeByteArrayArray#
  , writeMutableByteArrayArray#
  , writeArrayArrayArray#
  , writeMutableArrayArrayArray#
  , copyArrayArray#
  , copyMutableArrayArray#
  , sameArrayArray#
  , sameMutableArrayArray#
  )
  where

import GHC.Prim
import GHC.Types ( Type, UnliftedType, isTrue# )
import Unsafe.Coerce ( unsafeCoerce, unsafeCoerceUnlifted )
default ()

{- **********************************************************************
*                                                                       *
*                 Arrays of arrays (legacy interface)                    *
*                                                                       *
********************************************************************** -}

type ArrayArray# :: UnliftedType
newtype ArrayArray# = ArrayArray# (Array# ByteArray#)

type MutableArrayArray# :: Type -> UnliftedType
newtype MutableArrayArray# s = MutableArrayArray# (MutableArray# s ByteArray#)

-- | Create a new mutable array of arrays with the specified number of elements,
--   in the specified state thread, with each element recursively referring to the
--   newly created array.
newArrayArray# :: Int# -> State# s -> (# State# s, MutableArrayArray# s #)
newArrayArray# sz s1 =
  -- Create a placeholder ByteArray to initialise the underlying MutableArray#.
  case newByteArray# 0# s1 of
    (# s2, placeholder #) ->
      -- Create a new MutableArray# holding the placeholder ByteArray# value.
      case newArray# sz (unsafeCoerceUnlifted placeholder) s2 of
        (# s3, arr #) ->
          -- Now update the MutableArray# so that the elements refer back
          -- to the mutable array itself.
          case write_array_to_array arr 0# s3 of
            s4 -> (# s4, MutableArrayArray# (unsafeCoerceUnlifted arr) #)

  where
    write_array_to_array :: MutableArray# s ByteArray# -> Int# -> State# s -> State# s
    write_array_to_array _ i s
      | isTrue# (i >=# sz)
      = s
    write_array_to_array arr i s
      = case writeArray# arr i (unsafeCoerceUnlifted arr) s of
          s' -> write_array_to_array arr (i +# 1#) s'

-- | Make a mutable array of arrays immutable, without copying.
unsafeFreezeArrayArray# :: MutableArrayArray# s -> State# s -> (# State# s, ArrayArray# #)
unsafeFreezeArrayArray# = unsafeCoerce unsafeFreezeArray#

-- | Return the number of elements in the array.
sizeofArrayArray# :: ArrayArray# -> Int#
sizeofArrayArray# = unsafeCoerce sizeofArray#

-- | Return the number of elements in the array.
sizeofMutableArrayArray# :: MutableArrayArray# s -> Int#
sizeofMutableArrayArray# = unsafeCoerce sizeofMutableArray#

indexByteArrayArray# :: ArrayArray# -> Int# -> ByteArray#
indexByteArrayArray# = unsafeCoerce indexArray#

indexArrayArrayArray# :: ArrayArray# -> Int# -> ArrayArray#
indexArrayArrayArray# = unsafeCoerce indexArray#

readByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, ByteArray# #)
readByteArrayArray# = unsafeCoerce readArray#

readMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
readMutableByteArrayArray# = unsafeCoerce readArray#

readArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, ArrayArray# #)
readArrayArrayArray# = unsafeCoerce readArray#

readMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableArrayArray# s #)
readMutableArrayArrayArray# = unsafeCoerce readArray#

writeByteArrayArray# :: MutableArrayArray# s -> Int# -> ByteArray# -> State# s -> State# s
writeByteArrayArray# = unsafeCoerce writeArray#

writeMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> MutableByteArray# s -> State# s -> State# s
writeMutableByteArrayArray# = unsafeCoerce writeArray#

writeArrayArrayArray# :: MutableArrayArray# s -> Int# -> ArrayArray# -> State# s -> State# s
writeArrayArrayArray# = unsafeCoerce writeArray#

writeMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> State# s -> State# s
writeMutableArrayArrayArray# = unsafeCoerce writeArray#

-- | Copy a range of the 'ArrayArray#' to the specified region in the 'MutableArrayArray#'.
--   Both arrays must fully contain the specified ranges, but this is not checked.
--   The two arrays must not be the same array in different states, but this is not checked either.
copyArrayArray# :: ArrayArray# -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
copyArrayArray# = unsafeCoerce copyArray#

-- | Copy a range of the first MutableArrayArray# to the specified region in the second
--   MutableArrayArray#.
--   Both arrays must fully contain the specified ranges, but this is not checked.
--   The regions are allowed to overlap, although this is only possible when the same
--   array is provided as both the source and the destination.
copyMutableArrayArray# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
copyMutableArrayArray# = unsafeCoerce copyMutableArray#

-- | Compare the underlying pointers of two arrays of arrays.
sameArrayArray# :: ArrayArray# -> ArrayArray# -> Int#
sameArrayArray# (ArrayArray# arr1) (ArrayArray# arr2) = reallyUnsafePtrEquality# arr1 arr2

-- | Compare the underlying pointers of two mutable arrays of arrays.
sameMutableArrayArray# :: MutableArrayArray# s -> MutableArrayArray# s -> Int#
sameMutableArrayArray# (MutableArrayArray# marr1) (MutableArrayArray# marr2 ) = reallyUnsafePtrEquality# marr1 marr2
