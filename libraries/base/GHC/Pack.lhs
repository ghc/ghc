\begin{code}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Pack
-- Copyright   :  (c) The University of Glasgow 1997-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides a small set of low-level functions for packing
-- and unpacking a chunk of bytes. Used by code emitted by the compiler
-- plus the prelude libraries.
-- 
-- The programmer level view of packed strings is provided by a GHC
-- system library PackedString.
--
-----------------------------------------------------------------------------

module GHC.Pack
       (
        -- (**) - emitted by compiler.

        packCString#,
        unpackCString,
        unpackCString#,
        unpackNBytes#,
        unpackFoldrCString#,  -- (**)
        unpackAppendCString#,  -- (**)
       ) 
        where

import GHC.Base
import GHC.List ( length )
import GHC.ST
import GHC.Ptr

data ByteArray ix              = ByteArray        ix ix ByteArray#
data MutableByteArray s ix     = MutableByteArray ix ix (MutableByteArray# s)

unpackCString :: Ptr a -> [Char]
unpackCString a@(Ptr addr)
  | a == nullPtr  = []
  | otherwise      = unpackCString# addr

packCString#         :: [Char]          -> ByteArray#
packCString# str = case (packString str) of { ByteArray _ _ bytes -> bytes }

packString :: [Char] -> ByteArray Int
packString str = runST (packStringST str)

packStringST :: [Char] -> ST s (ByteArray Int)
packStringST str =
  let len = length str  in
  packNBytesST len str

packNBytesST :: Int -> [Char] -> ST s (ByteArray Int)
packNBytesST (I# length#) str =
  {- 
   allocate an array that will hold the string
   (not forgetting the NUL byte at the end)
  -}
 new_ps_array (length# +# 1#) >>= \ ch_array ->
   -- fill in packed string from "str"
 fill_in ch_array 0# str   >>
   -- freeze the puppy:
 freeze_ps_array ch_array length#
 where
  fill_in :: MutableByteArray s Int -> Int# -> [Char] -> ST s ()
  fill_in arr_in# idx [] =
   write_ps_array arr_in# idx (chr# 0#) >>
   return ()

  fill_in arr_in# idx (C# c : cs) =
   write_ps_array arr_in# idx c  >>
   fill_in arr_in# (idx +# 1#) cs

-- (Very :-) ``Specialised'' versions of some CharArray things...

new_ps_array    :: Int# -> ST s (MutableByteArray s Int)
write_ps_array  :: MutableByteArray s Int -> Int# -> Char# -> ST s () 
freeze_ps_array :: MutableByteArray s Int -> Int# -> ST s (ByteArray Int)

new_ps_array size = ST $ \ s ->
    case (newByteArray# size s)   of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray bot bot barr# #) }
  where
    bot = error "new_ps_array"

write_ps_array (MutableByteArray _ _ barr#) n ch = ST $ \ s# ->
    case writeCharArray# barr# n ch s#  of { s2#   ->
    (# s2#, () #) }

-- same as unsafeFreezeByteArray
freeze_ps_array (MutableByteArray _ _ arr#) len# = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray 0 (I# len#) frozen# #) }
\end{code}
