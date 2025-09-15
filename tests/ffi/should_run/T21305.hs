{-# LANGUAGE DataKinds, MagicHash, GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}

module Main where
-- Here we ensure that foreign imports with boxed Any-typed
-- arguments and results work as expected. To test the
-- lifted case we pass Int64s; to test the unlifted case
-- we pass a ByteArray#.
import Data.Kind
import GHC.Exts
import GHC.Int
import GHC.IO
import Unsafe.Coerce

foreign import prim "f" f :: Any @(TYPE LiftedRep)
                          -> Any @Type
                          -> Any @(TYPE UnliftedRep)
                          -> (# Any :: Type, Any :: TYPE LiftedRep, Any :: UnliftedType #)
main :: IO ()
main = IO $ \ s1 ->
  case newByteArray# 24# s1 of
  { (# s2, mba #) ->
  case writeByteArray# mba 0# [300, 4000, 50000] s2 of
  { s3 ->
  let
    (# b', a', c' #) =
        (f (unsafeCoerce (9 :: Int64))
           (unsafeCoerce (80 :: Int64))
           (unsafeCoerceUnlifted mba))
    a, b :: Int64
    a = unsafeCoerce a'
    b = unsafeCoerce b'
    c :: MutableByteArray# RealWorld
    c = unsafeCoerceUnlifted c'
  in
  case readInt64Array# c 0# s3 of
  { (# s4, e1 #) ->
  case readInt64Array# c 1# s4 of
  { (# s5, e2 #) ->
    unIO (print [a, b, I64# e1, I64# e2]) s5 }}}}

writeByteArray# :: MutableByteArray# RealWorld
                -> Int#
                -> [Int64]
                -> State# RealWorld -> State# RealWorld
writeByteArray# _   _   []          s = s
writeByteArray# mba off (I64# i:is) s =
  case writeInt64Array# mba off i s of
    s' -> writeByteArray# mba (off +# 8#) is s'
