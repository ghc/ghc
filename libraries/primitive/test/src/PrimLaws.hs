{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wall #-}

-- This module is almost an exact copy of the unexported module
-- Test.QuickCheck.Classes.Prim from quickcheck-classes. We cannot depend
-- on quickcheck-classes in the test suite since that would imply a circular
-- dependency between primitive and quickcheck-classes. Instead, we copy
-- this one module and then depend on quickcheck-classes-base to get
-- everything else we need.
module PrimLaws
  ( primLaws
  ) where

import Control.Applicative
import Control.Monad.Primitive (primitive_)
import Control.Monad.ST
import Data.Proxy (Proxy)
import Data.Primitive.PrimArray
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Primitive.Ptr
import Foreign.Marshal.Alloc
import GHC.Exts (State#,Int#,Int(I#),(+#),(<#))

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (IsList(fromList,toList))
#endif

import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))

import qualified Data.List as L
import qualified Data.Primitive as P

import Test.QuickCheck.Classes.Base (Laws(..))
import Test.QuickCheck.Classes.Internal (isTrue#)

-- | Test that a 'Prim' instance obey the several laws.
primLaws :: (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
primLaws p = Laws "Prim"
  [ ("ByteArray Put-Get (you get back what you put in)", primPutGetByteArray p)
  , ("ByteArray Get-Put (putting back what you got out has no effect)", primGetPutByteArray p)
  , ("ByteArray Put-Put (putting twice is same as putting once)", primPutPutByteArray p)
  , ("ByteArray Set Range", primSetByteArray p)
#if MIN_VERSION_base(4,7,0)
  , ("ByteArray List Conversion Roundtrips", primListByteArray p)
#endif
  , ("Ptr Put-Get (you get back what you put in)", primPutGetAddr p)
  , ("Ptr List Conversion Roundtrips", primListAddr p)
  ]

primListAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListAddr _ = property $ \(as :: [a]) -> unsafePerformIO $ do
  let len = L.length as
  ptr :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
  let go :: Int -> [a] -> IO ()
      go !ix xs = case xs of
        [] -> return ()
        (x : xsNext) -> do
          writeOffPtr ptr ix x
          go (ix + 1) xsNext
  go 0 as
  let rebuild :: Int -> IO [a]
      rebuild !ix = if ix < len
        then (:) <$> readOffPtr ptr ix <*> rebuild (ix + 1)
        else return []
  asNew <- rebuild 0
  free ptr
  return (as == asNew)

primPutGetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primPutGetByteArray _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ runST $ do
    arr <- newPrimArray len
    writePrimArray arr ix a
    a' <- readPrimArray arr ix
    return (a == a')

primGetPutByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primGetPutByteArray _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = primArrayFromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  arr2 <- return $ runST $ do
    marr <- newPrimArray len
    copyPrimArray marr 0 arr1 0 len
    a <- readPrimArray marr ix
    writePrimArray marr ix a
    unsafeFreezePrimArray marr
  return (arr1 == arr2)

primPutPutByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primPutPutByteArray _ = property $ \(a :: a) (as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = primArrayFromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  (arr2,arr3) <- return $ runST $ do
    marr2 <- newPrimArray len
    copyPrimArray marr2 0 arr1 0 len
    writePrimArray marr2 ix a
    marr3 <- newPrimArray len
    copyMutablePrimArray marr3 0 marr2 0 len
    arr2 <- unsafeFreezePrimArray marr2
    writePrimArray marr3 ix a
    arr3 <- unsafeFreezePrimArray marr3
    return (arr2,arr3)
  return (arr2 == arr3)

primPutGetAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primPutGetAddr _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptr :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
    writeOffPtr ptr ix a
    a' <- readOffPtr ptr ix
    free ptr
    return (a == a')

primSetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetByteArray _ = property $ \(as :: [a]) (z :: a) -> do
  let arr1 = primArrayFromList as :: PrimArray a
      len = L.length as
  x <- choose (0,len)
  y <- choose (0,len)
  let lo = min x y
      hi = max x y
  return $ runST $ do
    marr2 <- newPrimArray len
    copyPrimArray marr2 0 arr1 0 len
    marr3 <- newPrimArray len
    copyPrimArray marr3 0 arr1 0 len
    setPrimArray marr2 lo (hi - lo) z
    internalDefaultSetPrimArray marr3 lo (hi - lo) z
    arr2 <- unsafeFreezePrimArray marr2
    arr3 <- unsafeFreezePrimArray marr3
    return (arr2 == arr3)

#if MIN_VERSION_base(4,7,0)
primListByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListByteArray _ = property $ \(as :: [a]) ->
  as == toList (fromList as :: PrimArray a)
#endif

internalDefaultSetPrimArray :: Prim a
  => MutablePrimArray s a -> Int -> Int -> a -> ST s ()
internalDefaultSetPrimArray (MutablePrimArray arr) (I# i) (I# len) ident =
  primitive_ (internalDefaultSetByteArray# arr i len ident)

internalDefaultSetByteArray# :: Prim a
  => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
internalDefaultSetByteArray# arr# i# len# ident = go 0#
  where
  go ix# s0 = if isTrue# (ix# <# len#)
    then case writeByteArray# arr# (i# +# ix#) ident s0 of
      s1 -> go (ix# +# 1#) s1
    else s0
