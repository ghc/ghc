{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Utils
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for primitive marshaling
--
-----------------------------------------------------------------------------

module Foreign.Marshal.Utils (
  -- * General marshalling utilities

  -- ** Combined allocation and marshalling
  --
  with,
  new,

  -- ** Marshalling of Boolean values (non-zero corresponds to 'True')
  --
  fromBool,
  toBool,

  -- ** Marshalling of Maybe values
  --
  maybeNew,
  maybeWith,
  maybePeek,

  -- ** Marshalling lists of storable objects
  --
  withMany,

  -- ** Haskellish interface to memcpy and memmove
  -- | (argument order: destination, source)
  --
  copyBytes,
  moveBytes,
) where

import Data.Maybe
import Foreign.Ptr              ( Ptr, nullPtr )
import Foreign.Storable         ( Storable(poke) )
import Foreign.C.Types          ( CSize(..) )
import Foreign.Marshal.Alloc    ( malloc, alloca )

import GHC.Real                 ( fromIntegral )
import GHC.Num
import GHC.Base

-- combined allocation and marshalling
-- -----------------------------------

-- |Allocate a block of memory and marshal a value into it
-- (the combination of 'malloc' and 'poke').
-- The size of the area allocated is determined by the 'Foreign.Storable.sizeOf'
-- method from the instance of 'Storable' for the appropriate type.
--
-- The memory may be deallocated using 'Foreign.Marshal.Alloc.free' or
-- 'Foreign.Marshal.Alloc.finalizerFree' when no longer required.
--
new     :: Storable a => a -> IO (Ptr a)
new val  = 
  do 
    ptr <- malloc
    poke ptr val
    return ptr

-- |@'with' val f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory into which
-- @val@ has been marshalled (the combination of 'alloca' and 'poke').
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
--
with       :: Storable a => a -> (Ptr a -> IO b) -> IO b
with val f  =
  alloca $ \ptr -> do
    poke ptr val
    res <- f ptr
    return res


-- marshalling of Boolean values (non-zero corresponds to 'True')
-- -----------------------------

-- |Convert a Haskell 'Bool' to its numeric representation
--
fromBool       :: Num a => Bool -> a
fromBool False  = 0
fromBool True   = 1

-- |Convert a Boolean in numeric representation to a Haskell value
--
toBool :: (Eq a, Num a) => a -> Bool
toBool  = (/= 0)


-- marshalling of Maybe values
-- ---------------------------

-- |Allocate storage and marshal a storable value wrapped into a 'Maybe'
--
-- * the 'nullPtr' is used to represent 'Nothing'
--
maybeNew :: (      a -> IO (Ptr b))
         -> (Maybe a -> IO (Ptr b))
maybeNew  = maybe (return nullPtr)

-- |Converts a @withXXX@ combinator into one marshalling a value wrapped
-- into a 'Maybe', using 'nullPtr' to represent 'Nothing'.
--
maybeWith :: (      a -> (Ptr b -> IO c) -> IO c) 
          -> (Maybe a -> (Ptr b -> IO c) -> IO c)
maybeWith  = maybe ($ nullPtr)

-- |Convert a peek combinator into a one returning 'Nothing' if applied to a
-- 'nullPtr' 
--
maybePeek                           :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybePeek peek ptr | ptr == nullPtr  = return Nothing
                   | otherwise       = do a <- peek ptr; return (Just a)


-- marshalling lists of storable objects
-- -------------------------------------

-- |Replicates a @withXXX@ combinator over a list of objects, yielding a list of
-- marshalled objects
--
withMany :: (a -> (b -> res) -> res)  -- withXXX combinator for one object
         -> [a]                       -- storable objects
         -> ([b] -> res)              -- action on list of marshalled obj.s
         -> res
withMany _       []     f = f []
withMany withFoo (x:xs) f = withFoo x $ \x' ->
                              withMany withFoo xs (\xs' -> f (x':xs'))


-- Haskellish interface to memcpy and memmove
-- ------------------------------------------

-- |Copies the given number of bytes from the second area (source) into the
-- first (destination); the copied areas may /not/ overlap
--
copyBytes               :: Ptr a -> Ptr a -> Int -> IO ()
copyBytes dest src size  = do _ <- memcpy dest src (fromIntegral size)
                              return ()

-- |Copies the given number of bytes from the second area (source) into the
-- first (destination); the copied areas /may/ overlap
--
moveBytes               :: Ptr a -> Ptr a -> Int -> IO ()
moveBytes dest src size  = do _ <- memmove dest src (fromIntegral size)
                              return ()


-- auxilliary routines
-- -------------------

-- |Basic C routines needed for memory copying
--
foreign import ccall unsafe "string.h" memcpy  :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
foreign import ccall unsafe "string.h" memmove :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)

