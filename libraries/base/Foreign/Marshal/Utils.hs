{-# OPTIONS_GHC -fno-implicit-prelude #-}
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
  with,          -- :: Storable a => a -> (Ptr a -> IO b) -> IO b
  new,           -- :: Storable a => a -> IO (Ptr a)

  -- ** Marshalling of Boolean values (non-zero corresponds to 'True')
  --
  fromBool,      -- :: Num a => Bool -> a
  toBool,	 -- :: Num a => a -> Bool

  -- ** Marshalling of Maybe values
  --
  maybeNew,      -- :: (      a -> IO (Ptr a))
		 -- -> (Maybe a -> IO (Ptr a))
  maybeWith,     -- :: (      a -> (Ptr b -> IO c) -> IO c)
		 -- -> (Maybe a -> (Ptr b -> IO c) -> IO c)
  maybePeek,     -- :: (Ptr a -> IO        b )
		 -- -> (Ptr a -> IO (Maybe b))

  -- ** Marshalling lists of storable objects
  --
  withMany,      -- :: (a -> (b -> res) -> res) -> [a] -> ([b] -> res) -> res

  -- ** Haskellish interface to memcpy and memmove
  -- | (argument order: destination, source)
  --
  copyBytes,     -- :: Ptr a -> Ptr a -> Int -> IO ()
  moveBytes,     -- :: Ptr a -> Ptr a -> Int -> IO ()

  -- ** DEPRECATED FUNCTIONS (don\'t use; they may disappear at any time)
  --
  withObject     -- :: Storable a => a -> (Ptr a -> IO b) -> IO b
) where

import Data.Maybe
import Foreign.Ptr	        ( Ptr, nullPtr )
import Foreign.Storable		( Storable(poke) )
import Foreign.C.Types    	( CSize )
import Foreign.Marshal.Alloc 	( malloc, alloca )

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
import GHC.Real			( fromIntegral )
import GHC.Num
import GHC.Base
#endif

#ifdef __NHC__
import Foreign.C.Types		( CInt(..) )
#endif

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

-- old DEPRECATED name (don't use; may disappear at any time)
--
withObject :: Storable a => a -> (Ptr a -> IO b) -> IO b
{-# DEPRECATED withObject "use `with' instead" #-}
withObject  = with


-- marshalling of Boolean values (non-zero corresponds to 'True')
-- -----------------------------

-- |Convert a Haskell 'Bool' to its numeric representation
--
fromBool       :: Num a => Bool -> a
fromBool False  = 0
fromBool True   = 1

-- |Convert a Boolean in numeric representation to a Haskell value
--
toBool :: Num a => a -> Bool
toBool  = (/= 0)


-- marshalling of Maybe values
-- ---------------------------

-- |Allocate storage and marshall a storable value wrapped into a 'Maybe'
--
-- * the 'nullPtr' is used to represent 'Nothing'
--
maybeNew :: (      a -> IO (Ptr a))
	 -> (Maybe a -> IO (Ptr a))
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
	 -> [a]			      -- storable objects
	 -> ([b] -> res)	      -- action on list of marshalled obj.s
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
copyBytes dest src size  = memcpy dest src (fromIntegral size)

-- |Copies the given number of elements from the second area (source) into the
-- first (destination); the copied areas /may/ overlap
--
moveBytes               :: Ptr a -> Ptr a -> Int -> IO ()
moveBytes dest src size  = memmove dest src (fromIntegral size)


-- auxilliary routines
-- -------------------

-- |Basic C routines needed for memory copying
--
foreign import ccall unsafe "string.h" memcpy  :: Ptr a -> Ptr a -> CSize -> IO ()
foreign import ccall unsafe "string.h" memmove :: Ptr a -> Ptr a -> CSize -> IO ()
