{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.ForeignPtr
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'ForeignPtr' type and operations.  This module is part of the
-- Foreign Function Interface (FFI) and will usually be imported via
-- the "Foreign" module.
--
-----------------------------------------------------------------------------

module Foreign.ForeignPtr
        ( 
	-- * Finalised data pointers
	  ForeignPtr             -- abstract, instance of: Eq
        , newForeignPtr          -- :: Ptr a -> IO () -> IO (ForeignPtr a)
        , addForeignPtrFinalizer -- :: ForeignPtr a -> IO () -> IO ()
	, withForeignPtr         -- :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
	, foreignPtrToPtr	 -- :: ForeignPtr a -> Ptr a
	, touchForeignPtr        -- :: ForeignPtr a -> IO ()
	, castForeignPtr	 -- :: ForeignPtr a -> ForeignPtr b

	-- * GHC extensions
	, mallocForeignPtr	--  :: Storable a => IO (ForeignPtr a)
	, mallocForeignPtrBytes	--  :: Int -> IO (ForeignPtr a)
        ) 
	where

import Foreign.Ptr
import Foreign.Storable
import Data.Dynamic

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase
import GHC.Num
import GHC.Ptr	( Ptr(..) )
import GHC.Err
#endif

#include "Dynamic.h"
INSTANCE_TYPEABLE1(ForeignPtr,foreignPtrTc,"ForeignPtr")

#ifdef __GLASGOW_HASKELL__
-- |The type 'ForeignPtr' represents references to objects that are
-- maintained in a foreign language, i.e., that are not part of the
-- data structures usually managed by the Haskell storage manager.
-- The essential difference between 'ForeignPtr's and vanilla memory
-- references of type @Ptr a@ is that the former may be associated
-- with /finalisers/. A finaliser is a routine that is invoked when
-- the Haskell storage manager detects that - within the Haskell heap
-- and stack - there are no more references left that are pointing to
-- the 'ForeignPtr'.  Typically, the finaliser will, then, invoke
-- routines in the foreign language that free the resources bound by
-- the foreign object.
--
-- The 'ForeignPtr' is parameterised in the same way as 'Ptr'.  The
-- type argument of 'ForeignPtr' should normally be an instance of
-- class 'Storable'.
--
data ForeignPtr a 
  = ForeignPtr ForeignObj#
  | MallocPtr  (MutableByteArray# RealWorld)

eqForeignPtr  :: ForeignPtr a -> ForeignPtr a -> Bool
eqForeignPtr (ForeignPtr fo1#) (ForeignPtr fo2#) = eqForeignObj# fo1# fo2#
eqForeignPtr (MallocPtr fo1#)  (MallocPtr fo2#)  = sameMutableByteArray# fo1# fo2#
eqForeignPtr _ _ = False

instance Eq (ForeignPtr a) where 
    p == q = eqForeignPtr p q

newForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
-- ^Turns a plain memory reference into a foreign object
-- by associating a finaliser - given by the monadic operation
-- - with the reference.  The finaliser will be executed after
-- the last reference to the foreign object is dropped.  Note
-- that there is no guarantee on how soon the finaliser is
-- executed after the last reference was dropped; this depends
-- on the details of the Haskell storage manager. The only
-- guarantee is that the finaliser runs before the program
-- terminates.
newForeignPtr p finalizer
  = do fObj <- mkForeignPtr p
       addForeignPtrFinalizer fObj finalizer
       return fObj

-- | allocates some memory and returns a ForeignPtr to it.  The memory
-- will be released automatically when the ForeignPtr is discarded.
--
-- @mallocForeignPtr@ is equivalent to
--
-- >    do { p <- malloc; newForeignPtr p free }
-- 
-- although it may be implemented differently internally.  You may not
-- assume that the memory returned by 'mallocForeignPtr' has been
-- allocated with C's @malloc()@.

mallocForeignPtr :: Storable a => IO (ForeignPtr a)
mallocForeignPtr = doMalloc undefined
  where doMalloc :: Storable a => a -> IO (ForeignPtr a)
        doMalloc a = IO $ \s ->
	  case newPinnedByteArray# size s of { (# s, mbarr# #) ->
	   (# s, MallocPtr mbarr# #)
          }
	  where (I# size) = sizeOf a

-- | similar to 'mallocForeignPtr', except that the size of the memory required
-- is given explicitly as a number of bytes.
mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes (I# size) = IO $ \s ->
  case newPinnedByteArray# size s      of { (# s, mbarr# #) ->
   (# s, MallocPtr mbarr# #)
  }

addForeignPtrFinalizer :: ForeignPtr a -> IO () -> IO ()
-- ^This function adds another finaliser to the given
-- foreign object.  No guarantees are made on the order in
-- which multiple finalisers for a single object are run.
addForeignPtrFinalizer (ForeignPtr fo) finalizer = 
  IO $ \s -> case mkWeak# fo () finalizer s of { (# s1, w #) -> (# s1, () #) }
addForeignPtrFinalizer (MallocPtr fo) finalizer = 
  IO $ \s -> case mkWeak# fo () finalizer s of { (# s1, w #) -> (# s1, () #) }

mkForeignPtr :: Ptr a -> IO (ForeignPtr a) {- not exported -}
mkForeignPtr (Ptr obj) =  IO ( \ s# ->
    case mkForeignObj# obj s# of
      (# s1#, fo# #) -> (# s1#,  ForeignPtr fo# #) )

touchForeignPtr :: ForeignPtr a -> IO ()
-- ^This function ensures that the foreign object in
-- question is alive at the given place in the sequence of IO
-- actions. In particular 'withForeignPtr'
-- does a 'touchForeignPtr' after it
-- executes the user action.
-- 
-- This function can be used to express liveness
-- dependencies between 'ForeignPtr's: for
-- example, if the finalizer for one
-- 'ForeignPtr' touches a second
-- 'ForeignPtr', then it is ensured that the
-- second 'ForeignPtr' will stay alive at
-- least as long as the first.  This can be useful when you
-- want to manipulate /interior pointers/ to
-- a foreign structure: you can use
-- 'touchForeignObj' to express the
-- requirement that the exterior pointer must not be finalized
-- until the interior pointer is no longer referenced.
touchForeignPtr (ForeignPtr fo) 
   = IO $ \s -> case touch# fo s of s -> (# s, () #)
touchForeignPtr (MallocPtr fo) 
   = IO $ \s -> case touch# fo s of s -> (# s, () #)

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
-- ^This is a way to look at the pointer living inside a
-- foreign object.  This function takes a function which is
-- applied to that pointer. The resulting 'IO' action is then
-- executed. The foreign object is kept alive at least during
-- the whole action, even if it is not used directly
-- inside. Note that it is not safe to return the pointer from
-- the action and use it after the action completes. All uses
-- of the pointer should be inside the
-- 'withForeignPtr' bracket.  The reason for
-- this unsafety is the same as for
-- 'foreignPtrToPtr' below: the finalizer
-- may run earlier than expected, because the compiler can only
-- track usage of the 'ForeignPtr' object, not
-- a 'Ptr' object made from it.
--
-- This function is normally used for marshalling data to
-- or from the object pointed to by the
-- 'ForeignPtr', using the operations from the
-- 'Storable' class.
withForeignPtr fo io
  = do r <- io (foreignPtrToPtr fo)
       touchForeignPtr fo
       return r

foreignPtrToPtr :: ForeignPtr a -> Ptr a
-- ^This function extracts the pointer component of a foreign
-- pointer.  This is a potentially dangerous operations, as if the
-- argument to 'foreignPtrToPtr' is the last usage
-- occurence of the given foreign pointer, then its finaliser(s) will
-- be run, which potentially invalidates the plain pointer just
-- obtained.  Hence, 'touchForeignPtr' must be used
-- wherever it has to be guaranteed that the pointer lives on - i.e.,
-- has another usage occurrence.
--
-- To avoid subtle coding errors, hand written marshalling code
-- should preferably use 'withForeignPtr' rather
-- than combinations of 'foreignPtrToPtr' and
-- 'touchForeignPtr'.  However, the later routines
-- are occasionally preferred in tool generated marshalling code.
foreignPtrToPtr (ForeignPtr fo) = Ptr (foreignObjToAddr# fo)
foreignPtrToPtr (MallocPtr  fo) = Ptr (byteArrayContents# (unsafeCoerce# fo))

castForeignPtr :: ForeignPtr a -> ForeignPtr b
-- ^This function casts a 'ForeignPtr'
-- parameterised by one type into another type.
castForeignPtr (ForeignPtr a) = ForeignPtr a
castForeignPtr (MallocPtr  a) = MallocPtr  a
#endif

