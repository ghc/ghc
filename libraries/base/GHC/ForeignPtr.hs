{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ForeignPtr
-- Copyright   :  (c) The University of Glasgow, 1992-2003
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- GHC's implementation of the 'ForeignPtr' data type.
-- 
-----------------------------------------------------------------------------

-- #hide
module GHC.ForeignPtr
  (
	ForeignPtr(..),
	FinalizerPtr,
	newForeignPtr_,
	mallocForeignPtr,
	mallocForeignPtrBytes,
	addForeignPtrFinalizer,	
	touchForeignPtr,
	unsafeForeignPtrToPtr,
	castForeignPtr,
	newConcForeignPtr,
	addForeignPtrConcFinalizer,
	finalizeForeignPtr
  ) where

import Control.Monad 	( sequence_ )
import Foreign.Ptr
import Foreign.Storable

import GHC.List  	( null )
import GHC.Base
import GHC.IOBase
import GHC.Ptr		( Ptr(..) )
import GHC.Err
import GHC.Show

-- |The type 'ForeignPtr' represents references to objects that are
-- maintained in a foreign language, i.e., that are not part of the
-- data structures usually managed by the Haskell storage manager.
-- The essential difference between 'ForeignPtr's and vanilla memory
-- references of type @Ptr a@ is that the former may be associated
-- with /finalizers/. A finalizer is a routine that is invoked when
-- the Haskell storage manager detects that - within the Haskell heap
-- and stack - there are no more references left that are pointing to
-- the 'ForeignPtr'.  Typically, the finalizer will, then, invoke
-- routines in the foreign language that free the resources bound by
-- the foreign object.
--
-- The 'ForeignPtr' is parameterised in the same way as 'Ptr'.  The
-- type argument of 'ForeignPtr' should normally be an instance of
-- class 'Storable'.
--
data ForeignPtr a 
  = ForeignPtr ForeignObj# !(IORef [IO ()])
  | MallocPtr (MutableByteArray# RealWorld) !(IORef [IO ()])

instance Eq (ForeignPtr a) where
    p == q  =  unsafeForeignPtrToPtr p == unsafeForeignPtrToPtr q

instance Ord (ForeignPtr a) where
    compare p q  =  compare (unsafeForeignPtrToPtr p) (unsafeForeignPtrToPtr q)

instance Show (ForeignPtr a) where
    showsPrec p f = showsPrec p (unsafeForeignPtrToPtr f)

-- |A Finalizer is represented as a pointer to a foreign function that, at
-- finalisation time, gets as an argument a plain pointer variant of the
-- foreign pointer that the finalizer is associated with.
-- 
type FinalizerPtr a = FunPtr (Ptr a -> IO ())

newConcForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
--
-- ^Turns a plain memory reference into a foreign object by
-- associating a finalizer - given by the monadic operation - with the
-- reference.  The storage manager will start the finalizer, in a
-- separate thread, some time after the last reference to the
-- @ForeignPtr@ is dropped.  There is no guarantee of promptness, and
-- in fact there is no guarantee that the finalizer will eventually
-- run at all.
--
newConcForeignPtr p finalizer
  = do fObj <- newForeignPtr_ p
       addForeignPtrConcFinalizer fObj finalizer
       return fObj

mallocForeignPtr :: Storable a => IO (ForeignPtr a)
-- ^ Allocate some memory and return a 'ForeignPtr' to it.  The memory
-- will be released automatically when the 'ForeignPtr' is discarded.
--
-- 'mallocForeignPtr' is equivalent to
--
-- >    do { p <- malloc; newForeignPtr finalizerFree p }
-- 
-- although it may be implemented differently internally: you may not
-- assume that the memory returned by 'mallocForeignPtr' has been
-- allocated with 'Foreign.Marshal.Alloc.malloc'.
mallocForeignPtr = doMalloc undefined
  where doMalloc :: Storable b => b -> IO (ForeignPtr b)
        doMalloc a = do
  	  r <- newIORef []
	  IO $ \s ->
	    case newPinnedByteArray# size s of { (# s, mbarr# #) ->
	     (# s, MallocPtr mbarr# r #)
            }
	    where (I# size) = sizeOf a

-- | This function is similar to 'mallocForeignPtr', except that the
-- size of the memory required is given explicitly as a number of bytes.
mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes (I# size) = do 
  r <- newIORef []
  IO $ \s ->
     case newPinnedByteArray# size s      of { (# s, mbarr# #) ->
       (# s, MallocPtr mbarr# r #)
     }

addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a -> IO ()
-- ^This function adds a finalizer to the given foreign object.  The
-- finalizer will run /before/ all other finalizers for the same
-- object which have already been registered.
addForeignPtrFinalizer finalizer fptr = 
  addForeignPtrConcFinalizer fptr 
	(mkFinalizer finalizer (unsafeForeignPtrToPtr fptr))

addForeignPtrConcFinalizer :: ForeignPtr a -> IO () -> IO ()
-- ^This function adds a finalizer to the given @ForeignPtr@.  The
-- finalizer will run /before/ all other finalizers for the same
-- object which have already been registered.
--
-- This is a variant of @addForeignPtrFinalizer@, where the finalizer
-- is an arbitrary @IO@ action.  When it is invoked, the finalizer
-- will run in a new thread.
--
-- NB. Be very careful with these finalizers.  One common trap is that
-- if a finalizer references another finalized value, it does not
-- prevent that value from being finalized.  In particular, 'Handle's
-- are finalized objects, so a finalizer should not refer to a 'Handle'
-- (including @stdout@, @stdin@ or @stderr@).
--
addForeignPtrConcFinalizer f@(ForeignPtr fo r) finalizer = do
  fs <- readIORef r
  writeIORef r (finalizer : fs)
  if (null fs)
     then IO $ \s ->
	      let p = unsafeForeignPtrToPtr f in
	      case mkWeak# fo () (foreignPtrFinalizer r p) s of 
		 (# s1, w #) -> (# s1, () #)
     else return ()
addForeignPtrConcFinalizer f@(MallocPtr fo r) finalizer = do 
  fs <- readIORef r
  writeIORef r (finalizer : fs)
  if (null fs)
     then  IO $ \s -> 
	       let p = unsafeForeignPtrToPtr f in
	       case mkWeak# fo () (do foreignPtrFinalizer r p
				      touchPinnedByteArray# fo) s of 
		  (# s1, w #) -> (# s1, () #)
     else return ()

foreign import ccall "dynamic" 
  mkFinalizer :: FinalizerPtr a -> Ptr a -> IO ()

foreignPtrFinalizer :: IORef [IO ()] -> Ptr a -> IO ()
foreignPtrFinalizer r p = do
  fs <- readIORef r
  sequence_ fs

newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
-- ^Turns a plain memory reference into a foreign pointer that may be
-- associated with finalizers by using 'addForeignPtrFinalizer'.
newForeignPtr_ (Ptr obj) =  do
  r <- newIORef []
  IO $ \ s# ->
    case mkForeignObj# obj s# of
      (# s1#, fo# #) -> (# s1#,  ForeignPtr fo# r #)

touchPinnedByteArray# :: MutableByteArray# RealWorld -> IO ()
touchPinnedByteArray# ba# = IO $ \s -> case touch# ba# s of s -> (# s, () #)

touchForeignPtr :: ForeignPtr a -> IO ()
-- ^This function ensures that the foreign object in
-- question is alive at the given place in the sequence of IO
-- actions. In particular 'Foreign.ForeignPtr.withForeignPtr'
-- does a 'touchForeignPtr' after it
-- executes the user action.
-- 
-- Note that this function should not be used to express liveness
-- dependencies between 'ForeignPtr's.  For example, if the finalizer
-- for a 'ForeignPtr' @F1@ calls 'touchForeignPtr' on a second
-- 'ForeignPtr' @F2@, then the only guarantee is that the finalizer
-- for @F2@ is never started before the finalizer for @F1@.  They
-- might be started together if for example both @F1@ and @F2@ are
-- otherwise unreachable, and in that case the scheduler might end up
-- running the finalizer for @F2@ first.
--
-- In general, it is not recommended to use finalizers on separate
-- objects with ordering constraints between them.  To express the
-- ordering robustly requires explicit synchronisation using @MVar@s
-- between the finalizers, but even then the runtime sometimes runs
-- multiple finalizers sequentially in a single thread (for
-- performance reasons), so synchronisation between finalizers could
-- result in artificial deadlock.
--
touchForeignPtr (ForeignPtr fo r)
   = IO $ \s -> case touch# fo s of s -> (# s, () #)
touchForeignPtr (MallocPtr fo r)
   = touchPinnedByteArray# fo

unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
-- ^This function extracts the pointer component of a foreign
-- pointer.  This is a potentially dangerous operations, as if the
-- argument to 'unsafeForeignPtrToPtr' is the last usage
-- occurrence of the given foreign pointer, then its finalizer(s) will
-- be run, which potentially invalidates the plain pointer just
-- obtained.  Hence, 'touchForeignPtr' must be used
-- wherever it has to be guaranteed that the pointer lives on - i.e.,
-- has another usage occurrence.
--
-- To avoid subtle coding errors, hand written marshalling code
-- should preferably use 'Foreign.ForeignPtr.withForeignPtr' rather
-- than combinations of 'unsafeForeignPtrToPtr' and
-- 'touchForeignPtr'.  However, the later routines
-- are occasionally preferred in tool generated marshalling code.
unsafeForeignPtrToPtr (ForeignPtr fo r) = Ptr (foreignObjToAddr# fo)
unsafeForeignPtrToPtr (MallocPtr  fo r) = Ptr (byteArrayContents# (unsafeCoerce# fo))

castForeignPtr :: ForeignPtr a -> ForeignPtr b
-- ^This function casts a 'ForeignPtr'
-- parameterised by one type into another type.
castForeignPtr f = unsafeCoerce# f

-- | Causes the finalizers associated with a foreign pointer to be run
-- immediately.
finalizeForeignPtr :: ForeignPtr a -> IO ()
finalizeForeignPtr foreignPtr = do
	finalizers <- readIORef refFinalizers
	sequence_ finalizers
	writeIORef refFinalizers []
	where
		refFinalizers = case foreignPtr of
			(ForeignPtr _ ref) -> ref
			(MallocPtr  _ ref) -> ref
