\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Weak
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Weak pointers.
--
-----------------------------------------------------------------------------

module GHC.Weak where

import GHC.Base
import Data.Maybe
import GHC.IOBase	( IO(..), unIO )

{-|
A weak pointer object with a key and a value.  The value has type @v@.

A weak pointer expresses a relationship between two objects, the
/key/ and the /value/:  if the key is considered to be alive by the
garbage collector, then the value is also alive.  A reference from
the value to the key does /not/ keep the key alive.

A weak pointer may also have a finalizer of type @IO ()@; if it does,
then the finalizer will be run once, and once only, at a time after
the key has become unreachable by the program (\"dead\").  The storage
manager attempts to run the finalizer(s) for an object soon after the
object dies, but promptness is not guaranteed.  

References from the finalizer to the key are treated in the same way
as references from the value to the key: they do not keep the key
alive.  A finalizer may therefore ressurrect the key, perhaps by
storing it in the same data structure.

The finalizer, and the relationship between the key and the value,
exist regardless of whether the program keeps a reference to the
'Weak' object or not.

There may be multiple weak pointers with the same key.  In this
case, the finalizers for each of these weak pointers will all be
run in some arbitrary order, or perhaps concurrently, when the key
dies.  If the programmer specifies a finalizer that assumes it has
the only reference to an object (for example, a file that it wishes
to close), then the programmer must ensure that there is only one
such finalizer.

If there are no other threads to run, the runtime system will check
for runnable finalizers before declaring the system to be deadlocked.
-}
data Weak v = Weak (Weak# v)

-- | Establishes a weak pointer to @k@, with value @v@ and a finalizer.
--
-- This is the most general interface for building a weak pointer.
--
mkWeak  :: k				-- ^ key
	-> v				-- ^ value
	-> Maybe (IO ())		-- ^ finalizer
	-> IO (Weak v)			-- ^ returns: a weak pointer object

mkWeak key val (Just finalizer) = IO $ \s ->
   case mkWeak# key val finalizer s of { (# s1, w #) -> (# s1, Weak w #) }
mkWeak key val Nothing = IO $ \s ->
   case mkWeak# key val (unsafeCoerce# 0#) s of { (# s1, w #) -> (# s1, Weak w #) }

{-|
  A specialised version of 'mkWeak', where the key and the value are the
  same object:

  > mkWeakPtr key finalizer = mkWeak key key finalizer
-}
mkWeakPtr :: k -> Maybe (IO ()) -> IO (Weak k)
mkWeakPtr key finalizer = mkWeak key key finalizer

{-|
  A specialised version of 'mkWeakPtr', where the 'Weak' object
  returned is simply thrown away (however the finalizer will be
  remembered by the garbage collector, and will still be run
  when the key becomes unreachable).

  Note: adding a finalizer to a 'Foreign.ForeignPtr.ForeignPtr' using
  'addFinalizer' won't work as well as using the specialised version
  'Foreign.ForeignPtr.addForeignPtrFinalizer' because the latter
  version adds the finalizer to the primitive 'ForeignPtr#' object
  inside, whereas the generic 'addFinalizer' will add the finalizer to
  the box.  Optimisations tend to remove the box, which may cause the
  finalizer to run earlier than you intended.  The same motivation
  justifies the existence of
  'Control.Concurrent.MVar.addMVarFinalizer' and
  'Data.IORef.mkWeakIORef' (the non-unformity is accidental).
-}
addFinalizer :: key -> IO () -> IO ()
addFinalizer key finalizer = do
   mkWeakPtr key (Just finalizer)	-- throw it away
   return ()

{-
Instance Eq (Weak v) where
  (Weak w1) == (Weak w2) = w1 `sameWeak#` w2
-}


-- run a batch of finalizers from the garbage collector.  We're given 
-- an array of finalizers and the length of the array, and we just
-- call each one in turn.
--
-- the IO primitives are inlined by hand here to get the optimal
-- code (sigh) --SDM.

runFinalizerBatch :: Int -> Array# (IO ()) -> IO ()
runFinalizerBatch (I# n) arr = 
   let  go m  = IO $ \s ->
		  case m of 
		  0# -> (# s, () #)
		  _  -> let m' = m -# 1# in
			case indexArray# arr m' of { (# io #) -> 
			case unIO io s of	   { (# s, _ #) -> 
			unIO (go m') s
			}}
   in
        go n

\end{code}
