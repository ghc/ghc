-----------------------------------------------------------------------------
-- |
-- Module      :  System.Mem.Weak
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- In general terms, a weak pointer is a reference to an object that is
-- not followed by the garbage collector - that is, the existence of a
-- weak pointer to an object has no effect on the lifetime of that
-- object.  A weak pointer can be de-referenced to find out
-- whether the object it refers to is still alive or not, and if so
-- to return the object itself.
-- 
-- Weak pointers are particularly useful for caches and memo tables.
-- To build a memo table, you build a data structure 
-- mapping from the function argument (the key) to its result (the
-- value).  When you apply the function to a new argument you first
-- check whether the key\/value pair is already in the memo table.
-- The key point is that the memo table itself should not keep the
-- key and value alive.  So the table should contain a weak pointer
-- to the key, not an ordinary pointer.  The pointer to the value must
-- not be weak, because the only reference to the value might indeed be
-- from the memo table.   
-- 
-- So it looks as if the memo table will keep all its values
-- alive for ever.  One way to solve this is to purge the table
-- occasionally, by deleting entries whose keys have died.
-- 
-- The weak pointers in this library
-- support another approach, called /finalization/.
-- When the key referred to by a weak pointer dies, the storage manager
-- arranges to run a programmer-specified finalizer.  In the case of memo
-- tables, for example, the finalizer could remove the key\/value pair
-- from the memo table.  
-- 
-- Another difficulty with the memo table is that the value of a
-- key\/value pair might itself contain a pointer to the key.
-- So the memo table keeps the value alive, which keeps the key alive,
-- even though there may be no other references to the key so both should
-- die.  The weak pointers in this library provide a slight 
-- generalisation of the basic weak-pointer idea, in which each
-- weak pointer actually contains both a key and a value.
--
-----------------------------------------------------------------------------

module System.Mem.Weak (
	-- * The @Weak@ type
	Weak,	    		-- abstract

	-- * The general interface
	mkWeak,      		-- :: k -> v -> Maybe (IO ()) -> IO (Weak v)
	deRefWeak, 		-- :: Weak v -> IO (Maybe v)
	finalize,		-- :: Weak v -> IO ()

	-- * Specialised versions
	mkWeakPtr, 		-- :: k -> Maybe (IO ()) -> IO (Weak k)
	addFinalizer, 		-- :: key -> IO () -> IO ()
	mkWeakPair, 		-- :: k -> v -> Maybe (IO ()) -> IO (Weak (k,v))
	-- replaceFinaliser	-- :: Weak v -> IO () -> IO ()

	-- * A precise semantics
	
	-- $precise
   ) where

import Prelude

import Data.Typeable

#ifdef __HUGS__
import Hugs.Weak
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Weak
#endif

-- | A specialised version of 'mkWeak', where the key and the value are
-- the same object:
--
-- > mkWeakPtr key finalizer = mkWeak key key finalizer
--
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
  'Data.IORef.mkWeakIORef' (the non-uniformity is accidental).
-}
addFinalizer :: key -> IO () -> IO ()
addFinalizer key finalizer = do
   mkWeakPtr key (Just finalizer)	-- throw it away
   return ()

-- | A specialised version of 'mkWeak' where the value is actually a pair
-- of the key and value passed to 'mkWeakPair':
--
-- > mkWeakPair key val finalizer = mkWeak key (key,val) finalizer
--
-- The advantage of this is that the key can be retrieved by 'deRefWeak'
-- in addition to the value.
mkWeakPair :: k -> v -> Maybe (IO ()) -> IO (Weak (k,v))
mkWeakPair key val finalizer = mkWeak key (key,val) finalizer


{- $precise

The above informal specification is fine for simple situations, but
matters can get complicated.  In particular, it needs to be clear
exactly when a key dies, so that any weak pointers that refer to it
can be finalized.  Suppose, for example, the value of one weak pointer
refers to the key of another...does that keep the key alive?

The behaviour is simply this:

 *  If a weak pointer (object) refers to an /unreachable/
    key, it may be finalized.

 *  Finalization means (a) arrange that subsequent calls
    to 'deRefWeak' return 'Nothing'; and (b) run the finalizer.

This behaviour depends on what it means for a key to be reachable.
Informally, something is reachable if it can be reached by following
ordinary pointers from the root set, but not following weak pointers.
We define reachability more precisely as follows A heap object is
reachable if:

 * It is a member of the /root set/.

 * It is directly pointed to by a reachable object, other than
   a weak pointer object.

 * It is a weak pointer object whose key is reachable.

 * It is the value or finalizer of an object whose key is reachable.
-}
