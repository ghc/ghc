% $Id: Memo.lhs,v 1.1 2005/12/16 10:46:05 simonmar Exp $
%
% (c) The GHC Team, 1999
%
% Hashing memo tables.

\begin{code}
{-# LANGUAGE CPP #-}

module Memo2
#ifndef __PARALLEL_HASKELL__
	( memo  	-- :: (a -> b) -> a -> b
	, memoSized 	-- :: Int -> (a -> b) -> a -> b
 	) 
#endif
	where

#ifndef __PARALLEL_HASKELL__

import System.Mem.StableName	( StableName, makeStableName, hashStableName )
import System.Mem.Weak		( Weak, mkWeakPtr, mkWeak, deRefWeak, finalize )
import Data.Array.IO		( IOArray, newArray, readArray, writeArray )
import System.IO.Unsafe		( unsafePerformIO )
import Control.Concurrent.MVar	( MVar, newMVar, putMVar, takeMVar )
\end{code}

-----------------------------------------------------------------------------
Memo table representation.

The representation is this: a fixed-size hash table where each bucket
is a list of table entries, of the form (key,value).

The key in this case is (StableName key), and we use hashStableName to
hash it.

It's important that we can garbage collect old entries in the table
when the key is no longer reachable in the heap.  Hence the value part
of each table entry is (Weak val), where the weak pointer "key" is the
key for our memo table, and 'val' is the value of this memo table
entry.  When the key becomes unreachable, a finalizer will fire and
remove this entry from the hash bucket, and further attempts to
dereference the weak pointer will return Nothing.  References from
'val' to the key are ignored (see the semantics of weak pointers in
the documentation).

\begin{code}
type MemoTable key val
	= MVar (
	    Int,	-- current table size
	    IOArray Int [MemoEntry key val]   -- hash table
	   )

-- a memo table entry: compile with -funbox-strict-fields to eliminate
-- the boxes around the StableName and Weak fields.
data MemoEntry key val = MemoEntry !(StableName key) !(Weak val)
\end{code}

We use an MVar to the hash table, so that several threads may safely
access it concurrently.  This includes the finalization threads that
remove entries from the table.

ToDo: Can efficiency be improved at all?

\begin{code}
memo :: (a -> b) -> a -> b
memo f = memoSized default_table_size f

default_table_size = 1001

-- Our memo functions are *strict*.  Lazy memo functions tend to be
-- less useful because it is less likely you'll get a memo table hit
-- for a thunk.  This change was made to match Hugs's Memo
-- implementation, and as the result of feedback from Conal Elliot
-- <conal@microsoft.com>.

memoSized :: Int -> (a -> b) -> a -> b
memoSized size f = strict (lazyMemoSized size f)

strict = ($!)

lazyMemoSized :: Int -> (a -> b) -> a -> b
lazyMemoSized size f =
   let (table,weak) = unsafePerformIO (
		do { tbl <- newArray (0,size) []
		   ; mvar <- newMVar (size,tbl)
		   ; weak <- mkWeakPtr mvar (Just (table_finalizer tbl size))
		   ; return (mvar,weak)
		   })
   in  memo' f table weak

table_finalizer :: IOArray Int [MemoEntry key val] -> Int -> IO ()
table_finalizer table size = 
   sequence_ [ finalizeBucket i | i <- [0..size] ]
 where
   finalizeBucket i = do
      bucket <- readArray table i 
      sequence_ [ finalize w | MemoEntry _ w <- bucket ]

memo' :: (a -> b) -> MemoTable a b -> Weak (MemoTable a b) -> a -> b
memo' f ref weak_ref = \k -> unsafePerformIO $ do
   stable_key <- makeStableName k
   (size, table) <- takeMVar ref
   let hash_key = hashStableName stable_key `mod` size
   bucket <- readArray table hash_key
   lkp <- lookupSN stable_key bucket

   case lkp of
     Just result -> do
	putMVar ref (size,table)
	return result
     Nothing -> do
	let result = f k
	weak <- mkWeak k result (Just (finalizer hash_key stable_key weak_ref))
	writeArray table hash_key (MemoEntry stable_key weak : bucket)
	putMVar ref (size,table)
	return result

finalizer :: Int -> StableName a -> Weak (MemoTable a b) -> IO ()
finalizer hash_key stable_key weak_ref = 
  do r <- deRefWeak weak_ref 
     case r of
	Nothing -> return ()
	Just mvar -> do
        	(size,table) <- takeMVar mvar
		bucket <- readArray table hash_key
		let new_bucket = [ e | e@(MemoEntry sn weak) <- bucket, 
				       sn /= stable_key ]
		writeArray table hash_key new_bucket
		putMVar mvar (size,table)

lookupSN :: StableName key -> [MemoEntry key val] -> IO (Maybe val)
lookupSN sn [] = sn `seq` return Nothing -- make it strict in sn
lookupSN sn (MemoEntry sn' weak : xs)
   | sn == sn'  = do maybe_item <- deRefWeak weak
		     case maybe_item of
			Nothing -> error ("dead weak pair: " ++ 
						show (hashStableName sn))
			Just v  -> return (Just v)
   | otherwise  = lookupSN sn xs
#endif
\end{code}
