% $Id: Memo.lhs,v 1.3 1999/02/26 17:43:55 simonm Exp $
%
% (c) The GHC Team, 1999
%
% Hashing memo tables.

\begin{code}
{-# OPTIONS -fglasgow-exts #-}

module Memo
	( memo  	-- :: (a -> b) -> a -> b
	, memo_sized 	-- :: Int -> (a -> b) -> a -> b
 	) where

import Stable
import Weak
import IO
import IOExts
import Concurrent
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
	    IOArray Int [(StableName key, Weak val)]   -- hash table
	   )
\end{code}

We use an MVar to the hash table, so that several threads may safely
access it concurrently.  This includes the finalization threads that
remove entries from the table.

ToDo: make the finalizers refer to the memo table only through a weak
pointer, because otherwise the memo table will keep itself alive
(i.e. even after the function is dead, the weak pointers in the memo
table stay alive because their keys are alive, and hence the values
and finalizers are alive, therefore the table itself stays alive.
Bad).

\begin{code}
memo :: (a -> b) -> a -> b
memo f = memo_sized default_table_size f

default_table_size = 1001

memo_sized :: Int -> (a -> b) -> a -> b
memo_sized size f =
   let (table,weak) = unsafePerformIO (
		do { tbl <- newIOArray (0,1001) []
		   ; mvar <- newMVar (size,tbl)
		   ; weak <- mkWeakPtr mvar (Just (table_finalizer tbl size))
		   ; return (mvar,weak)
		   })
   in  memo' f table weak

table_finalizer :: IOArray Int [(StableName key, Weak val)] -> Int -> IO ()
table_finalizer table size = 
   sequence_ [ finalizeBucket i | i <- [0..size] ]
 where
   finalizeBucket i = do
      bucket <- readIOArray table i 
      sequence_ [ finalize w | (_,w) <- bucket ]

memo' :: (a -> b) -> MemoTable a b -> Weak (MemoTable a b) -> a -> b
memo' f ref weak_ref = \k -> unsafePerformIO $ do
   stable_key <- makeStableName k
   (size, table) <- takeMVar ref
   let hash_key = hashStableName stable_key `mod` size
   bucket <- readIOArray table hash_key
   lkp <- lookupSN stable_key bucket

   case lkp of
     Just result -> do
	putMVar ref (size,table)
	return result
     Nothing -> do
	let result = f k
	weak <- mkWeak k result (Just (finalizer hash_key stable_key weak_ref))
	writeIOArray table hash_key ((stable_key,weak):bucket)
	putMVar ref (size,table)
	return result

finalizer :: Int -> StableName a -> Weak (MemoTable a b) -> IO ()
finalizer hash_key stable_key weak_ref = 
  do r <- deRefWeak weak_ref 
     case r of
	Nothing -> return ()
	Just mvar -> do
        	(size,table) <- takeMVar mvar
		bucket <- readIOArray table hash_key
		let new_bucket = [ (sn,weak) 
				 | (sn,weak) <- bucket, 
				   sn /= stable_key ]
		writeIOArray table hash_key new_bucket
		putMVar mvar (size,table)

lookupSN :: StableName key -> [(StableName key, Weak val)] -> IO (Maybe val)
lookupSN sn [] = return Nothing
lookupSN sn ((sn',weak) : xs)
   | sn == sn'  = do maybe_item <- deRefWeak weak
		     case maybe_item of
			Nothing -> error ("dead weak pair: " ++ 
						show (hashStableName sn))
			Just v  -> return (Just v)
   | otherwise  = lookupSN sn xs
\end{code}
