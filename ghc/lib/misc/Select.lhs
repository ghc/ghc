%
% (c) sof, 1999
%

Haskell wrapper for select() OS functionality. It's use
shouldn't be all that common in a Haskell system that implements
IO in such a way that's thread friendly, but still.

\begin{code}
{-# OPTIONS -#include "cbits/selectFrom.h" #-}
module Select
    (
      hSelect     -- :: [Handle]
                  -- -> [Handle]
		  -- -> [Handle]
		  -- -> TimeOut
		  -- -> IO SelectResult
    , TimeOut(..) -- type _ = Maybe Int
    , SelectResult(..) 
    ) where

import Posix
import GlaExts
import IO
import Monad
import Maybe
import PrelIOBase
import PosixUtil (fdToInt)
\end{code}

This stuff should really be done using HDirect.

\begin{code}
type TimeOut
 = Maybe Int
    -- Nothing => wait indefinitely.
    -- Just x | x >= 0    => block waiting for 'x' micro seconds.
    --        | otherwise => block waiting for '-x' micro seconds.

type SelectResult
 = ([Handle], [Handle], [Handle])

hSelect :: [Handle]  -- input/read handles
        -> [Handle]  -- output/write handles
	-> [Handle]  -- exceptional handles
	-> TimeOut
	-> IO SelectResult
hSelect ins outs excps timeout = do
     ins_         <- mapM getFd ins
     outs_        <- mapM getFd outs
     excps_       <- mapM getFd excps
     (max_in,  fds_ins)   <- marshallFDs ins_
     (max_out, fds_outs)  <- marshallFDs outs_
     (max_excp,fds_excps) <- marshallFDs excps_
     tout                 <- marshallTimeout timeout
     let max_fd = max_in `max` max_out `max` max_excp
     rc                <- selectFrom__ fds_ins
     				       fds_outs
				       fds_excps
				       (max_fd+1) tout
     if (rc /= 0)
      then constructErrorAndFail "hSelect"
      else
         let 
	   -- thunk these so that we only pay unmarshalling costs if demanded.
	  ins_ready   = unsafePerformIO (getReadyOnes fds_ins ins_)
          outs_ready  = unsafePerformIO (getReadyOnes fds_outs outs_)
          excps_ready = unsafePerformIO (getReadyOnes fds_outs outs_)
	 in
	 return (ins_ready, outs_ready, excps_ready)

getFd :: Handle -> IO (Fd,Handle)
getFd h = do
  f <- handleToFd h
  return (f,h)

foreign import "selectFrom__" unsafe
		selectFrom__ :: ByteArray Int
			     -> ByteArray Int
			     -> ByteArray Int
			     -> Int
			     -> Int
			     -> IO Int

marshallTimeout :: Maybe Int -> IO Int
marshallTimeout Nothing  = return (-1)
marshallTimeout (Just x) = return (abs x)

getReadyOnes :: ByteArray Int -> [(Fd,Handle)] -> IO [Handle]
getReadyOnes ba ls = do
  xs <- mapM isReady ls
  return (catMaybes xs)
 where
  isReady (f,h) = do
     let fi = fdToInt f
     flg <- is_fd_set ba fi
     if (flg /= 0) then
        return (Just h)
      else 
        return Nothing

marshallFDs :: [(Fd,Handle)] -> IO (Int, ByteArray Int)
marshallFDs ls = do
  ba <- stToIO (newCharArray (0, sizeof_fd_set))
  fd_zero ba
  let
   fillIn acc (f,_) = do
     let fi = fdToInt f
     fd_set ba fi
     return (max acc fi)
  x  <- foldM fillIn 0 ls
  ba <- stToIO (unsafeFreezeByteArray ba)
  return (x, ba)

foreign import "is_fd_set__" unsafe
	       is_fd_set :: ByteArray Int -> Int -> IO Int

foreign import "fd_zero__" unsafe
	       fd_zero :: MutableByteArray RealWorld Int -> IO ()

foreign import "fd_set__" unsafe
	       fd_set :: MutableByteArray RealWorld Int -> Int -> IO ()

foreign import "sizeof_fd_set__" unsafe
	       sizeof_fd_set :: Int

\end{code}
