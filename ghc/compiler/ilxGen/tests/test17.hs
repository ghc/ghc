{-# OPTIONS -fno-implicit-prelude #-}

module Test17 where

import PrelGHC
import PrelBase

data Exception = IOException IOError | OtherExc

data IOError 
 = IOError 
     String      

tthrow :: Exception -> a

tthrow exception = raise# exception
ccatchException (IO m) k =  IO (\s -> catch# m (\ex -> unIO (k ex)) s)


ccatch           :: IO a -> (IOError -> IO a) -> IO a 
ccatch m k	=  ccatchException m handler
  where handler (IOException err) = k err
	handler other             = tthrow other

ccatchNonIO      :: IO a -> (Exception -> IO a) -> IO a 
ccatchNonIO m k	=  ccatchException m handler
  where handler (IOException err) = ioError err
	handler other             = k other

newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a

ioError         :: IOError -> IO a 
ioError err	=  IO (\s -> tthrow (IOException err) s)



blockAsyncExceptions :: IO a -> IO a
blockAsyncExceptions (IO io) = IO (blockAsyncExceptions# io)

unblockAsyncExceptions :: IO a -> IO a
unblockAsyncExceptions (IO io) = IO (unblockAsyncExceptions# io)
