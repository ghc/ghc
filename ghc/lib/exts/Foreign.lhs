%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Foreign]{Module @Foreign@}

\begin{code}
module Foreign 
       ( 
	 ForeignObj      -- abstract, instance of: Eq
       , makeForeignObj  -- :: Addr{-obj-} -> Addr{-finaliser-} -> IO ForeignObj
       , writeForeignObj -- :: ForeignObj  -> Addr{-new obj-}   -> IO ()
       
       , StablePtr {-a-} -- abstract.
       , makeStablePtr   -- :: a -> IO (StablePtr a)
       , deRefStablePtr  -- :: StablePtr a -> IO a
       , freeStablePtr   -- :: StablePtr a -> IO ()

       , indexCharOffForeignObj
       , indexAddrOffForeignObj
       , indexIntOffForeignObj
       , indexFloatOffForeignObj
       , indexDoubleOffForeignObj
       , readCharOffForeignObj
       , readAddrOffForeignObj
       , readIntOffForeignObj
       , readFloatOffForeignObj
       , readDoubleOffForeignObj
       , writeCharOffForeignObj
       , writeAddrOffForeignObj
       , writeIntOffForeignObj
       , writeFloatOffForeignObj
       , writeDoubleOffForeignObj
        
       , indexWord8OffForeignObj
       , indexWord16OffForeignObj
       , indexWord32OffForeignObj
       , indexWord64OffForeignObj
       , readWord8OffForeignObj
       , readWord16OffForeignObj
       , readWord32OffForeignObj
       , readWord64OffForeignObj
       , writeWord8OffForeignObj
       , writeWord16OffForeignObj
       , writeWord32OffForeignObj
       , writeWord64OffForeignObj

       , indexInt8OffForeignObj
       , indexInt16OffForeignObj
       , indexInt32OffForeignObj
       , indexInt64OffForeignObj
       , readInt8OffForeignObj
       , readInt16OffForeignObj
       , readInt32OffForeignObj
       , readInt64OffForeignObj
       , writeInt8OffForeignObj
       , writeInt16OffForeignObj
       , writeInt32OffForeignObj
       , writeInt64OffForeignObj

       ) where

import PrelForeign
import PrelBase    ( Int(..), Double(..), Float(..), Char(..) )
import PrelGHC     ( indexCharOffForeignObj#, indexIntOffForeignObj#, 
		     indexAddrOffForeignObj#, indexFloatOffForeignObj#, 
		     indexDoubleOffForeignObj#
		   )
import PrelAddr    ( Addr(..) )
import Word 
   ( 
     indexWord8OffForeignObj
   , indexWord16OffForeignObj
   , indexWord32OffForeignObj
   , indexWord64OffForeignObj
   , readWord8OffForeignObj
   , readWord16OffForeignObj
   , readWord32OffForeignObj
   , readWord64OffForeignObj
   , writeWord8OffForeignObj
   , writeWord16OffForeignObj
   , writeWord32OffForeignObj
   , writeWord64OffForeignObj
   )

import Int
   ( 
     indexInt8OffForeignObj
   , indexInt16OffForeignObj
   , indexInt32OffForeignObj
   , indexInt64OffForeignObj
   , readInt8OffForeignObj
   , readInt16OffForeignObj
   , readInt32OffForeignObj
   , readInt64OffForeignObj
   , writeInt8OffForeignObj
   , writeInt16OffForeignObj
   , writeInt32OffForeignObj
   , writeInt64OffForeignObj
   )
import PrelIOBase ( IO(..), IOResult(..) )
\end{code}

\begin{code}
indexCharOffForeignObj   :: ForeignObj -> Int -> Char
indexCharOffForeignObj (ForeignObj fo#) (I# i#) = C# (indexCharOffForeignObj# fo# i#)

indexIntOffForeignObj    :: ForeignObj -> Int -> Int
indexIntOffForeignObj (ForeignObj fo#) (I# i#) = I# (indexIntOffForeignObj# fo# i#)

indexAddrOffForeignObj   :: ForeignObj -> Int -> Addr
indexAddrOffForeignObj (ForeignObj fo#) (I# i#) = A# (indexAddrOffForeignObj# fo# i#)

indexFloatOffForeignObj  :: ForeignObj -> Int -> Float
indexFloatOffForeignObj (ForeignObj fo#) (I# i#) = F# (indexFloatOffForeignObj# fo# i#)

indexDoubleOffForeignObj :: ForeignObj -> Int -> Double
indexDoubleOffForeignObj (ForeignObj fo#) (I# i#) = D# (indexDoubleOffForeignObj# fo# i#)

-- read value out of mutable memory
readCharOffForeignObj    :: ForeignObj -> Int -> IO Char
readCharOffForeignObj fo i = _casm_ `` %r=(StgChar)(((StgChar*)%0)[(StgInt)%1]); '' fo i

readIntOffForeignObj     :: ForeignObj -> Int -> IO Int
readIntOffForeignObj fo i = _casm_ `` %r=(StgInt)(((StgInt*)%0)[(StgInt)%1]); '' fo i

readWordOffForeignObj     :: ForeignObj -> Int -> IO Word
readWordOffForeignObj fo i = _casm_ `` %r=(StgWord)(((StgWord*)%0)[(StgInt)%1]); '' fo i

readAddrOffForeignObj    :: ForeignObj -> Int -> IO Addr
readAddrOffForeignObj fo i = _casm_ `` %r=(StgAddr)(((StgAddr*)%0)[(StgInt)%1]); '' fo i

readFloatOffForeignObj   :: ForeignObj -> Int -> IO Float
readFloatOffForeignObj fo i = _casm_ `` %r=(StgFloat)(((StgFloat*)%0)[(StgInt)%1]); '' fo i

readDoubleOffForeignObj  :: ForeignObj -> Int -> IO Double
readDoubleOffForeignObj fo i = _casm_ `` %r=(StgDouble)(((StgDouble*)%0)[(StgInt)%1]); '' fo i
\end{code}

\begin{code}
writeCharOffForeignObj   :: ForeignObj -> Int -> Char   -> IO ()
writeCharOffForeignObj fo i e = _casm_ `` (((StgChar*)%0)[(StgInt)%1])=(StgChar)%2; '' fo i e

writeIntOffForeignObj    :: ForeignObj -> Int -> Int    -> IO ()
writeIntOffForeignObj fo i e = _casm_ `` (((StgInt*)%0)[(StgInt)%1])=(StgInt)%2; '' fo i e

writeWordOffForeignObj    :: ForeignObj -> Int -> Word  -> IO ()
writeWordOffForeignObj fo i e = _casm_ `` (((StgWord*)%0)[(StgInt)%1])=(StgWord)%2; '' fo i e

writeAddrOffForeignObj   :: ForeignObj -> Int -> Addr   -> IO ()
writeAddrOffForeignObj fo i e = _casm_ `` (((StgAddr*)%0)[(StgInt)%1])=(StgAddr)%2; ''fo i e

writeFloatOffForeignObj  :: ForeignObj -> Int -> Float  -> IO ()
writeFloatOffForeignObj fo i e = _casm_ `` (((StgFloat*)%0)[(StgInt)%1])=(StgFloat)%2; '' fo i e

writeDoubleOffForeignObj :: ForeignObj -> Int -> Double -> IO ()
writeDoubleOffForeignObj fo i e = _casm_ `` (((StgDouble*)%0)[(StgInt)%1])=(StgDouble)%2; '' fo i e

\end{code}
