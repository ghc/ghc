%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[GlaExts]{The @GlaExts@ interface}

The @GlaExts@ packages up various Glasgow extensions and
exports them all through one interface. The Idea being that
a Haskell program using a Glasgow extension doesn't have to
selective import of obscure/likely-to-move (believe me, we
really like to move functions around for the prelude bits!)
GHC interfaces - instead import the GlaExts rag bag and you should be away!

\begin{code}
module GlaExts

       (
        -- From module STBase, the PrimIO monad 
        -- (an instance of ST):
	PrimIO,
        ST, RealWorld,
        module Monad,   -- ST is an instance
        fixPrimIO, unsafePerformPrimIO, unsafeInterleavePrimIO,
        returnPrimIO, thenPrimIO, seqPrimIO,
        
        listPrimIO, mapPrimIO, mapAndUnzipPrimIO,

        -- operations for interfacing IO and ST/PrimIO
        --
        stToIO,       --:: ST RealWorld a -> IO a
	primIOToIO,   --:: PrimIO a       -> IO a
	ioToST,	      --:: IO a -> ST RealWorld a
	ioToPrimIO,   --:: IO a -> PrimIO       a
        thenIO_Prim,  -- :: PrimIO a -> (a -> IO b) -> IO b

        -- Everything from module ByteArray:
	module ByteArray,

        -- Same for Mutable(Byte)Array interface:
	module MutableArray,
	
        -- the representation of some basic types:
        Int(..),Addr(..),Word(..),Float(..),Double(..),Integer(..),

        -- misc bits
	trace,

        -- and finally, all the unboxed primops of GHC!
        module GHC

       ) where

import GHC
import STBase
import PrelBase
import ByteArray
import MutableArray
import Monad
import IOBase

\end{code}
