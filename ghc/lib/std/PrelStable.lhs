% -----------------------------------------------------------------------------
% $Id: PrelStable.lhs,v 1.1 1999/01/26 12:25:01 simonm Exp $
%
% (c) The GHC Team, 1992-1999
%

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelStable 
	( StablePtr(..)
	, makeStablePtr	  -- :: a -> IO (StablePtr a)    
	, deRefStablePtr  -- :: StablePtr a -> a
	, freeStablePtr   -- :: StablePtr a -> IO ()
   ) where

import PrelBase
import PrelIOBase

-----------------------------------------------------------------------------
-- Stable Pointers

data StablePtr  a = StablePtr  (StablePtr#  a)

instance CCallable   (StablePtr a)
instance CCallable   (StablePtr# a)
instance CReturnable (StablePtr a)

makeStablePtr  :: a -> IO (StablePtr a)
deRefStablePtr :: StablePtr a -> IO a
freeStablePtr  :: StablePtr a -> IO ()

makeStablePtr a = IO $ \ s ->
    case makeStablePtr# a s of (# s', sp #) -> (# s', StablePtr sp #)

deRefStablePtr (StablePtr sp) = IO $ \s -> deRefStablePtr# sp s

freeStablePtr  sp = _ccall_ freeStablePtr sp

instance Eq (StablePtr a) where 
    (StablePtr sp1) == (StablePtr sp2) =
	case eqStablePtr# sp1 sp2 of
	   0# -> False
	   _  -> True
\end{code}
