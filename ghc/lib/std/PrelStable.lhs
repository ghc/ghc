% -----------------------------------------------------------------------------
% $Id: PrelStable.lhs,v 1.9 2001/03/25 09:57:26 qrczak Exp $
%
% (c) The GHC Team, 1992-2000
%

\section{Module @PrelStable@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelStable 
	( StablePtr(..)
	, newStablePtr    -- :: a -> IO (StablePtr a)    
	, deRefStablePtr  -- :: StablePtr a -> a
	, freeStablePtr   -- :: StablePtr a -> IO ()
   ) where

import PrelBase
import PrelIOBase

-----------------------------------------------------------------------------
-- Stable Pointers

data StablePtr a = StablePtr (StablePtr# a)

instance CCallable   (StablePtr a)
instance CReturnable (StablePtr a)

newStablePtr   :: a -> IO (StablePtr a)
newStablePtr a = IO $ \ s ->
    case makeStablePtr# a s of (# s', sp #) -> (# s', StablePtr sp #)

deRefStablePtr :: StablePtr a -> IO a
deRefStablePtr (StablePtr sp) = IO $ \s -> deRefStablePtr# sp s

foreign import unsafe freeStablePtr :: StablePtr a -> IO ()

instance Eq (StablePtr a) where 
    (StablePtr sp1) == (StablePtr sp2) =
	case eqStablePtr# sp1 sp2 of
	   0# -> False
	   _  -> True
\end{code}
