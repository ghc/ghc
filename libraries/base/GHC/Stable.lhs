% -----------------------------------------------------------------------------
% $Id: Stable.lhs,v 1.1 2001/06/28 14:15:03 simonmar Exp $
%
% (c) The GHC Team, 1992-2000
%

\section{Module @GHC.Stable@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module GHC.Stable 
	( StablePtr(..)
	, newStablePtr		-- :: a -> IO (StablePtr a)    
	, deRefStablePtr	-- :: StablePtr a -> a
	, freeStablePtr   	-- :: StablePtr a -> IO ()
	, castStablePtrToPtr 	-- :: StablePtr a -> Ptr ()
	, castPtrToStablePtr	-- :: Ptr () -> StablePtr a
   ) where

import Foreign.Ptr

import GHC.Base
import GHC.IOBase

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

castStablePtrToPtr :: StablePtr a -> Ptr ()
castStablePtrToPtr (StablePtr s) = Ptr (unsafeCoerce# s)

castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr (Ptr a) = StablePtr (unsafeCoerce# a)

instance Eq (StablePtr a) where 
    (StablePtr sp1) == (StablePtr sp2) =
	case eqStablePtr# sp1 sp2 of
	   0# -> False
	   _  -> True
\end{code}
