\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Stable
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Stable pointers.
--
-----------------------------------------------------------------------------

module GHC.Stable 
	( StablePtr(..)
	, newStablePtr		-- :: a -> IO (StablePtr a)    
	, deRefStablePtr	-- :: StablePtr a -> a
	, freeStablePtr   	-- :: StablePtr a -> IO ()
	, castStablePtrToPtr 	-- :: StablePtr a -> Ptr ()
	, castPtrToStablePtr	-- :: Ptr () -> StablePtr a
   ) where

import GHC.Ptr
import GHC.Base
import GHC.IOBase

-----------------------------------------------------------------------------
-- Stable Pointers

data StablePtr a = StablePtr (StablePtr# a)

newStablePtr   :: a -> IO (StablePtr a)
newStablePtr a = IO $ \ s ->
    case makeStablePtr# a s of (# s', sp #) -> (# s', StablePtr sp #)

deRefStablePtr :: StablePtr a -> IO a
deRefStablePtr (StablePtr sp) = IO $ \s -> deRefStablePtr# sp s

foreign import ccall unsafe freeStablePtr :: StablePtr a -> IO ()

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
