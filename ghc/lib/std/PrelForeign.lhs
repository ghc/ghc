% ------------------------------------------------------------------------------
% $Id: PrelForeign.lhs,v 1.20 2001/07/16 00:39:04 sof Exp $
%
% (c) The University of Glasgow, 1994-2000
%

\section[Foreign]{Module @Foreign@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelForeign where

import PrelIOBase
import PrelNum			-- for fromInteger
import PrelBase
import PrelPtr
\end{code}

%*********************************************************
%*							*
\subsection{ForeignPtr}
%*							*
%*********************************************************

\begin{code}
data ForeignPtr a = ForeignPtr ForeignObj#
instance CCallable (ForeignPtr a)

eqForeignPtr  :: ForeignPtr a -> ForeignPtr a -> Bool
eqForeignPtr (ForeignPtr fo1#) (ForeignPtr fo2#) = eqForeignObj# fo1# fo2#

instance Eq (ForeignPtr a) where 
    p == q = eqForeignPtr p q
    p /= q = not (eqForeignPtr p q)

newForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
newForeignPtr p finalizer
  = do fObj <- mkForeignPtr p
       addForeignPtrFinalizer fObj finalizer
       return fObj

addForeignPtrFinalizer :: ForeignPtr a -> IO () -> IO ()
addForeignPtrFinalizer (ForeignPtr fo) finalizer = 
  IO $ \s -> case mkWeak# fo () finalizer s of { (# s1, w #) -> (# s1, () #) }

mkForeignPtr :: Ptr a -> IO (ForeignPtr a) {- not exported -}
mkForeignPtr (Ptr obj) =  IO ( \ s# ->
    case mkForeignObj# obj s# of
      (# s1#, fo# #) -> (# s1#,  ForeignPtr fo# #) )

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr (ForeignPtr fo) 
   = IO $ \s -> case touch# fo s of s -> (# s, () #)

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fo io
  = do r <- io (foreignPtrToPtr fo)
       touchForeignPtr fo
       return r

foreignPtrToPtr :: ForeignPtr a -> Ptr a
foreignPtrToPtr (ForeignPtr fo) = Ptr (foreignObjToAddr# fo)

castForeignPtr (ForeignPtr a) = ForeignPtr a

\end{code}


