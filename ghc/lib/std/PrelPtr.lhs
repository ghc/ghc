-----------------------------------------------------------------------------
-- $Id: PrelPtr.lhs,v 1.1 2001/01/11 17:25:57 simonmar Exp $
-- 
-- (c) 2000
-- 
-- Module PrelPtr

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
module PrelPtr{-everything-} where
	
import PrelBase

------------------------------------------------------------------------
-- Data pointers.

data Ptr a = Ptr Addr# deriving (Eq, Ord)

nullPtr :: Ptr a
nullPtr = Ptr (int2Addr# 0#)

castPtr :: Ptr a -> Ptr b
castPtr (Ptr addr) = Ptr addr

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr addr) (I# d) = Ptr (int2Addr# (addr2Int# addr +# d))

alignPtr :: Ptr a -> Int -> Ptr a
alignPtr addr@(Ptr a) (I# i)
  = case addr2Int# a	of { ai ->
    case remInt# ai i	of {
      0# -> addr;
      n  -> Ptr (int2Addr# (ai +# (i -# n))) }}

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr (Ptr a1) (Ptr a2) = I# (addr2Int# a1 -# addr2Int# a2)

instance CCallable   (Ptr a)
instance CReturnable (Ptr a)

------------------------------------------------------------------------
-- Function pointers for the default calling convention.

newtype FunPtr a = FunPtr (Ptr a) deriving (Eq, Ord)

nullFunPtr :: FunPtr a
nullFunPtr = FunPtr nullPtr

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr a) = FunPtr (castPtr a)

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr a) = castPtr a

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr a = FunPtr (castPtr a)

instance CCallable   (FunPtr a)
instance CReturnable (FunPtr a)
\end{code}
