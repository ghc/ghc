%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[Parallel]{Parallel Constructs}

\begin{code}
module Parallel (par, seq) 

where

infixr 0 `par`
infixr 1 `seq`

par, seq :: a -> b -> b

#if !defined(__CONCURRENT_HASKELL__) && !defined(__PARALLEL_HASKELL__)

par a b = b
seq a b = b

#else

-- Just names without the ugly underscores

{-# INLINE par #-}
par a b = _par_ a b

{-# INLINE seq #-}
seq a b = _seq_ a b

-- Maybe parIO and the like could be added here later.

#endif {- __CONCURRENT_HASKELL__ -}
\end{code}

