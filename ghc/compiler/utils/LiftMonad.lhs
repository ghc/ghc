%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[LiftMonad]{A lifting monad}

\begin{code}
#if defined(__GLASGOW_HASKELL__)
module LiftMonad where { bogusLiftMonadThing = True }

#else
module LiftMonad (
	LiftM,	-- abstract
	thenLft, returnLft, mapLft
    ) where

infixr 9 `thenLft`

data LiftM a = MkLiftM a
	-- Just add a bottom element under the domain
\end{code}

Notice that @thenLft@ is strict in its first argument.

\begin{code}
thenLft :: LiftM a -> (a -> b) -> b
(MkLiftM x) `thenLft` cont = cont x

returnLft :: a -> LiftM a
returnLft a = MkLiftM a

mapLft :: (a -> LiftM b) -> [a] -> LiftM [b]
mapLft f []  	 = returnLft []
mapLft f (x:xs)
  = f x		  `thenLft` \ x2 ->
    mapLft f xs   `thenLft` \ xs2 ->
    returnLft (x2 : xs2)

#endif
\end{code}
