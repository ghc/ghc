%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Maybes]{The `Maybe' types and associated utility functions}

\begin{code}
module Maybes (
	MaybeErr(..),

	orElse, 
	mapMaybe,
	allMaybes,
	firstJust,
	expectJust,
	maybeToBool,

	thenMaybe, seqMaybe, returnMaybe, failMaybe, catMaybes,

	thenMaB, returnMaB, failMaB

    ) where

#include "HsVersions.h"

import Maybe( catMaybes, mapMaybe )


infixr 4 `orElse`
\end{code}

%************************************************************************
%*									*
\subsection[Maybe type]{The @Maybe@ type}
%*									*
%************************************************************************

\begin{code}
maybeToBool :: Maybe a -> Bool
maybeToBool Nothing  = False
maybeToBool (Just x) = True
\end{code}

@catMaybes@ takes a list of @Maybe@s and returns a list of
the contents of all the @Just@s in it.	@allMaybes@ collects
a list of @Justs@ into a single @Just@, returning @Nothing@ if there
are any @Nothings@.

\begin{code}
allMaybes :: [Maybe a] -> Maybe [a]
allMaybes [] = Just []
allMaybes (Nothing : ms) = Nothing
allMaybes (Just x  : ms) = case (allMaybes ms) of
			     Nothing -> Nothing
			     Just xs -> Just (x:xs)

\end{code}

@firstJust@ takes a list of @Maybes@ and returns the
first @Just@ if there is one, or @Nothing@ otherwise.

\begin{code}
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x  : ms) = Just x
firstJust (Nothing : ms) = firstJust ms
\end{code}

\begin{code}
findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f []	  = Nothing
findJust f (a:as) = case f a of
		      Nothing -> findJust f as
		      b	 -> b
\end{code}

\begin{code}
expectJust :: String -> Maybe a -> a
{-# INLINE expectJust #-}
expectJust err (Just x) = x
expectJust err Nothing  = error ("expectJust " ++ err)
\end{code}

The Maybe monad
~~~~~~~~~~~~~~~
\begin{code}
seqMaybe :: Maybe a -> Maybe a -> Maybe a
seqMaybe (Just x) _  = Just x
seqMaybe Nothing  my = my

thenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
thenMaybe ma mb = case ma of
		    Just x  -> mb x
		    Nothing -> Nothing

returnMaybe :: a -> Maybe a
returnMaybe = Just

failMaybe :: Maybe a
failMaybe = Nothing

orElse :: Maybe a -> a -> a
(Just x) `orElse` y = x
Nothing  `orElse` y = y
\end{code}


%************************************************************************
%*									*
\subsection[MaybeErr type]{The @MaybeErr@ type}
%*									*
%************************************************************************

\begin{code}
data MaybeErr val err = Succeeded val | Failed err
\end{code}

\begin{code}
thenMaB :: MaybeErr val1 err -> (val1 -> MaybeErr val2 err) -> MaybeErr val2 err
thenMaB m k
  = case m of
      Succeeded v -> k v
      Failed e	  -> Failed e

returnMaB :: val -> MaybeErr val err
returnMaB v = Succeeded v

failMaB :: err -> MaybeErr val err
failMaB e = Failed e
\end{code}

