%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Maybes]{The `Maybe' types and associated utility functions}

\begin{code}
#include "HsVersions.h"

module Maybes (
--	Maybe(..), -- no, it's in 1.3
	MaybeErr(..),

	mapMaybe,
	allMaybes,
	firstJust,
	expectJust,
	maybeToBool,

	assocMaybe,
	mkLookupFun, mkLookupFunDef,

	failMaB,
	failMaybe,
	seqMaybe,
	returnMaB,
	returnMaybe,
	thenMaB,
	catMaybes
    ) where

CHK_Ubiq()		-- debugging consistency check
import Unique  (Unique)	-- only for specialising

#if __GLASGOW_HASKELL__ >= 204
import Maybe( catMaybes, mapMaybe )
#endif

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

#if __GLASGOW_HASKELL__ < 204
	-- After 2.04 we get these from the library Maybe
catMaybes :: [Maybe a] -> [a]
catMaybes []		    = []
catMaybes (Nothing : xs)   = catMaybes xs
catMaybes (Just x : xs)	   = (x : catMaybes xs)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (x:xs) = case f x of
			Just y  -> y : mapMaybe f xs
			Nothing -> mapMaybe f xs
#endif
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

returnMaybe :: a -> Maybe a
returnMaybe = Just

failMaybe :: Maybe a
failMaybe = Nothing
\end{code}

Lookup functions
~~~~~~~~~~~~~~~~

@assocMaybe@ looks up in an assocation list, returning
@Nothing@ if it fails.

\begin{code}
assocMaybe :: (Eq a) => [(a,b)] -> a -> Maybe b

assocMaybe alist key
  = lookup alist
  where
    lookup []		  = Nothing
    lookup ((tv,ty):rest) = if key == tv then Just ty else lookup rest

{-# SPECIALIZE assocMaybe
	:: [(FAST_STRING,   b)] -> FAST_STRING -> Maybe b
	 , [(Int,           b)] -> Int         -> Maybe b
	 , [(Unique,        b)] -> Unique      -> Maybe b
  #-}
\end{code}

@mkLookupFun eq alist@ is a function which looks up
its argument in the association list @alist@, returning a Maybe type.
@mkLookupFunDef@ is similar except that it is given a value to return
on failure.

\begin{code}
mkLookupFun :: (key -> key -> Bool)	-- Equality predicate
	    -> [(key,val)] 		-- The assoc list
	    -> key 			-- The key
	    -> Maybe val		-- The corresponding value

mkLookupFun eq alist s
  = case [a | (s',a) <- alist, s' `eq` s] of
      []    -> Nothing
      (a:_) -> Just a

mkLookupFunDef :: (key -> key -> Bool)	-- Equality predicate
	       -> [(key,val)] 		-- The assoc list
	       -> val 			-- Value to return on failure
	       -> key 			-- The key
	       -> val			-- The corresponding value

mkLookupFunDef eq alist deflt s
  = case [a | (s',a) <- alist, s' `eq` s] of
      []    -> deflt
      (a:_) -> a
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
