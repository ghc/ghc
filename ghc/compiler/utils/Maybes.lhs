%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Maybes]{The `Maybe' types and associated utility functions}

\begin{code}
#if defined(COMPILING_GHC)
#include "HsVersions.h"
#endif

module Maybes (
	Maybe(..), MaybeErr(..),

	allMaybes,	-- GHCI only
	assocMaybe,
	catMaybes,
	failMaB,
	failMaybe,
	firstJust,
	mapMaybe,	-- GHCI only
	maybeToBool,
	mkLookupFun,
	returnMaB,
	returnMaybe,	-- GHCI only
	thenMaB,
	thenMaybe	-- GHCI only

#if ! defined(COMPILING_GHC)
	, findJust
	, foldlMaybeErrs
	, listMaybeErrs
#endif
    ) where

#if defined(COMPILING_GHC)
import AbsUniType
import Id
import IdInfo
import Name
import Outputable
#if USE_ATTACK_PRAGMAS
import Util
#endif
#endif
\end{code}


%************************************************************************
%*									*
\subsection[Maybe type]{The @Maybe@ type}
%*									*
%************************************************************************

\begin{code}
#if __HASKELL1__ < 3
data Maybe a
  = Nothing
  | Just a
#endif
\end{code}

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
catMaybes :: [Maybe a] -> [a]
catMaybes []		    = []
catMaybes (Nothing : xs)   = catMaybes xs
catMaybes (Just x : xs)	   = (x : catMaybes xs)

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

@assocMaybe@ looks up in an assocation list, returning
@Nothing@ if it fails.

\begin{code}
assocMaybe :: (Eq a) => [(a,b)] -> a -> Maybe b

assocMaybe alist key
  = lookup alist
  where
    lookup []		  = Nothing
    lookup ((tv,ty):rest) = if key == tv then Just ty else lookup rest

#if defined(COMPILING_GHC)
{-# SPECIALIZE assocMaybe
	:: [(String,        b)] -> String        -> Maybe b,
	   [(Id,            b)] -> Id            -> Maybe b,
	   [(Class,         b)] -> Class         -> Maybe b,
	   [(Int,           b)] -> Int           -> Maybe b,
	   [(Name,          b)] -> Name          -> Maybe b,
	   [(TyVar,         b)] -> TyVar         -> Maybe b,
	   [(TyVarTemplate, b)] -> TyVarTemplate -> Maybe b
  #-}
#endif
\end{code}

@mkLookupFun alist s@ is a function which looks up
@s@ in the association list @alist@, returning a Maybe type.

\begin{code}
mkLookupFun :: (key -> key -> Bool)	-- Equality predicate
	    -> [(key,val)] 		-- The assoc list
	    -> key 			-- The key
	    -> Maybe val		-- The corresponding value

mkLookupFun eq alist s
  = case [a | (s',a) <- alist, s' `eq` s] of
      []    -> Nothing
      (a:_) -> Just a
\end{code}

\begin{code}
#if __HASKELL1__ < 3
thenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
m `thenMaybe` k = case m of
		  Nothing -> Nothing
		  Just a  -> k a
#endif
returnMaybe :: a -> Maybe a
returnMaybe = Just 

failMaybe :: Maybe a
failMaybe = Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []	  = returnMaybe []
mapMaybe f (x:xs) = f x				`thenMaybe` (\ x' ->
		    mapMaybe f xs		`thenMaybe` (\ xs' ->
		    returnMaybe (x':xs')		     ))
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


@listMaybeErrs@ takes a list of @MaybeErrs@ and, if they all succeed, returns
a @Succeeded@ of a list of their values.  If any fail, it returns a
@Failed@ of the list of all the errors in the list.

\begin{code}
listMaybeErrs :: [MaybeErr val err] -> MaybeErr [val] [err]
listMaybeErrs
  = foldr combine (Succeeded []) 
  where
    combine (Succeeded v) (Succeeded vs) = Succeeded (v:vs)
    combine (Failed err)  (Succeeded _)	 = Failed [err]
    combine (Succeeded v) (Failed errs)	 = Failed errs
    combine (Failed err)  (Failed errs)	 = Failed (err:errs)
\end{code}

@foldlMaybeErrs@ works along a list, carrying an accumulator; it
applies the given function to the accumulator and the next list item,
accumulating any errors that occur.

\begin{code}
foldlMaybeErrs :: (acc -> input -> MaybeErr acc err)
	       -> acc
	       -> [input]
	       -> MaybeErr acc [err]

foldlMaybeErrs k accum ins = do_it [] accum ins
  where
    do_it []   acc []	  = Succeeded acc
    do_it errs acc []	  = Failed errs
    do_it errs acc (v:vs) = case (k acc v) of
			      Succeeded acc' -> do_it errs	 acc' vs
			      Failed err     -> do_it (err:errs) acc  vs
\end{code}
