%
% (c) The AQUA Project, Glasgow University, 1994-1999
%
\section[Maybe]{Module @Maybe@}

The standard Haskell 1.3 library for working with
@Maybe@ values.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Maybe
   (
     Maybe(Nothing,Just)
			-- instance of: Eq, Ord, Show, Read,
			--		Functor, Monad, MonadPlus

   , maybe		-- :: b -> (a -> b) -> Maybe a -> b

   , isJust		-- :: Maybe a -> Bool
   , isNothing		-- :: Maybe a -> Bool
   , fromJust		-- :: Maybe a -> a
   , fromMaybe		-- :: a -> Maybe a -> a
   , listToMaybe        -- :: [a] -> Maybe a
   , maybeToList	-- :: Maybe a -> [a]
   , catMaybes		-- :: [Maybe a] -> [a]
   , mapMaybe		-- :: (a -> Maybe b) -> [a] -> [b]

     -- Implementation checked wrt. Haskell 98 lib report, 1/99.
   ) where

#ifndef __HUGS__
import PrelErr	( error )
import PrelList
import PrelMaybe
import PrelBase
#endif
\end{code}


%*********************************************************
%*							*
\subsection{Functions}
%*							*
%*********************************************************

\begin{code}
isJust         :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing         :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

fromJust          :: Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing" -- yuck
fromJust (Just x) = x

fromMaybe     :: a -> Maybe a -> a
fromMaybe d x = case x of {Nothing -> d;Just v  -> v}

maybeToList            :: Maybe a -> [a]
maybeToList  Nothing   = []
maybeToList  (Just x)  = [x]

listToMaybe           :: [a] -> Maybe a
listToMaybe []        =  Nothing
listToMaybe (a:_)     =  Just a
 
catMaybes              :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r:rs

\end{code}

