%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[Maybe]{Module @Maybe@}

The standard Haskell 1.3 library for working with
@Maybe@ values.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Maybe
   (
    Maybe(..),
    isJust, fromJust, 
    fromMaybe, 
    listToMaybe, maybeToList,
    catMaybes, 
    mapMaybe, 
    unfoldr
   ) where

import PrelErr	( error )
import Monad	( filter )
import PrelList
import PrelMaybe
import PrelBase
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
 
findMaybe              :: (a -> Bool) -> [a] -> Maybe a
findMaybe p            =  listToMaybe . filter p

catMaybes              :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r:rs

--OLD: mapMaybe f             =  catMaybes . map f
-- new version is potentially more space efficient

-- Not exported
joinMaybe         :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a 
joinMaybe f m1 m2 =
 case m1 of
  Nothing -> m2
  Just v1 -> case m2 of {Nothing -> m1; Just v2 -> Just (f v1 v2)}

{- OLD: Note: stricter than the above.
joinMaybe _ Nothing  Nothing  = Nothing
joinMaybe _ (Just g) Nothing  = Just g
joinMaybe _ Nothing  (Just g) = Just g
joinMaybe f (Just g) (Just h) = Just (f g h)
-}

\end{code}

\begin{verbatim}
  unfoldr f' (foldr f z xs) == (xs,z)

 if the following holds:

   f' (f x y) = Just (x,y)
   f' z       = Nothing
\end{verbatim}

\begin{code}
unfoldr       :: (a -> Maybe (b, a)) -> a -> ([b],a)
unfoldr f x   =
  case f x of
  Just (y,x') -> let (ys,x'') = unfoldr f x' in (y:ys,x'')
  Nothing     -> ([],x)
\end{code}
