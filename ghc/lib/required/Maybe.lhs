%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Maybe]{Module @Maybe@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Maybe(
    Maybe(..),
    the, exists, theExists, maybe, fromMaybe, listToMaybe, maybeToList,
    findMaybe, catMaybes, mapMaybe, joinMaybe, unfoldr
  ) where

import IOBase	( error )
import Monad	( filter )
import PrelList
import PrelBase
\end{code}


%*********************************************************
%*							*
\subsection{Functions}
%*							*
%*********************************************************

\begin{code}
maybe                   :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing       =  n
maybe n f (Just x)      =  f x

exists                 :: Maybe a -> Bool
exists                 =  maybe False (const True)

the                    :: Maybe a -> a
the                    =  maybe (error "Maybe.the: Nothing") id

theExists              :: Maybe a -> (a, Bool)
theExists Nothing      =  (error "Maybe.theExists: Nothing", False)
theExists (Just x)     =  (x, True)

fromMaybe              :: a -> Maybe a -> a
fromMaybe d            =  maybe d id

maybeToList            :: Maybe a -> [a]
maybeToList            =  maybe [] (\ x -> [x])

listToMaybe            :: [a] -> Maybe a
listToMaybe []         =  Nothing
listToMaybe (a:as)     =  Just a
 
findMaybe              :: (a -> Bool) -> [a] -> Maybe a
findMaybe p            =  listToMaybe . filter p

catMaybes              :: [Maybe a] -> [a]
catMaybes []           =  []
catMaybes (Nothing:xs) =  catMaybes xs
catMaybes (Just x:xs)  =  x : catMaybes xs

mapMaybe               :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f             =  catMaybes . map f

joinMaybe              :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a 
joinMaybe _ Nothing  Nothing  = Nothing
joinMaybe _ (Just g) Nothing  = Just g
joinMaybe _ Nothing  (Just g) = Just g
joinMaybe f (Just g) (Just h) = Just (f g h)

--    unfoldr f' (foldr f z xs) == (xs,z)
--
-- if the following holds:
--
--    f' (f x y) = Just (x,y)
--    f' z       = Nothing
unfoldr                :: (a -> Maybe (b, a)) -> a -> ([b],a)
unfoldr f x =
  case f x of
  Just (y,x') -> let (ys,x'') = unfoldr f x' in (y:ys,x'')
  Nothing     -> ([],x)
\end{code}
