{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Maybe
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Maybe.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
--
-- The Maybe type, and associated operations.
--
-----------------------------------------------------------------------------

module Data.Maybe
   (
     Maybe(Nothing,Just)-- instance of: Eq, Ord, Show, Read,
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
   ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Err	( error )
import GHC.List
import GHC.Maybe
import GHC.Base
#endif

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

