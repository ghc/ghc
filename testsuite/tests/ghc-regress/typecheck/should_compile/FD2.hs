{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

-- Trac #1783
-- Like Trac #1781 you could argue that this one should succeed
-- but we stick with the old behaviour for now.  When we do 
-- fundeps properly it'll probably start to work

module ShouldCompile where

import Prelude hiding (foldr, foldr1)

import Data.Maybe

class Elem a e | a -> e

class Foldable a where
  foldr :: Elem a e => (e -> b -> b) -> b -> a -> b

--  foldr1 :: forall e. Elem a e => (e -> e -> e) -> a -> e  -- WORKS!
  foldr1 :: Elem a e => (e -> e -> e) -> a -> e
  foldr1 f xs = fromMaybe (error "foldr1: empty structure")
                  (foldr mf Nothing xs)
     where mf :: Elem a e => (e -> Maybe e -> Maybe e)
           mf x Nothing  = Just x
           mf x (Just y) = Just (f x y)
