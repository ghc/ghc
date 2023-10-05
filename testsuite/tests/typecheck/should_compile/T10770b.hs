{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-redundant-constraints #-}
module T10770b where

f :: (Show a, Show (Maybe a)) => Maybe a -> String
f x = let k = show x in k

g :: (Show a, Show (Maybe a)) => Maybe a -> String
g x = show x
