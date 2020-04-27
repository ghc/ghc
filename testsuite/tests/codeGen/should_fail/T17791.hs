{-# LANGUAGE NoImplicitPrelude #-}

module T17791 where

data Maybe a = Just a | Nothing

data Int = Int
data String = String

foo :: Maybe String -> Int
foo x = case x of
   Nothing -> Int
