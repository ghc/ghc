{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}
module T2846 where

f :: String
f = show ([1,2,3] :: [Num a => a])
-- Rejected with Quick Look
-- The arg of 'show' is a naked 'a'
-- And the actual arg has type (forall a. [Num a => a]), which is polymorphic
