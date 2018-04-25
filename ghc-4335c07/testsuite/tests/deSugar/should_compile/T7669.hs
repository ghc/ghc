{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}  

module T7669 where

data Void

foo :: Void -> ()
foo x = case x of {}
-- Should not get incomplete-pattern warning

