{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

data Expr :: * -> * where
   EInt    :: Int                                 -> Expr Int
   EBool   :: Bool                                -> Expr Bool
   EIf     :: (Expr Bool) -> (Expr a) -> (Expr a) -> Expr a

