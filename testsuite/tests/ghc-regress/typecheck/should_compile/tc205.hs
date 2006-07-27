{-# OPTIONS_GHC -fglasgow-exts #-}

-- Tests infix type constructors in GADT declarations

module ShouldCompile where

infix 1 `DArrowX`	-- (->) has precedence 0

data DArrowX :: * -> * -> * where
  First   :: a `DArrowX`  a' -> (a,b) `DArrowX` (a',b)
