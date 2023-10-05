{-# LANGUAGE Haskell2010 #-}
module T19109 where

import Data.Functor.Identity

f (Identity @Int x) = x
