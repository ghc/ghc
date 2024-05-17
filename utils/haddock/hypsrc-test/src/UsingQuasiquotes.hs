{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE QuasiQuotes #-}
module UsingQuasiquotes where

import Quasiquoter

baz  = [string| foo bar |] ++ [string| some
  mulitline
  quasiquote
|]
