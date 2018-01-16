{-# LANGUAGE MagicHash #-}

module ShouldFail where

f = let x = ( 1#, 'c' ) in x
