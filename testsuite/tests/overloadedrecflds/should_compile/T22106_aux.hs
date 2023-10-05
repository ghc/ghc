{-# LANGUAGE NoFieldSelectors #-}

module T22106_aux where

data T = MkT { foo :: Int, bar :: Int }
foo = ()
