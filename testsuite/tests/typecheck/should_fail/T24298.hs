{-# LANGUAGE ImplicitParams, MagicHash #-}
module T24298 where

f = let ?foo = 4# in True
