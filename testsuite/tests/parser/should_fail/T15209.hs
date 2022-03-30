{-# LANGUAGE GADTs, TypeOperators #-}
module T15209 where

import GHC.Exts

foo :: a ~# Int -> ()
foo = ()
