{-# LANGUAGE GADTs, TypeOperators #-}
module T15209 where

import GHC.Prim

foo :: a ~# Int -> ()
foo = ()
