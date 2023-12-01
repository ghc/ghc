{-# OPTIONS_GHC -W #-}

module T24234 where

foo :: [()] -> ()
foo ~(a:_) = a
foo _      = ()
