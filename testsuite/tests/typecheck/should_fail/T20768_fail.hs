{-# language LambdaCase #-}

module Main where

import Data.Function

bar = \cases | 'c' -> "foo"

main = (\cases 1 2 -> return ()) "foo"

foo = 1 & \cases 1 2 -> return ()
                 1 -> return ()
