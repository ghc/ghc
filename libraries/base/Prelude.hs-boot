{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module Prelude where

import GHC.IOBase

catch :: IO a -> (IOError -> IO a) -> IO a
