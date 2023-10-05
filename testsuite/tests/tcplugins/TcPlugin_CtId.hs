{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin CtIdPlugin #-}

module TcPlugin_CtId where

import Definitions
  ( CtId )

foo :: CtId (Num a) => a
foo = 5

bar :: Int
bar = foo
