{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin NullaryPlugin #-}

module TcPlugin_Nullary where

import Definitions
  ( Nullary )

foo :: Nullary => ()
foo = ()

bar :: ()
bar = foo
