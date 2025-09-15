{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin ArgsPlugin
                -fplugin-opt ArgsPlugin:17
 #-}

module Main where

import Definitions
  ( MyClass(methC) )

foo :: Integer
foo = methC

main :: IO ()
main = do
  putStr "foo = "
  print foo
