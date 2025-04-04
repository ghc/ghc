{-# LANGUAGE NamedDefaults #-}

module Main where

import T25912_helper

-- now we declare the default instances
-- for the classes C again to check that
-- it won't hide the default instances for class B
default C (String)

main :: IO ()
main = do
  print b
