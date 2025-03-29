{-# LANGUAGE NamedDefaults #-}

module Main where

import T25858v2_helper

main :: IO ()
main = do
  print c
  print b

-- another import version of ticket #25858.
