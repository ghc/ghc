{-# LANGUAGE NamedDefaults #-}
module Main where

import T25858v1_helper

main = print (pf "25858")
 where
  pf =  stringify . read
