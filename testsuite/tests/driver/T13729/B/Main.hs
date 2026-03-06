{-# LANGUAGE TemplateHaskell #-}
module Main where

import TH

main = putStrLn $(th_string)
