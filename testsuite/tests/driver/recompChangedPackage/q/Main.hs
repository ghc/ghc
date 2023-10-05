{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib

main = print $(p)
