{-# LANGUAGE TemplateHaskell #-}
module Main where

import TH

main = print $(splice)
