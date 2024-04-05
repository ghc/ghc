{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hello

$(mkHello)

main :: IO ()
main = hello >>= print
