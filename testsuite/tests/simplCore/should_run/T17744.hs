{-# LANGUAGE OverloadedStrings #-}

module Main where

import T17744A

main :: IO ()
main = print $ completeResults $ feed "f" $ parse uriScheme

uriScheme :: Format (Parser LeftBiasedLocal) Maybe
uriScheme = satisfy_ mytake

ipV4address :: Format (Parser LeftBiasedLocal) Maybe
ipV4address = satisfy_ mytake2
