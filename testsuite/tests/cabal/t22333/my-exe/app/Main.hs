{-# LANGUAGE PackageImports #-}

module Main (main) where

import "my-package-a" LibB (someFuncB)
--import LibB (someFuncB)

main :: IO ()
main = print someFuncB
