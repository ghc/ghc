{-# LANGUAGE TemplateHaskellQuotes, TypeApplications #-}
module Main (main) where

import Language.Haskell.TH
import Data.Proxy

hasty :: Q Type -> Int
hasty ty = const @Int @($ty) 42

main :: IO ()
main = print $ hasty [| Char |]
