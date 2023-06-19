{-# LANGUAGE TemplateHaskellQuotes, TypeApplications #-}
module Main (main) where

import Language.Haskell.TH
import Data.Proxy

hasty :: IO Type
hasty = [t| forall (ty :: TypeQ). Proxy $ty |]

main :: IO ()
main = hasty >>= print
