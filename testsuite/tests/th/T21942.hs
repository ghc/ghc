{-# LANGUAGE LinearTypes, TemplateHaskell #-}

module Main where

import Language.Haskell.TH

main :: IO ()
main = runQ [t| forall a m n. a %(m n) -> a |] >>= putStrLn . pprint
