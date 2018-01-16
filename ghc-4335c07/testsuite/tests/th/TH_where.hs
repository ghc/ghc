{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH


main = do {d <- runQ $ [| let { foo = bar where bar = 3 } in foo |];  print d}
