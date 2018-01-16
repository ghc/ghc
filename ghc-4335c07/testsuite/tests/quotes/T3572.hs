{-# LANGUAGE EmptyDataDecls #-}

-- Trac #3572

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

main = putStrLn . pprint =<< runQ [d| data Void |]
