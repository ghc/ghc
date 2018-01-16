{- LANGUAGE TemplateHaskell #-}

-- Template Haskell type splices
-- Should fail, with a decent error message

module T3177a where

f :: $(id [t| Int Int |])
f = 3

g :: Int Int
g = 3

