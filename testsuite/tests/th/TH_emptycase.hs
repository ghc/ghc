{-# LANGUAGE TemplateHaskell, EmptyCase #-}
-- #2431: empty case expression
--             now accepted

module Main where

import Language.Haskell.TH

f :: Int
f = $(caseE (litE $ CharL 'a') [])

main = print f
