{-# LANGUAGE TemplateHaskell #-}
-- Trac #2431: empty case expression
--             currently rejected

module Main where

import Language.Haskell.TH

f :: Int
f = $(caseE (litE $ CharL 'a') [])

main = print f
