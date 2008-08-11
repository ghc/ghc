{-# LANGUAGE TemplateHaskell #-}
-- Trac #2431: empty case expression

module Main where

import Language.Haskell.TH

f :: Int
f = $(caseE (litE $ CharL 'a') [])

main = print f
