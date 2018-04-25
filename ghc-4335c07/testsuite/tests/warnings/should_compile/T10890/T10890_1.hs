module Main where

import Base
import Extends

-- Previously GHC was giving this false positive:
--
--   T10890_1.hs:4:1: Warning:
--       The import of ‘Extends’ is redundant
--         except perhaps to import instances from ‘Extends’
--       To import instances alone, use: import Extends()

data Bar = Bar

instance AClass Bar where
  has = Bar

instance BClass Bar where
  has = Bar

main :: IO ()
main = return ()
