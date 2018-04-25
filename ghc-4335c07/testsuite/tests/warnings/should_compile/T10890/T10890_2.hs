module T10890_2 where

-- Previously GHC was printing this warning:
--
--   Main.hs:5:1: Warning:
--       The import of ‘A.has’ from module ‘A’ is redundant
--
--   Main.hs:6:1: Warning:
--       The import of ‘B.has’ from module ‘B’ is redundant

import T10890_2A (A (has))
import T10890_2B (B (has))

data Blah = Blah

instance A Blah where
  has = Blah
