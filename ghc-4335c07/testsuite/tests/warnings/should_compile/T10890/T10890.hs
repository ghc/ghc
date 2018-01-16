module Main where

-- Previously GHC was printing this warning:
--
--   Main.hs:5:1: Warning:
--       The import of ‘A.has’ from module ‘A’ is redundant
--
--   Main.hs:6:1: Warning:
--       The import of ‘B.has’ from module ‘B’ is redundant

import A (A (has))
import B (B (has))

data Blah = Blah

instance A Blah where
  has = Blah

instance B Blah where
  has = Blah

main :: IO ()
main = return ()
