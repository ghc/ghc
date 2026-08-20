module Main where

import T27705_Inst

-- The dictionaries (D1/D2) are mutually recursive. We have to watch
-- out for the specializer looping on them. This was first detected in #22802
-- but no test was added, which caused it to break again #27705 :(
main :: IO ()
main = print (b (3 :: Int))
