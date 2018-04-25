module Main where

import Prog (prog)

-- #ifdef PAR
-- main input = prog input
-- #else
-- partain: doesn't actually look at input;
-- real input is wired into Key.lhs

main = do
    str <- getContents
    putStr (prog str)
-- #endif
