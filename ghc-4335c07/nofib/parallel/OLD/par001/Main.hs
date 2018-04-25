{-
This program has two threads, one producing a list,
the other consuming it (and very many global addresses, etc
getting produced along the way).

If the argument supplied is too small, then the second
thread may not have time to "fire".  If the argument is
too large, the program will run for too long.  Some nice
in-between value will push the lots-of-global-addresses
code.

This test program is courtesy of Andrew Partridge.
-}
module Main(main) where

import System.Environment
import Control.Parallel

result x
   = par sxs ((force xs) `seq` sxs)
     where
        xs = take x (repeat (1::Int))
        sxs = sum xs

-- force returns only when the argument list has been
-- completely evaluated.  However, it does not 'hang on' to the list.

force :: [Int] -> ()
force [] = ()
force (x:xs) = x `seq` (force xs)

main = do
    ~[a1] <- getArgs
    let x = fst (head ((reads::ReadS Int) a1))
    putStr (show (result x))
