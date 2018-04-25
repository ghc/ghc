import System.Environment
main = do 
 [_,inf,outf] <- getArgs
 s <- readFile inf
 writeFile outf ("{-# LANGUAGE GADTs #-}\n" ++ s)
