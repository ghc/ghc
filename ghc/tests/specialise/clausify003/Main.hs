module Main ( main ) where

import PreludeClausify (clausify)

-- the main program: reads stdin and writes stdout
main = scc "CAF:main" 
    do
	input <- getContents
	putStr (clausify input)
