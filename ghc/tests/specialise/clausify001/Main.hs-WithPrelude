module Main ( main ) where

import PreludeClausify (clausify)

-- the main program: reads stdin and writes stdout
main = scc "CAF:main" 
       readChan stdin exit ( \input ->
       appendChan stdout (clausify input) exit done)
