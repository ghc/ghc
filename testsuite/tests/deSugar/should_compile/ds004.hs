-- !!! ds004 -- nodups from SLPJ p 79
--
module ShouldCompile where

-- SLPJ, p 79
nodups []                   = []
nodups [x]                  = [x]
nodups (y:x:xs) | y == x    = nodups (x:xs)
    	    	| True	    = y : nodups (x:xs)
