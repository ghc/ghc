-----------------------------------------------------------------------------
-- $Id: GetImports.hs,v 1.2 2000/11/17 13:33:17 sewardj Exp $
--
-- GHC Driver program
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

module GetImports ( getImports ) where

import Module
import List
import Char

getImports :: String -> ([ModuleName], [ModuleName], ModuleName)
getImports s
   = f [{-accum source imports-}] [{-accum normal imports-}] 
       (mkModuleName "Main") (words (clean s))
     where
        f si ni _  ("module" : me : ws) = f si ni (mkModuleName me) ws

	f si ni me ("foreign" : "import" : ws) = f si ni me ws
        f si ni me ("import" : "{-#" : "SOURCE" : "#-}" : "qualified" : m : ws) 
           = f ((mkMN m):si) ni me ws
        f si ni me ("import" : "{-#" : "SOURCE" : "#-}" : m : ws) 
           = f ((mkMN m):si) ni me ws
        f si ni me ("import" : "qualified" : m : ws) 
           = f si ((mkMN m):ni) me ws
        f si ni me ("import" : m : ws) 
           = f si ((mkMN m):ni) me ws
        f si ni me (w:ws) = f si ni me ws
        f si ni me [] = (nub si, nub ni, me)

        mkMN str = mkModuleName (takeWhile isModId str)
        isModId c = isAlphaNum c || c `elem` "'_"

-- remove literals and comments from a string
clean :: String -> String
clean s
   = keep s
     where
        -- running through text we want to keep
        keep []                   = []
        keep ('"':cs)             = dquote cs		-- "
		-- try to eliminate single quotes when they're part of
		-- an identifier...
	keep (c:'\'':cs) | isAlphaNum c || c == '_' = keep (dropWhile (=='\'') cs)
        keep ('\'':cs)            = squote cs
        keep ('-':'-':cs)         = linecomment cs
        keep ('{':'-':'#':' ':cs) = "{-# " ++ keep cs
        keep ('{':'-':cs)         = runcomment cs	-- -}
        keep (c:cs)               = c : keep cs

        -- in a double-quoted string
        dquote []             = []
        dquote ('\\':'\"':cs) = dquote cs		-- "
        dquote ('\\':'\\':cs) = dquote cs
        dquote ('\"':cs)      = keep cs			-- "
        dquote (c:cs)         = dquote cs

        -- in a single-quoted string
        squote []             = []
        squote ('\\':'\'':cs) = squote cs
        squote ('\\':'\\':cs) = squote cs
        squote ('\'':cs)      = keep cs
        squote (c:cs)         = squote cs

        -- in a line comment
        linecomment []        = []
        linecomment ('\n':cs) = '\n':keep cs
        linecomment (c:cs)    = linecomment cs

        -- in a running comment
        runcomment []           = []
        runcomment ('-':'}':cs) = keep cs
        runcomment (c:cs)       = runcomment cs
