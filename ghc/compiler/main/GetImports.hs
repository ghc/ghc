-----------------------------------------------------------------------------
-- $Id: GetImports.hs,v 1.1 2000/11/16 15:57:05 simonmar Exp $
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

getImports :: String -> ([ModuleName], [ModuleName])
getImports str
   = let all_imps = (nub . gmiBase . clean) str
         srcs     = concatMap (either unit nil) all_imps
         normals  = concatMap (either nil unit) all_imps
         unit x   = [x]
         nil x    = []
     in  (srcs, normals)

-- really get the imports from a de-litted, cpp'd, de-literal'd string
-- Lefts are source imports.  Rights are normal ones.
gmiBase :: String -> [Either ModuleName ModuleName]
gmiBase s
   = f (words s)
     where
	f ("foreign" : "import" : ws) = f ws
        f ("import" : "{-#" : "SOURCE" : "#-}" : "qualified" : m : ws) 
           = Left (mkMN m) : f ws
        f ("import" : "{-#" : "SOURCE" : "#-}" : m : ws) 
           = Left (mkMN m) : f ws
        f ("import" : "qualified" : m : ws) 
           = Right (mkMN m) : f ws
        f ("import" : m : ws) 
           = Right (mkMN m) : f ws
        f (w:ws) = f ws
        f [] = []

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
