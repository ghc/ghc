-----------------------------------------------------------------------------
-- $Id: GetImports.hs,v 1.1 2000/08/02 15:27:25 simonmar Exp $
--
-- Collect up the imports from a Haskell module.  This is approximate: we don't
-- parse the module, but we do eliminate comments and strings.
--
-- (c) The GHC Team 2000
--

module GetImports (Import(..), getImports) where

import List ( nub )
import Char ( isAlphaNum )

data Import 
   = Normal String | Source String
     deriving (Eq, Show)

getImports :: String -> [Import]
getImports = nub . gmiBase . clean

-- really get the imports from a de-litted, cpp'd, de-literal'd string
gmiBase :: String -> [Import]
gmiBase s
   = f (words s)
     where
	f ("foreign" : "import" : ws) = f ws
        f ("import" : "{-#" : "SOURCE" : "#-}" : "qualified" : m : ws) 
           = Source (takeWhile isModId m) : f ws
        f ("import" : "{-#" : "SOURCE" : "#-}" : m : ws) 
           = Source (takeWhile isModId m) : f ws
        f ("import" : "qualified" : m : ws) 
           = Normal (takeWhile isModId m) : f ws
        f ("import" : m : ws) 
           = Normal (takeWhile isModId m) : f ws
        f (w:ws) = f ws
        f [] = []

isModId c = isAlphaNum c || c `elem` "'_"

-- remove literals and comments from a string
clean :: String -> String
clean s
   = keep s
     where
        -- running through text we want to keep
        keep []                   = []
        keep ('"':cs)             = dquote cs
		-- try to eliminate single quotes when they're part of
		-- an identifier...
	keep (c:'\'':cs) | isAlphaNum c || c == '_' = keep (dropWhile (=='\'') cs)
        keep ('\'':cs)            = squote cs
        keep ('-':'-':cs)         = linecomment cs
        keep ('{':'-':'#':' ':cs) = "{-# " ++ keep cs
        keep ('{':'-':cs)         = runcomment cs
        keep (c:cs)               = c : keep cs

        -- in a double-quoted string
        dquote []             = []
        dquote ('\\':'\"':cs) = dquote cs
        dquote ('\\':'\\':cs) = dquote cs
        dquote ('\"':cs)      = keep cs
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
