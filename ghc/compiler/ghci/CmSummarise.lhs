%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[CmSummarise]{Module summariser for GHCI}

\begin{code}
module CmSummarise ( ModImport(..), mi_name,
                     ModSummary(..), summarise, ms_get_imports )
where

#include "HsVersions.h"

import List 		( nub )
import Char		( ord, isAlphaNum )

import CmFind	 	( ModName, ModLocation(..) )
import Outputable	( pprPanic, text )
\end{code}

\begin{code}


data ModSummary
   = ModSummary {
        ms_loc     :: ModLocation,                   -- location and kind
        ms_source  :: (Maybe (String, Fingerprint)), -- source and sig if .hs
        ms_imports :: (Maybe [ModImport])            -- imports if .hs or .hi
     }
     deriving Show

data ModImport
   = MINormal ModName | MISource ModName
     deriving (Eq, Show)

mi_name (MINormal nm) = nm
mi_name (MISource nm) = nm

ms_get_imports :: ModSummary -> [ModImport]
ms_get_imports summ
   = case ms_imports summ of { Just is -> is; Nothing -> [] }

type Fingerprint = Int

summarise :: ModLocation -> IO ModSummary

summarise loc
   = case loc of
        InPackage mod path -- if in a package, investigate no further
           -> return (ModSummary loc Nothing Nothing)
        SourceOnly mod path -- source; read, cache and get imports
           -> readFile path >>= \ modsrc ->
              let imps = getImports modsrc
                  fp   = fingerprint modsrc
              in  return (ModSummary loc (Just (modsrc,fp)) (Just imps))
        ObjectCode mod oPath hiPath -- can we get away with the src summariser
                                    -- for interface files?
           -> readFile hiPath >>= \ hisrc ->
              let imps = getImports hisrc
              in  return (ModSummary loc Nothing (Just imps))
        NotFound
           -> pprPanic "summarise:NotFound" (text (show loc))

fingerprint :: String -> Int
fingerprint s
   = dofp s 3 3
     where
        -- Copied from hash() in Hugs' storage.c.
        dofp :: String -> Int -> Int -> Int
        dofp []     m fp = fp
        dofp (c:cs) m fp = dofp cs (m+1) (iabs (fp + m * ord c))
        iabs :: Int -> Int
        iabs n = if n < 0 then -n else n
\end{code}

Collect up the imports from a Haskell source module.  This is
approximate: we don't parse the module, but we do eliminate comments
and strings.  Doesn't currently know how to unlit or cppify the module
first.

\begin{code}

getImports :: String -> [ModImport]
getImports = nub . gmiBase . clean

-- really get the imports from a de-litted, cpp'd, de-literal'd string
gmiBase :: String -> [ModImport]
gmiBase s
   = f (words s)
     where
	f ("foreign" : "import" : ws) = f ws
        f ("import" : "{-#" : "SOURCE" : "#-}" : "qualified" : m : ws) 
           = MISource (takeWhile isModId m) : f ws
        f ("import" : "{-#" : "SOURCE" : "#-}" : m : ws) 
           = MISource (takeWhile isModId m) : f ws
        f ("import" : "qualified" : m : ws) 
           = MINormal (takeWhile isModId m) : f ws
        f ("import" : m : ws) 
           = MINormal (takeWhile isModId m) : f ws
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
\end{code}