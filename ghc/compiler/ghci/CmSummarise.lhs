%
% (c) The University of Glasgow, 2000
%
\section[CmSummarise]{Module summariser for GHCI}

\begin{code}
module CmSummarise ( ModImport(..), mi_name,
                     ModSummary(..), summarise, ms_get_imports,
		     name_of_summary, deps_of_summary,
		     getImports )
where

#include "HsVersions.h"

import List 		( nub )
import Char		( ord, isAlphaNum )

import CmFind	 	( ModName, ModLocation(..), ml_modname )
import Outputable
\end{code}

\begin{code}


-- The ModLocation contains the original source filename of the module.
-- The ms_ppsource field contains another filename, which is intended to
-- be the cleaned-up source file after all preprocessing has happened to
-- it.  The point is that the summariser will have to cpp/unlit/whatever
-- all files anyway, and there's no point in doing this twice -- just 
-- park the result in a temp file, put the name of it in ms_ppsource,
-- and let @compile@ read from that file on the way back up.
data ModSummary
   = ModSummary {
        ms_loc      :: ModLocation,                     -- location and kind
        ms_ppsource :: (Maybe (FilePath, Fingerprint)), -- preprocessed and sig if .hs
        ms_imports  :: (Maybe [ModImport])              -- imports if .hs or .hi
     }

instance Outputable ModSummary where
   ppr ms
      = sep [text "ModSummary {",
             nest 3 (sep [text "ms_loc =" <+> ppr (ms_loc ms),
             text "ms_ppsource =" <+> fooble (ms_ppsource ms),
             text "ms_imports=" <+> ppr (ms_imports ms)]),
             char '}'
            ]
        where
           fooble Nothing = text "Nothing"
           fooble (Just (cppd_source_name,fp)) 
              = text "(fp =" <+> int fp <> text "," 
                <+> text (show cppd_source_name) <> text ")"

data ModImport
   = MINormal ModName | MISource ModName
     deriving Eq

instance Outputable ModImport where
   ppr (MINormal nm) = text nm
   ppr (MISource nm) = text "{-# SOURCE #-}" <+> text nm


mi_name (MINormal nm) = nm
mi_name (MISource nm) = nm

name_of_summary :: ModSummary -> ModName
name_of_summary = ml_modname . ms_loc

deps_of_summary :: ModSummary -> [ModName]
deps_of_summary = map mi_name . ms_get_imports

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
              in  return (ModSummary loc (Just (path,fp)) (Just imps))
        ObjectCode mod oPath hiPath -- can we get away with the src summariser
                                    -- for interface files?
           -> readFile hiPath >>= \ hisrc ->
              let imps = getImports hisrc
              in  return (ModSummary loc Nothing (Just imps))
        NotFound
           -> pprPanic "summarise:NotFound" (ppr loc)

fingerprint :: String -> Int
fingerprint s
   = dofp s 3# 3#
     where
        -- Copied from hash() in Hugs' storage.c.
        dofp :: String -> Int# -> Int# -> Int
        dofp []     m fp = I# fp
        dofp (c:cs) m fp = dofp cs (m +# 1#) (iabs (fp +# m *# unbox (ord c)))
        unbox (I# i) = i
        iabs :: Int# -> Int#
        iabs n = if n <# 0# then 0# -# n else n
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
