%
% (c) The University of Glasgow, 2000
%
\section[CmSummarise]{Module summariser for GHCI}

\begin{code}
module CmSummarise ( ModImport(..), mimp_name,
                     ModSummary(..), summarise, ms_get_imports,
		     name_of_summary, deps_of_summary, is_source_import,
		     getImports )
where

#include "HsVersions.h"

import List 		( nub )
import Char		( isAlphaNum )
import Util		( unJust )
import HscTypes		( ModuleLocation(..) )

import Module
import Outputable
\end{code}

\begin{code}


-- The Module contains the original source filename of the module.
-- The ms_ppsource field contains another filename, which is intended to
-- be the cleaned-up source file after all preprocessing has happened to
-- it.  The point is that the summariser will have to cpp/unlit/whatever
-- all files anyway, and there's no point in doing this twice -- just 
-- park the result in a temp file, put the name of it in ms_ppsource,
-- and let @compile@ read from that file on the way back up.
data ModSummary
   = ModSummary {
        ms_mod      :: Module,                          -- name, package
	ms_location :: ModuleLocation,			-- location
        ms_imports  :: (Maybe [ModImport])              -- imports if .hs or .hi
     }

instance Outputable ModSummary where
   ppr ms
      = sep [text "ModSummary {",
             nest 3 (sep [text "ms_mod =" <+> ppr (ms_mod ms) <> comma,
             text "ms_imports =" <+> ppr (ms_imports ms)]),
             char '}'
            ]

data ModImport
   = MINormal ModuleName | MISource ModuleName
     deriving Eq

instance Outputable ModImport where
   ppr (MINormal nm) = ppr nm
   ppr (MISource nm) = text "{-# SOURCE #-}" <+> ppr nm


mimp_name (MINormal nm) = nm
mimp_name (MISource nm) = nm

is_source_import (MINormal _) = False
is_source_import (MISource _) = True

name_of_summary :: ModSummary -> ModuleName
name_of_summary = moduleName . ms_mod

deps_of_summary :: ModSummary -> [ModuleName]
deps_of_summary = map mimp_name . ms_get_imports

ms_get_imports :: ModSummary -> [ModImport]
ms_get_imports summ
   = case ms_imports summ of { Just is -> is; Nothing -> [] }

type Fingerprint = Int

-- The first arg is supposed to be DriverPipeline.preprocess.
-- Passed in here to avoid a hard-to-avoid circular dependency
-- between CmSummarise and DriverPipeline.  Same deal as with
-- CmLink.link.
summarise :: (FilePath -> IO FilePath)
          -> Module -> ModuleLocation -> IO ModSummary
summarise preprocess mod location
   | isModuleInThisPackage mod
   = do let hs_fn = unJust (ml_hs_file location) "summarise"
        hspp_fn <- preprocess hs_fn
        modsrc <- readFile hspp_fn
        let imps = getImports modsrc
        return (ModSummary mod location{ml_hspp_file=Just hspp_fn} (Just imps))
   | otherwise
   = return (ModSummary mod location Nothing)
\end{code}

Collect up the imports from a Haskell source module.  This is
approximate: we don't parse the module, but we do eliminate comments
and strings.  Doesn't currently know how to unlit or cppify the module
first.

NB !!!!! Ignores source imports, pro tem.

\begin{code}

getImports :: String -> [ModImport]
getImports = filter (not . is_source_import) .
             nub . gmiBase . clean

-- really get the imports from a de-litted, cpp'd, de-literal'd string
gmiBase :: String -> [ModImport]
gmiBase s
   = f (words s)
     where
	f ("foreign" : "import" : ws) = f ws
        f ("import" : "{-#" : "SOURCE" : "#-}" : "qualified" : m : ws) 
           = MISource (mkMN m) : f ws
        f ("import" : "{-#" : "SOURCE" : "#-}" : m : ws) 
           = MISource (mkMN m) : f ws
        f ("import" : "qualified" : m : ws) 
           = MINormal (mkMN m) : f ws
        f ("import" : m : ws) 
           = MINormal (mkMN m) : f ws
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
\end{code}
