%
% (c) The University of Glasgow, 2000
%
\section[CmSummarise]{Module summariser for GHCI}

\begin{code}
module CmSummarise ( ModSummary(..), summarise, name_of_summary,
		     getImports {-, source_has_changed-} )
where

#include "HsVersions.h"

import List 		( nub )
import Char		( isAlphaNum )
--import Time		( ClockTime )
--import Directory	( getModificationTime )

import Util		( unJust )
import HscTypes		( ModuleLocation(..) )
import Module
import Outputable
\end{code}

\begin{code}


-- The ModuleLocation contains both the original source filename and the
-- filename of the cleaned-up source file after all preprocessing has been
-- done.  The point is that the summariser will have to cpp/unlit/whatever
-- all files anyway, and there's no point in doing this twice -- just 
-- park the result in a temp file, put the name of it in the location,
-- and let @compile@ read from that file on the way back up.
data ModSummary
   = ModSummary {
        ms_mod      :: Module,               -- name, package
	ms_location :: ModuleLocation,	     -- location
        ms_srcimps  :: [ModuleName],         -- source imports
        ms_imps     :: [ModuleName]          -- non-source imports
        --ms_date     :: Maybe ClockTime       -- timestamp of summarised
					     -- file, if home && source
     }

instance Outputable ModSummary where
   ppr ms
      = sep [--text "ModSummary { ms_date = " <> text (show ms_date),
             text "ModSummary {",
             nest 3 (sep [text "ms_mod =" <+> ppr (ms_mod ms) <> comma,
                          text "ms_imps =" <+> ppr (ms_imps ms),
                          text "ms_srcimps =" <+> ppr (ms_srcimps ms)]),
             char '}'
            ]

name_of_summary :: ModSummary -> ModuleName
name_of_summary = moduleName . ms_mod


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
        let (srcimps,imps) = getImports modsrc

--        maybe_timestamp
--           <- case ml_hs_file location of 
--                 Nothing     -> return Nothing
--                 Just src_fn -> getModificationTime src_fn >>= Just

        return (ModSummary mod location{ml_hspp_file=Just hspp_fn} 
                               srcimps imps
                                {-maybe_timestamp-} )
   | otherwise
   = return (ModSummary mod location [] [])

-- Compare the timestamp on the source file with that already
-- in the summary, and see if the source file is younger.  If 
-- in any doubt, return True (because False could cause compilation
-- to be omitted).
{-
source_has_changed :: ModSummary -> IO Bool
source_has_changed summary
   = case ms_date summary of {
        Nothing        -> True;   -- don't appear to have a previous timestamp
        Just summ_date -> 
     case ml_hs_file (ms_loc summary) of {
        Nothing        -> True;   -- don't appear to have a source file (?!?!)
        Just src_fn -> do now_date <- getModificationTime src_fn
                          return (now_date > summ_date)
     }}
-}
\end{code}

Collect up the imports from a Haskell source module.  This is
approximate: we don't parse the module, but we do eliminate comments
and strings.  Doesn't currently know how to unlit or cppify the module
first.

\begin{code}
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
\end{code}
