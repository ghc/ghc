%
% (c) The University of Glasgow, 2000
%
\section[CmTypes]{Types for the compilation manager}

\begin{code}
module CmTypes ( 
   Unlinked(..),  isObject, nameOfObject, isInterpretable,
   Linkable(..),
   ModSummary(..), ms_allimps, name_of_summary, pprSummaryTime
  ) where

import Interpreter
import HscTypes
import Module
--import CmStaticInfo
import Outputable

import Time		( ClockTime )


data Unlinked
   = DotO FilePath
   | DotA FilePath
   | DotDLL FilePath
   | BCOs [UnlinkedBCO] ItblEnv  -- bunch of interpretable bindings, +
		                 -- a mapping from DataCons to their itbls

instance Outputable Unlinked where
   ppr (DotO path)   = text "DotO" <+> text path
   ppr (DotA path)   = text "DotA" <+> text path
   ppr (DotDLL path) = text "DotDLL" <+> text path
   ppr (BCOs bcos _) = text "BCOs" <+> vcat (map ppr bcos)

isObject (DotO _) = True
isObject (DotA _) = True
isObject (DotDLL _) = True
isObject _ = False

nameOfObject (DotO fn)   = fn
nameOfObject (DotA fn)   = fn
nameOfObject (DotDLL fn) = fn

isInterpretable (BCOs _ _) = True
isInterpretable _          = False

data Linkable = LM {
  linkableTime :: ClockTime,
  linkableModName ::  ModuleName,
  linkableUnlinked ::  [Unlinked]
 }

instance Outputable Linkable where
   ppr (LM when_made mod_nm unlinkeds)
      = text "LinkableM" <+> parens (text (show when_made)) <+> ppr mod_nm 
                         <+> ppr unlinkeds

-- The ModuleLocation contains both the original source filename and the
-- filename of the cleaned-up source file after all preprocessing has been
-- done.  The point is that the summariser will have to cpp/unlit/whatever
-- all files anyway, and there's no point in doing this twice -- just 
-- park the result in a temp file, put the name of it in the location,
-- and let @compile@ read from that file on the way back up.
data ModSummary
   = ModSummary {
        ms_mod      :: Module,               -- name, package
        ms_location :: ModuleLocation,       -- location
        ms_srcimps  :: [ModuleName],         -- source imports
        ms_imps     :: [ModuleName],         -- non-source imports
        ms_hs_date  :: Maybe ClockTime       -- timestamp of summarised
                                             -- file, if home && source
     }

instance Outputable ModSummary where
   ppr ms
      = sep [text "ModSummary {",
             nest 3 (sep [text "ms_hs_date = " <> text (show (ms_hs_date ms)),
                          text "ms_mod =" <+> ppr (ms_mod ms) <> comma,
                          text "ms_imps =" <+> ppr (ms_imps ms),
                          text "ms_srcimps =" <+> ppr (ms_srcimps ms)]),
             char '}'
            ]

pprSummaryTime ms
   = text "ms_hs_date = " <> parens (text (show (ms_hs_date ms)))

ms_allimps ms 
   = ms_srcimps ms ++ ms_imps ms

name_of_summary :: ModSummary -> ModuleName
name_of_summary = moduleName . ms_mod
\end{code}
