%
% (c) The University of Glasgow, 2000
%
\section[CmTypes]{Types for the compilation manager}

\begin{code}
module CmTypes ( 
   Unlinked(..),  isObject, nameOfObject, isInterpretable,
   Linkable(..),
   ModSummary(..), name_of_summary, pprSummaryTimes
  ) where

import Interpreter
import HscTypes
import Module
import CmStaticInfo
import Outputable

import Time		( ClockTime )


data Unlinked
   = DotO FilePath
   | DotA FilePath
   | DotDLL FilePath
   | Trees [UnlinkedIBind] ItblEnv  -- bunch of interpretable bindings, +
				    -- a mapping from DataCons to their itbls

instance Outputable Unlinked where
   ppr (DotO path)   = text "DotO" <+> text path
   ppr (DotA path)   = text "DotA" <+> text path
   ppr (DotDLL path) = text "DotDLL" <+> text path
   ppr (Trees binds _) = text "Trees" <+> ppr binds

isObject (DotO _) = True
isObject (DotA _) = True
isObject (DotDLL _) = True
isObject _ = False

nameOfObject (DotO fn)   = fn
nameOfObject (DotA fn)   = fn
nameOfObject (DotDLL fn) = fn

isInterpretable (Trees _ _) = True
isInterpretable _ = False

data Linkable
   = LM ModuleName [Unlinked]
   | LP PackageName

instance Outputable Linkable where
   ppr (LM mod_nm unlinkeds) = text "LinkableM" <+> ppr mod_nm <+> ppr unlinkeds
   ppr (LP package_nm)       = text "LinkableP" <+> ptext package_nm

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
        ms_hs_date  :: Maybe ClockTime,      -- timestamp of summarised
                                             -- file, if home && source
        ms_hi_date  :: Maybe ClockTime       -- timestamp of old iface,
                                             -- if home && source
     }

instance Outputable ModSummary where
   ppr ms
      = sep [text "ModSummary {",
             nest 3 (sep [text "ms_hs_date = " <> text (show (ms_hs_date ms)),
                          text "ms_hi_date = " <> text (show (ms_hi_date ms)),
                          text "ms_mod =" <+> ppr (ms_mod ms) <> comma,
                          text "ms_imps =" <+> ppr (ms_imps ms),
                          text "ms_srcimps =" <+> ppr (ms_srcimps ms)]),
             char '}'
            ]

pprSummaryTimes ms
   = sep [text "ms_hs_date = " <> text (show (ms_hs_date ms)),
          text "ms_hi_date = " <> text (show (ms_hi_date ms))]

name_of_summary :: ModSummary -> ModuleName
name_of_summary = moduleName . ms_mod
\end{code}
