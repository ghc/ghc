%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Foreign]{Foreign calls}

\begin{code}
module ForeignCall (
	ForeignCall(..),
	Safety(..), playSafe,

	CExportSpec(..),
	CCallSpec(..), 
	CCallTarget(..), isDynamicTarget, isCasmTarget,
	CCallConv(..), defaultCCallConv, ccallConvToInt, ccallConvAttribute,

	DNCallSpec(..),

	okToExposeFCall
    ) where

#include "HsVersions.h"

import CStrings		( CLabelString, pprCLabelString )
import FastString	( FastString )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsubsection{Data types}
%*									*
%************************************************************************

\begin{code}
data ForeignCall
  = CCall	CCallSpec
  | DNCall	DNCallSpec
  deriving( Eq )		-- We compare them when seeing if an interface
				-- has changed (for versioning purposes)

-- We may need more clues to distinguish foreign calls
-- but this simple printer will do for now
instance Outputable ForeignCall where
  ppr (CCall cc)      = ppr cc		
  ppr (DNCall dn) = ppr dn
\end{code}

  
\begin{code}
data Safety
  = PlaySafe		-- Might invoke Haskell GC, or do a call back, or
			-- switch threads, etc.  So make sure things are
			-- tidy before the call

  | PlayRisky		-- None of the above can happen; the call will return
			-- without interacting with the runtime system at all
  deriving( Eq, Show )
	-- Show used just for Show Lex.Token, I think

instance Outputable Safety where
  ppr PlaySafe  = empty
  ppr PlayRisky = ptext SLIT("unsafe")

playSafe PlaySafe  = True
playSafe PlayRisky = False
\end{code}


%************************************************************************
%*									*
\subsubsection{Calling C}
%*									*
%************************************************************************

\begin{code}
data CExportSpec
  = CExportStatic		-- foreign export ccall foo :: ty
	CLabelString		-- C Name of exported function
	CCallConv

data CCallSpec
  =  CCallSpec	CCallTarget	-- What to call
		CCallConv	-- Calling convention to use.
		Safety
  deriving( Eq )
\end{code}

The call target:

\begin{code}
data CCallTarget
  = StaticTarget  CLabelString  -- An "unboxed" ccall# to `fn'.
  | DynamicTarget 		-- First argument (an Addr#) is the function pointer
  | CasmTarget    CLabelString	-- Inline C code (now seriously deprecated)
  deriving( Eq )

isDynamicTarget, isCasmTarget :: CCallTarget -> Bool
isDynamicTarget DynamicTarget = True
isDynamicTarget other	      = False

isCasmTarget (CasmTarget _) = True
isCasmTarget other	    = False
\end{code}


Stuff to do with calling convention

\begin{code}
data CCallConv = CCallConv | StdCallConv
	       deriving( Eq )

instance Outputable CCallConv where
  ppr StdCallConv = ptext SLIT("__stdcall")
  ppr CCallConv   = ptext SLIT("_ccall")

defaultCCallConv :: CCallConv
defaultCCallConv = CCallConv

ccallConvToInt :: CCallConv -> Int
ccallConvToInt StdCallConv = 0
ccallConvToInt CCallConv   = 1
\end{code}

Generate the gcc attribute corresponding to the given
calling convention (used by PprAbsC):

ToDo: The stdcall calling convention is x86 (win32) specific,
so perhaps we should emit a warning if it's being used on other
platforms.

\begin{code}
ccallConvAttribute :: CCallConv -> String
ccallConvAttribute StdCallConv = "__stdcall"
ccallConvAttribute CCallConv   = ""
\end{code}

Printing into C files:

\begin{code}
instance Outputable CExportSpec where
  ppr (CExportStatic str _) = pprCLabelString str

instance Outputable CCallSpec where
  ppr (CCallSpec fun cconv safety)
    = hcat [ ifPprDebug callconv, ppr_fun fun ]
    where
      callconv = text "{-" <> ppr cconv <> text "-}"

      gc_suf | playSafe safety = text "_GC"
	     | otherwise       = empty

      ppr_fun DynamicTarget     = text "__dyn_ccall" <> gc_suf <+> text "\"\""
      ppr_fun (StaticTarget fn) = text "__ccall"     <> gc_suf <+> pprCLabelString fn
      ppr_fun (CasmTarget   fn) = text "__casm"      <> gc_suf <+> text "``" <> pprCLabelString fn <> text "''"
\end{code}


%************************************************************************
%*									*
\subsubsection{.NET stuff}
%*									*
%************************************************************************

\begin{code}
data DNCallSpec = DNCallSpec FastString
		    deriving( Eq )

instance Outputable DNCallSpec where
  ppr (DNCallSpec s) = text "DotNet" <+> ptext s
\end{code}



%************************************************************************
%*									*
\subsubsection{Misc}
%*									*
%************************************************************************

\begin{code}
okToExposeFCall :: ForeignCall -> Bool
-- OK to unfold a Foreign Call in an interface file
-- Yes, unless it's a _casm_
okToExposeFCall (CCall (CCallSpec target _ _)) = not (isCasmTarget target)
okToExposeFCall other  			       = True
\end{code}
