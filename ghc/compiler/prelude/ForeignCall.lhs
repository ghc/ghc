%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Foreign]{Foreign calls}

\begin{code}
module ForeignCall (
	ForeignCall(..),
	Safety(..), playSafe,

	CCallSpec(..), ccallIsCasm,
	CCallTarget(..), dynamicTarget, isDynamicTarget,
	CCallConv(..), defaultCCallConv, ccallConvToInt, ccallConvAttribute,

	DotNetCallSpec(..)
    ) where

#include "HsVersions.h"

import CStrings		( CLabelString, pprCLabelString )
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
  | DotNetCall	DotNetCallSpec
  deriving( Eq )		-- We compare them when seeing if an interface
				-- has changed (for versioning purposes)

-- We may need more clues to distinguish foreign calls
-- but this simple printer will do for now
instance Outputable ForeignCall where
  ppr (CCall cc)      = ppr cc		
  ppr (DotNetCall dn) = ppr dn
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
data CCallSpec
  =  CCallSpec	CCallTarget	-- What to call
		CCallConv	-- Calling convention to use.
		Safety
		Bool		-- True <=> really a "casm"
  deriving( Eq )


ccallIsCasm :: CCallSpec -> Bool
ccallIsCasm (CCallSpec _ _ _ c_asm) = c_asm
\end{code}

The call target:

\begin{code}
data CCallTarget
  = StaticTarget  CLabelString  -- An "unboxed" ccall# to `fn'.
  | DynamicTarget 		-- First argument (an Addr#) is the function pointer
  deriving( Eq )

isDynamicTarget DynamicTarget    = True
isDynamicTarget (StaticTarget _) = False

dynamicTarget :: CCallTarget
dynamicTarget = DynamicTarget
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
instance Outputable CCallSpec where
  ppr (CCallSpec fun cconv safety is_casm)
    = hcat [ ifPprDebug callconv
	   , text "__", ppr_dyn
           , text before , ppr_fun , after]
    where
        callconv = text "{-" <> ppr cconv <> text "-}"
	play_safe = playSafe safety

	before
	  | is_casm && play_safe = "casm_GC ``"
	  | is_casm	         = "casm ``"
	  | play_safe	         = "ccall_GC "
	  | otherwise	         = "ccall "

	after
	  | is_casm   = text "''"
	  | otherwise = empty
	  
	ppr_dyn = case fun of
		    DynamicTarget -> text "dyn_"
		    _	   	  -> empty

	ppr_fun = case fun of
		     DynamicTarget   -> text "\"\""
		     StaticTarget fn -> pprCLabelString fn
\end{code}


%************************************************************************
%*									*
\subsubsection{.NET stuff}
%*									*
%************************************************************************

\begin{code}
data DotNetCallSpec = DotNetCallSpec
		    deriving( Eq )

instance Outputable DotNetCallSpec where
  ppr DotNetCallSpec = text "DotNet!"
\end{code}
