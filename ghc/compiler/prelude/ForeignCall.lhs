{-% DrIFT (Automatic class derivations for Haskell) v1.1 %-}
{-% DrIFT (Automatic class derivations for Haskell) v1.1 %-}
%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Foreign]{Foreign calls}

\begin{code}
module ForeignCall (
	ForeignCall(..),
	Safety(..), playSafe, playThreadSafe,

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
import Binary
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
  {-! derive: Binary !-}

-- We may need more clues to distinguish foreign calls
-- but this simple printer will do for now
instance Outputable ForeignCall where
  ppr (CCall cc)  = ppr cc		
  ppr (DNCall dn) = ppr dn
\end{code}

  
\begin{code}
data Safety
  = PlaySafe		-- Might invoke Haskell GC, or do a call back, or
			-- switch threads, etc.  So make sure things are
			-- tidy before the call
	Bool            -- => True, external function is also re-entrant.
			--    [if supported, RTS arranges for the external call
			--    to be executed by a separate OS thread, i.e.,
			--    _concurrently_ to the execution of other Haskell threads.]

  | PlayRisky		-- None of the above can happen; the call will return
			-- without interacting with the runtime system at all
  deriving( Eq, Show )
	-- Show used just for Show Lex.Token, I think
  {-! derive: Binary !-}

instance Outputable Safety where
  ppr (PlaySafe False) = ptext SLIT("safe")
  ppr (PlaySafe True)  = ptext SLIT("threadsafe")
  ppr PlayRisky = ptext SLIT("unsafe")

playSafe :: Safety -> Bool
playSafe PlaySafe{} = True
playSafe PlayRisky  = False

playThreadSafe :: Safety -> Bool
playThreadSafe (PlaySafe x) = x
playThreadSafe _ = False
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
  {-! derive: Binary !-}

data CCallSpec
  =  CCallSpec	CCallTarget	-- What to call
		CCallConv	-- Calling convention to use.
		Safety
  deriving( Eq )
  {-! derive: Binary !-}
\end{code}

The call target:

\begin{code}
data CCallTarget
  = StaticTarget  CLabelString  -- An "unboxed" ccall# to `fn'.
  | DynamicTarget 		-- First argument (an Addr#) is the function pointer
  | CasmTarget    CLabelString	-- Inline C code (now seriously deprecated)
  deriving( Eq )
  {-! derive: Binary !-}

isDynamicTarget, isCasmTarget :: CCallTarget -> Bool
isDynamicTarget DynamicTarget = True
isDynamicTarget other	      = False

isCasmTarget (CasmTarget _) = True
isCasmTarget other	    = False
\end{code}


Stuff to do with calling convention:

ccall:		Caller allocates parameters, *and* deallocates them.

stdcall: 	Caller allocates parameters, callee deallocates.
		Function name has @N after it, where N is number of arg bytes
		e.g.  _Foo@8

ToDo: The stdcall calling convention is x86 (win32) specific,
so perhaps we should emit a warning if it's being used on other
platforms.

\begin{code}
data CCallConv = CCallConv | StdCallConv
  deriving (Eq)
  {-! derive: Binary !-}

instance Outputable CCallConv where
  ppr StdCallConv = ptext SLIT("stdcall")
  ppr CCallConv   = ptext SLIT("ccall")

defaultCCallConv :: CCallConv
defaultCCallConv = CCallConv

ccallConvToInt :: CCallConv -> Int
ccallConvToInt StdCallConv = 0
ccallConvToInt CCallConv   = 1
\end{code}

Generate the gcc attribute corresponding to the given
calling convention (used by PprAbsC):

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
  deriving (Eq)
  {-! derive: Binary !-}

instance Outputable DNCallSpec where
  ppr (DNCallSpec s) = char '"' <> ptext s <> char '"'
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
\begin{code}
{-* Generated by DrIFT-v1.0 : Look, but Don't Touch. *-}
instance Binary ForeignCall where
    put_ bh (CCall aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (DNCall ab) = do
	    putByte bh 1
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (CCall aa)
	      _ -> do ab <- get bh
		      return (DNCall ab)

instance Binary Safety where
    put_ bh (PlaySafe aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh PlayRisky = do
	    putByte bh 1
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (PlaySafe aa)
	      _ -> do return PlayRisky

instance Binary CExportSpec where
    put_ bh (CExportStatic aa ab) = do
	    put_ bh aa
	    put_ bh ab
    get bh = do
	  aa <- get bh
	  ab <- get bh
	  return (CExportStatic aa ab)

instance Binary CCallSpec where
    put_ bh (CCallSpec aa ab ac) = do
	    put_ bh aa
	    put_ bh ab
	    put_ bh ac
    get bh = do
	  aa <- get bh
	  ab <- get bh
	  ac <- get bh
	  return (CCallSpec aa ab ac)

instance Binary CCallTarget where
    put_ bh (StaticTarget aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh DynamicTarget = do
	    putByte bh 1
    put_ bh (CasmTarget ab) = do
	    putByte bh 2
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (StaticTarget aa)
	      1 -> do return DynamicTarget
	      _ -> do ab <- get bh
		      return (CasmTarget ab)

instance Binary CCallConv where
    put_ bh CCallConv = do
	    putByte bh 0
    put_ bh StdCallConv = do
	    putByte bh 1
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return CCallConv
	      _ -> do return StdCallConv

instance Binary DNCallSpec where
    put_ bh (DNCallSpec aa) = do
	    put_ bh aa
    get bh = do
	  aa <- get bh
	  return (DNCallSpec aa)

--  Imported from other files :-

\end{code}
