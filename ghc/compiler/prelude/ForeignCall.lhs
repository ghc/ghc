%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Foreign]{Foreign calls}

\begin{code}
module ForeignCall (
	ForeignCall(..),
	Safety(..), playSafe, playThreadSafe,

	CExportSpec(..), CLabelString, isCLabelString, pprCLabelString,
	CCallSpec(..), 
	CCallTarget(..), isDynamicTarget,
	CCallConv(..), defaultCCallConv, ccallConvToInt, ccallConvAttribute,

	DNCallSpec(..), DNKind(..), DNType(..),
	withDNTypes
    ) where

#include "HsVersions.h"

import FastString	( FastString, unpackFS )
import Char		( isAlphaNum )
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
  deriving( Eq )
  {-! derive: Binary !-}

isDynamicTarget :: CCallTarget -> Bool
isDynamicTarget DynamicTarget = True
isDynamicTarget other	      = False
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
ccallConvAttribute StdCallConv = "__attribute__((__stdcall__))"
ccallConvAttribute CCallConv   = ""
\end{code}

\begin{code}
type CLabelString = FastString		-- A C label, completely unencoded

pprCLabelString :: CLabelString -> SDoc
pprCLabelString lbl = ftext lbl

isCLabelString :: CLabelString -> Bool	-- Checks to see if this is a valid C label
isCLabelString lbl 
  = all ok (unpackFS lbl)
  where
    ok c = isAlphaNum c || c == '_' || c == '.'
	-- The '.' appears in e.g. "foo.so" in the 
	-- module part of a ExtName.  Maybe it should be separate
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
\end{code}


%************************************************************************
%*									*
\subsubsection{.NET interop}
%*									*
%************************************************************************

\begin{code}
data DNCallSpec = 
	DNCallSpec Bool       -- True => static method/field
		   DNKind     -- what type of access
		   String     -- assembly
		   String     -- fully qualified method/field name.
		   [DNType]   -- argument types.
		   DNType     -- result type.
    deriving ( Eq )
  {-! derive: Binary !-}

data DNKind
  = DNMethod
  | DNField
  | DNConstructor
    deriving ( Eq )
  {-! derive: Binary !-}

data DNType
  = DNByte
  | DNBool
  | DNChar
  | DNDouble
  | DNFloat
  | DNInt
  | DNInt8
  | DNInt16
  | DNInt32
  | DNInt64
  | DNWord8
  | DNWord16
  | DNWord32
  | DNWord64
  | DNPtr
  | DNUnit
  | DNObject
  | DNString
    deriving ( Eq )
  {-! derive: Binary !-}

withDNTypes :: DNCallSpec -> [DNType] -> DNType -> DNCallSpec
withDNTypes (DNCallSpec isStatic k assem nm _ _) argTys resTy
  = DNCallSpec isStatic k assem nm argTys resTy

instance Outputable DNCallSpec where
  ppr (DNCallSpec isStatic kind ass nm _ _ ) 
    = char '"' <> 
       (if isStatic then text "static" else empty) <+>
       (text (case kind of { DNMethod -> "method" ; DNField -> "field"; DNConstructor -> "ctor" })) <+>
       (if null ass then char ' ' else char '[' <> text ass <> char ']') <>
       text nm <> 
      char '"'
\end{code}



%************************************************************************
%*									*
\subsubsection{Misc}
%*									*
%************************************************************************

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
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (StaticTarget aa)
	      _ -> do return DynamicTarget

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
    put_ bh (DNCallSpec isStatic kind ass nm _ _) = do
            put_ bh isStatic
	    put_ bh kind
	    put_ bh ass
	    put_ bh nm
    get bh = do
          isStatic <- get bh
	  kind     <- get bh
	  ass      <- get bh
	  nm       <- get bh
	  return (DNCallSpec isStatic kind ass nm [] undefined)

instance Binary DNKind where
    put_ bh DNMethod = do
	    putByte bh 0
    put_ bh DNField = do
	    putByte bh 1
    put_ bh DNConstructor = do
	    putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return DNMethod
	      1 -> do return DNField
	      _ -> do return DNConstructor

instance Binary DNType where
    put_ bh DNByte = do
	    putByte bh 0
    put_ bh DNBool = do
	    putByte bh 1
    put_ bh DNChar = do
	    putByte bh 2
    put_ bh DNDouble = do
	    putByte bh 3
    put_ bh DNFloat = do
	    putByte bh 4
    put_ bh DNInt = do
	    putByte bh 5
    put_ bh DNInt8 = do
	    putByte bh 6
    put_ bh DNInt16 = do
	    putByte bh 7
    put_ bh DNInt32 = do
	    putByte bh 8
    put_ bh DNInt64 = do
	    putByte bh 9
    put_ bh DNWord8 = do
	    putByte bh 10
    put_ bh DNWord16 = do
	    putByte bh 11
    put_ bh DNWord32 = do
	    putByte bh 12
    put_ bh DNWord64 = do
	    putByte bh 13
    put_ bh DNPtr = do
	    putByte bh 14
    put_ bh DNUnit = do
	    putByte bh 15
    put_ bh DNObject = do
	    putByte bh 16
    put_ bh DNString = do
	    putByte bh 17

    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> return DNByte
	      1 -> return DNBool
	      2 -> return DNChar
  	      3 -> return DNDouble
  	      4 -> return DNFloat
  	      5 -> return DNInt
  	      6 -> return DNInt8
  	      7 -> return DNInt16
  	      8 -> return DNInt32
	      9 -> return DNInt64
	      10 -> return DNWord8
	      11 -> return DNWord16
	      12 -> return DNWord32
	      13 -> return DNWord64
	      14 -> return DNPtr
	      15 -> return DNUnit
	      16 -> return DNObject
	      17 -> return DNString

--  Imported from other files :-

\end{code}
