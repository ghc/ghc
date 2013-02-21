%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Foreign]{Foreign calls}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}

module ForeignCall (
        ForeignCall(..), isSafeForeignCall,
        Safety(..), playSafe, playInterruptible,

        CExportSpec(..), CLabelString, isCLabelString, pprCLabelString,
        CCallSpec(..),
        CCallTarget(..), isDynamicTarget,
        CCallConv(..), defaultCCallConv, ccallConvToInt, ccallConvAttribute,

        Header(..), CType(..),
    ) where

import FastString
import Binary
import Outputable
import Module

import Data.Char
import Data.Data
\end{code}


%************************************************************************
%*                                                                      *
\subsubsection{Data types}
%*                                                                      *
%************************************************************************

\begin{code}
newtype ForeignCall = CCall CCallSpec
  deriving Eq
  {-! derive: Binary !-}

isSafeForeignCall :: ForeignCall -> Bool
isSafeForeignCall (CCall (CCallSpec _ _ safe)) = playSafe safe

-- We may need more clues to distinguish foreign calls
-- but this simple printer will do for now
instance Outputable ForeignCall where
  ppr (CCall cc)  = ppr cc
\end{code}


\begin{code}
data Safety
  = PlaySafe            -- Might invoke Haskell GC, or do a call back, or
                        -- switch threads, etc.  So make sure things are
                        -- tidy before the call. Additionally, in the threaded
                        -- RTS we arrange for the external call to be executed
                        -- by a separate OS thread, i.e., _concurrently_ to the
                        -- execution of other Haskell threads.

  | PlayInterruptible   -- Like PlaySafe, but additionally
                        -- the worker thread running this foreign call may
                        -- be unceremoniously killed, so it must be scheduled
                        -- on an unbound thread.

  | PlayRisky           -- None of the above can happen; the call will return
                        -- without interacting with the runtime system at all
  deriving ( Eq, Show, Data, Typeable )
        -- Show used just for Show Lex.Token, I think
  {-! derive: Binary !-}

instance Outputable Safety where
  ppr PlaySafe = ptext (sLit "safe")
  ppr PlayInterruptible = ptext (sLit "interruptible")
  ppr PlayRisky = ptext (sLit "unsafe")

playSafe :: Safety -> Bool
playSafe PlaySafe = True
playSafe PlayInterruptible = True
playSafe PlayRisky = False

playInterruptible :: Safety -> Bool
playInterruptible PlayInterruptible = True
playInterruptible _ = False
\end{code}


%************************************************************************
%*                                                                      *
\subsubsection{Calling C}
%*                                                                      *
%************************************************************************

\begin{code}
data CExportSpec
  = CExportStatic               -- foreign export ccall foo :: ty
        CLabelString            -- C Name of exported function
        CCallConv
  deriving (Data, Typeable)
  {-! derive: Binary !-}

data CCallSpec
  =  CCallSpec  CCallTarget     -- What to call
                CCallConv       -- Calling convention to use.
                Safety
  deriving( Eq )
  {-! derive: Binary !-}
\end{code}

The call target:

\begin{code}

-- | How to call a particular function in C-land.
data CCallTarget
  -- An "unboxed" ccall# to named function in a particular package.
  = StaticTarget
        CLabelString                    -- C-land name of label.

        (Maybe PackageId)               -- What package the function is in.
                                        -- If Nothing, then it's taken to be in the current package.
                                        -- Note: This information is only used for PrimCalls on Windows.
                                        --       See CLabel.labelDynamic and CoreToStg.coreToStgApp
                                        --       for the difference in representation between PrimCalls
                                        --       and ForeignCalls. If the CCallTarget is representing
                                        --       a regular ForeignCall then it's safe to set this to Nothing.

  -- The first argument of the import is the name of a function pointer (an Addr#).
  --    Used when importing a label as "foreign import ccall "dynamic" ..."
        Bool                            -- True => really a function
                                        -- False => a value; only
                                        -- allowed in CAPI imports
  | DynamicTarget

  deriving( Eq, Data, Typeable )
  {-! derive: Binary !-}

isDynamicTarget :: CCallTarget -> Bool
isDynamicTarget DynamicTarget = True
isDynamicTarget _             = False
\end{code}


Stuff to do with calling convention:

ccall:          Caller allocates parameters, *and* deallocates them.

stdcall:        Caller allocates parameters, callee deallocates.
                Function name has @N after it, where N is number of arg bytes
                e.g.  _Foo@8

ToDo: The stdcall calling convention is x86 (win32) specific,
so perhaps we should emit a warning if it's being used on other
platforms.

See: http://www.programmersheaven.com/2/Calling-conventions

\begin{code}
data CCallConv = CCallConv | CApiConv | StdCallConv | PrimCallConv
  deriving (Eq, Data, Typeable)
  {-! derive: Binary !-}

instance Outputable CCallConv where
  ppr StdCallConv = ptext (sLit "stdcall")
  ppr CCallConv   = ptext (sLit "ccall")
  ppr CApiConv    = ptext (sLit "capi")
  ppr PrimCallConv = ptext (sLit "prim")

defaultCCallConv :: CCallConv
defaultCCallConv = CCallConv

ccallConvToInt :: CCallConv -> Int
ccallConvToInt StdCallConv = 0
ccallConvToInt CCallConv   = 1
ccallConvToInt CApiConv    = panic "ccallConvToInt CApiConv"
ccallConvToInt (PrimCallConv {}) = panic "ccallConvToInt PrimCallConv"
\end{code}

Generate the gcc attribute corresponding to the given
calling convention (used by PprAbsC):

\begin{code}
ccallConvAttribute :: CCallConv -> SDoc
ccallConvAttribute StdCallConv       = text "__attribute__((__stdcall__))"
ccallConvAttribute CCallConv         = empty
ccallConvAttribute CApiConv          = empty
ccallConvAttribute (PrimCallConv {}) = panic "ccallConvAttribute PrimCallConv"
\end{code}

\begin{code}
type CLabelString = FastString          -- A C label, completely unencoded

pprCLabelString :: CLabelString -> SDoc
pprCLabelString lbl = ftext lbl

isCLabelString :: CLabelString -> Bool  -- Checks to see if this is a valid C label
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

      ppr_fun (StaticTarget fn mPkgId isFun)
        = text (if isFun then "__pkg_ccall"
                         else "__pkg_ccall_value")
       <> gc_suf
       <+> (case mPkgId of
            Nothing -> empty
            Just pkgId -> ppr pkgId)
       <+> pprCLabelString fn

      ppr_fun DynamicTarget
        = text "__dyn_ccall" <> gc_suf <+> text "\"\""
\end{code}

\begin{code}
-- The filename for a C header file
newtype Header = Header FastString
    deriving (Eq, Data, Typeable)

instance Outputable Header where
    ppr (Header h) = quotes $ ppr h

-- | A C type, used in CAPI FFI calls
data CType = CType (Maybe Header) -- header to include for this type
                   FastString     -- the type itself
    deriving (Data, Typeable)

instance Outputable CType where
    ppr (CType mh ct) = hDoc <+> ftext ct
        where hDoc = case mh of
                     Nothing -> empty
                     Just h -> ppr h
\end{code}


%************************************************************************
%*                                                                      *
\subsubsection{Misc}
%*                                                                      *
%************************************************************************

\begin{code}
{-* Generated by DrIFT-v1.0 : Look, but Don't Touch. *-}
instance Binary ForeignCall where
    put_ bh (CCall aa) = put_ bh aa
    get bh = do aa <- get bh; return (CCall aa)

instance Binary Safety where
    put_ bh PlaySafe = do
            putByte bh 0
    put_ bh PlayInterruptible = do
            putByte bh 1
    put_ bh PlayRisky = do
            putByte bh 2
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return PlaySafe
              1 -> do return PlayInterruptible
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
    put_ bh (StaticTarget aa ab ac) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
            put_ bh ac
    put_ bh DynamicTarget = do
            putByte bh 1
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      ab <- get bh
                      ac <- get bh
                      return (StaticTarget aa ab ac)
              _ -> do return DynamicTarget

instance Binary CCallConv where
    put_ bh CCallConv = do
            putByte bh 0
    put_ bh StdCallConv = do
            putByte bh 1
    put_ bh PrimCallConv = do
            putByte bh 2
    put_ bh CApiConv = do
            putByte bh 3
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return CCallConv
              1 -> do return StdCallConv
              2 -> do return PrimCallConv
              _ -> do return CApiConv

instance Binary CType where
    put_ bh (CType mh fs) = do put_ bh mh
                               put_ bh fs
    get bh = do mh <- get bh
                fs <- get bh
                return (CType mh fs)

instance Binary Header where
    put_ bh (Header h) = put_ bh h
    get bh = do h <- get bh
                return (Header h)
\end{code}
