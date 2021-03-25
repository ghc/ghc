{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[Foreign]{Foreign calls}
-}

{-# LANGUAGE DeriveDataTypeable #-}

module GHC.Types.ForeignCall (
        ForeignCall(..), isSafeForeignCall,
        Safety(..), playSafe, playInterruptible,

        CExportSpec(..), CLabelString, isCLabelString, pprCLabelString,
        CCallSpec(..),
        CCallTarget(..), isDynamicTarget,
        CCallConv(..), defaultCCallConv, ccallConvToInt, ccallConvAttribute,

        Header(..), CType(..),
    ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Unit.Module
import GHC.Types.SourceText ( SourceText, pprWithSourceText )

import Data.Char
import Data.Data

{-
************************************************************************
*                                                                      *
\subsubsection{Data types}
*                                                                      *
************************************************************************
-}

newtype ForeignCall = CCall CCallSpec
  deriving Eq

isSafeForeignCall :: ForeignCall -> Bool
isSafeForeignCall (CCall (CCallSpec _ _ safe)) = playSafe safe

-- We may need more clues to distinguish foreign calls
-- but this simple printer will do for now
instance Outputable ForeignCall where
  ppr (CCall cc)  = ppr cc

data Safety
  = PlaySafe          -- ^ Might invoke Haskell GC, or do a call back, or
                      --   switch threads, etc.  So make sure things are
                      --   tidy before the call. Additionally, in the threaded
                      --   RTS we arrange for the external call to be executed
                      --   by a separate OS thread, i.e., _concurrently_ to the
                      --   execution of other Haskell threads.

  | PlayInterruptible -- ^ Like PlaySafe, but additionally
                      --   the worker thread running this foreign call may
                      --   be unceremoniously killed, so it must be scheduled
                      --   on an unbound thread.

  | PlayRisky         -- ^ None of the above can happen; the call will return
                      --   without interacting with the runtime system at all.
                      --   Specifically:
                      --
                      --     * No GC
                      --     * No call backs
                      --     * No blocking
                      --     * No precise exceptions
                      --
  deriving ( Eq, Show, Data )
        -- Show used just for Show Lex.Token, I think

instance Outputable Safety where
  ppr PlaySafe = text "safe"
  ppr PlayInterruptible = text "interruptible"
  ppr PlayRisky = text "unsafe"

playSafe :: Safety -> Bool
playSafe PlaySafe = True
playSafe PlayInterruptible = True
playSafe PlayRisky = False

playInterruptible :: Safety -> Bool
playInterruptible PlayInterruptible = True
playInterruptible _ = False

{-
************************************************************************
*                                                                      *
\subsubsection{Calling C}
*                                                                      *
************************************************************************
-}

data CExportSpec
  = CExportStatic               -- foreign export ccall foo :: ty
        SourceText              -- of the CLabelString.
                                -- See note [Pragma source text] in GHC.Types.SourceText
        CLabelString            -- C Name of exported function
        CCallConv
  deriving Data

data CCallSpec
  =  CCallSpec  CCallTarget     -- What to call
                CCallConv       -- Calling convention to use.
                Safety
  deriving( Eq )

-- The call target:

-- | How to call a particular function in C-land.
data CCallTarget
  -- An "unboxed" ccall# to named function in a particular package.
  = StaticTarget
        SourceText                -- of the CLabelString.
                                  -- See note [Pragma source text] in GHC.Types.SourceText
        CLabelString                    -- C-land name of label.

        (Maybe Unit)                    -- What package the function is in.
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

  deriving( Eq, Data )

isDynamicTarget :: CCallTarget -> Bool
isDynamicTarget DynamicTarget = True
isDynamicTarget _             = False

{-
Stuff to do with calling convention:

ccall:          Caller allocates parameters, *and* deallocates them.

stdcall:        Caller allocates parameters, callee deallocates.
                Function name has @N after it, where N is number of arg bytes
                e.g.  _Foo@8. This convention is x86 (win32) specific.

See: http://www.programmersheaven.com/2/Calling-conventions
-}

-- any changes here should be replicated in  the CallConv type in template haskell
data CCallConv = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv
  deriving (Eq, Data)

instance Outputable CCallConv where
  ppr StdCallConv = text "stdcall"
  ppr CCallConv   = text "ccall"
  ppr CApiConv    = text "capi"
  ppr PrimCallConv = text "prim"
  ppr JavaScriptCallConv = text "javascript"

defaultCCallConv :: CCallConv
defaultCCallConv = CCallConv

ccallConvToInt :: CCallConv -> Int
ccallConvToInt StdCallConv = 0
ccallConvToInt CCallConv   = 1
ccallConvToInt CApiConv    = panic "ccallConvToInt CApiConv"
ccallConvToInt (PrimCallConv {}) = panic "ccallConvToInt PrimCallConv"
ccallConvToInt JavaScriptCallConv = panic "ccallConvToInt JavaScriptCallConv"

{-
Generate the gcc attribute corresponding to the given
calling convention (used by PprAbsC):
-}

ccallConvAttribute :: CCallConv -> SDoc
ccallConvAttribute StdCallConv       = text "__attribute__((__stdcall__))"
ccallConvAttribute CCallConv         = empty
ccallConvAttribute CApiConv          = empty
ccallConvAttribute (PrimCallConv {}) = panic "ccallConvAttribute PrimCallConv"
ccallConvAttribute JavaScriptCallConv = panic "ccallConvAttribute JavaScriptCallConv"

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

-- Printing into C files:

instance Outputable CExportSpec where
  ppr (CExportStatic _ str _) = pprCLabelString str

instance Outputable CCallSpec where
  ppr (CCallSpec fun cconv safety)
    = hcat [ whenPprDebug callconv, ppr_fun fun, text " ::" ]
    where
      callconv = text "{-" <> ppr cconv <> text "-}"

      gc_suf | playSafe safety = text "_safe"
             | otherwise       = text "_unsafe"

      ppr_fun (StaticTarget st lbl mPkgId isFun)
        = text (if isFun then "__ffi_static_ccall"
                         else "__ffi_static_ccall_value")
       <> gc_suf
       <+> (case mPkgId of
            Nothing -> empty
            Just pkgId -> ppr pkgId)
       <> text ":"
       <> ppr lbl
       <+> (pprWithSourceText st empty)

      ppr_fun DynamicTarget
        = text "__ffi_dyn_ccall" <> gc_suf <+> text "\"\""

-- The filename for a C header file
-- Note [Pragma source text] in GHC.Types.SourceText
data Header = Header SourceText FastString
    deriving (Eq, Data)

instance Outputable Header where
    ppr (Header st h) = pprWithSourceText st (doubleQuotes $ ppr h)

-- | A C type, used in CAPI FFI calls
--
--  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'{-\# CTYPE'@,
--        'GHC.Parser.Annotation.AnnHeader','GHC.Parser.Annotation.AnnVal',
--        'GHC.Parser.Annotation.AnnClose' @'\#-}'@,

-- For details on above see note [exact print annotations] in "GHC.Parser.Annotation"
data CType = CType SourceText -- Note [Pragma source text] in GHC.Types.SourceText
                   (Maybe Header) -- header to include for this type
                   (SourceText,FastString) -- the type itself
    deriving (Eq, Data)

instance Outputable CType where
    ppr (CType stp mh (stct,ct))
      = pprWithSourceText stp (text "{-# CTYPE") <+> hDoc
        <+> pprWithSourceText stct (doubleQuotes (ftext ct)) <+> text "#-}"
        where hDoc = case mh of
                     Nothing -> empty
                     Just h -> ppr h

{-
************************************************************************
*                                                                      *
\subsubsection{Misc}
*                                                                      *
************************************************************************
-}

instance Binary ForeignCall where
    put_ bh (CCall aa) = put_ bh aa
    get bh = do aa <- get bh; return (CCall aa)

instance Binary Safety where
    put_ bh PlaySafe =
            putByte bh 0
    put_ bh PlayInterruptible =
            putByte bh 1
    put_ bh PlayRisky =
            putByte bh 2
    get bh = do
            h <- getByte bh
            case h of
              0 -> return PlaySafe
              1 -> return PlayInterruptible
              _ -> return PlayRisky

instance Binary CExportSpec where
    put_ bh (CExportStatic ss aa ab) = do
            put_ bh ss
            put_ bh aa
            put_ bh ab
    get bh = do
          ss <- get bh
          aa <- get bh
          ab <- get bh
          return (CExportStatic ss aa ab)

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
    put_ bh (StaticTarget ss aa ab ac) = do
            putByte bh 0
            put_ bh ss
            put_ bh aa
            put_ bh ab
            put_ bh ac
    put_ bh DynamicTarget =
            putByte bh 1
    get bh = do
            h <- getByte bh
            case h of
              0 -> do ss <- get bh
                      aa <- get bh
                      ab <- get bh
                      ac <- get bh
                      return (StaticTarget ss aa ab ac)
              _ -> return DynamicTarget

instance Binary CCallConv where
    put_ bh CCallConv =
            putByte bh 0
    put_ bh StdCallConv =
            putByte bh 1
    put_ bh PrimCallConv =
            putByte bh 2
    put_ bh CApiConv =
            putByte bh 3
    put_ bh JavaScriptCallConv =
            putByte bh 4
    get bh = do
            h <- getByte bh
            case h of
              0 -> return CCallConv
              1 -> return StdCallConv
              2 -> return PrimCallConv
              3 -> return CApiConv
              _ -> return JavaScriptCallConv

instance Binary CType where
    put_ bh (CType s mh fs) = do put_ bh s
                                 put_ bh mh
                                 put_ bh fs
    get bh = do s  <- get bh
                mh <- get bh
                fs <- get bh
                return (CType s mh fs)

instance Binary Header where
    put_ bh (Header s h) = put_ bh s >> put_ bh h
    get bh = do s <- get bh
                h <- get bh
                return (Header s h)
