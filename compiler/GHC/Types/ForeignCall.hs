{-# LANGUAGE TypeFamilies #-}
{-
Orphan 'Binary' and 'Outputable' instances for the following types:

  * CCallConv
  * CCallTarget
  * CExportSpec
  * CType
  * Header
  * Safety

To be resolved at a later time, see TODO at the end of this module.

-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[Foreign]{Foreign calls}
-}

module GHC.Types.ForeignCall (
  -- * Foreign function interface declarations
  -- ** Data-type
  ForeignDecl(..),
  -- ** Record synonym
  LForeignDecl,

  -- * Foreign call
  ForeignCall(..),
  -- ** Queries
  isSafeForeignCall,
  -- ** CCallSpec
  CCallSpec(..),
  -- ** CLabelSpec
  CLabelSpec(..),

  -- * Foreign export types
  -- ** Data-type
  ForeignExport(..),
  -- ** Specification
  CExportSpec(..),

  -- * Foreign import types
  -- ** Data-type
  ForeignImport(..),
  -- ** Call target
  CCallTarget(..),
  -- *** GHC extension point
  StaticTargetGhc(..),
  CLabelTargetLibrary(..),
  -- *** Queries
  isDynamicTarget,
  -- ** Foreign target kind
  ForeignKind(..),
  -- ** Safety
  Safety(..),
  -- *** Queries
  playSafe,
  playInterruptible,
  -- ** Specification
  CImportSpec(..),

  -- * Foreign binding type
  -- ** Data-type
  CType(..),
  -- *** Construction
  defaultCType,
  mkCType,
  -- *** Conversion
  typeCheckCType,
  -- *** GHC extension point
  CTypeGhc(..),

  -- * General sub-types
  -- ** CCallConv
  CCallConv(..),
  -- *** Default construction
  defaultCCallConv,
  -- *** Pretty-printing
  ccallConvAttribute,
  -- ** CLabelString
  CLabelString,
  -- *** Queries
  isCLabelString,
  -- *** Pretty-printing
  pprCLabelString,
  -- ** Header
  Header(..),
  -- *** Conversion
  renameHeader,
  typeCheckHeader,
  -- ** ForeignLabelIsFunctionOrData
  ForeignLabelIsFunctionOrData(..),
  ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Hs.Extension
import GHC.Types.SourceText (SourceText(..), pprWithSourceText)
import GHC.Unit.Types
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Language.Haskell.Syntax.Decls.Foreign
import Language.Haskell.Syntax.Extension

import Data.Char
import Data.Data (Data)
import Data.Functor ((<&>))

import Control.DeepSeq (NFData(..))

{-
************************************************************************
*                                                                      *
\subsubsection{Data types}
*                                                                      *
************************************************************************
-}

newtype ForeignCall = CCall CCallSpec
  deriving (Eq)

isSafeForeignCall :: ForeignCall -> Bool
isSafeForeignCall (CCall (CCallSpec _ _ safe)) = playSafe safe

-- We may need more clues to distinguish foreign calls
-- but this simple printer will do for now
instance Outputable ForeignCall where
  ppr (CCall cc)  = ppr cc

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

data CCallSpec
  =  CCallSpec
        (CCallTarget GhcTc) -- What to call
        CCallConv           -- Calling convention to use.
        Safety
  deriving (Eq)

isDynamicTarget :: CCallTarget p -> Bool
isDynamicTarget DynamicTarget{} = True
isDynamicTarget _               = False

defaultCCallConv :: CCallConv
defaultCCallConv = CCallConv

{-
Generate the gcc attribute corresponding to the given
calling convention (used by PprAbsC):
-}

ccallConvAttribute :: CCallConv -> SDoc
ccallConvAttribute StdCallConv       = panic "ccallConvAttribute StdCallConv"
ccallConvAttribute CCallConv         = empty
ccallConvAttribute CApiConv          = empty
ccallConvAttribute (PrimCallConv {}) = panic "ccallConvAttribute PrimCallConv"
ccallConvAttribute JavaScriptCallConv = empty

pprCLabelString :: CLabelString -> SDoc
pprCLabelString lbl = ftext lbl

isCLabelString :: CLabelString -> Bool  -- Checks to see if this is a valid C label
isCLabelString lbl
  = all ok (unpackFS lbl)
  where
    ok c = isAlphaNum c || c == '_' || c == '.' || c == '@'
        -- The '.' appears in e.g. "foo.so" in the
        -- module part of a ExtName.  Maybe it should be separate

-- Printing into C files:

instance Outputable CCallSpec where
  ppr (CCallSpec fun cconv safety)
    = hcat [ whenPprDebug callconv, ppr_fun fun, text " ::" ]
    where
      callconv = text "{-" <> ppr cconv <> text "-}"

      gc_suf | playSafe safety = text "_safe"
             | otherwise       = text "_unsafe"

      ppr_fun = \case
        DynamicTarget{} -> text "__ffi_dyn_ccall" <> gc_suf <+> text "\"\""
        StaticTarget ext label isFun ->
          let pCallType = case isFun of
                ForeignValue    -> text "__ffi_static_ccall_value"
                ForeignFunction -> text "__ffi_static_ccall"
              pprUnit ext = case staticTargetUnit ext of
                CLabelTargetUnknown     -> empty
                CLabelTargetInUnit unit -> ppr unit
              (srcTxt, pPkgId) = (staticTargetLabel ext, pprUnit ext)
          in pCallType
               <> gc_suf
               <+> pPkgId
               <> text ":"
               <> ppr label
               <+> (pprWithSourceText srcTxt empty)

defaultCType :: String -> CType (GhcPass p)
defaultCType =
  CType (CTypeGhc NoSourceText NoSourceText) Nothing . fsLit

mkCType :: SourceText -> SourceText -> Maybe (Header (GhcPass p)) -> FastString -> CType (GhcPass p)
mkCType x y m =
  CType (CTypeGhc x y) m

typeCheckCType :: CType GhcRn -> CType GhcTc
typeCheckCType (CType x y z) = CType x (typeCheckHeader <$> y) z

typeCheckHeader :: Header GhcRn -> Header GhcTc
typeCheckHeader (Header a b) = Header a b

renameHeader :: Header GhcPs -> Header GhcRn
renameHeader (Header a b) = Header a b

{-
************************************************************************
*                                                                      *
\subsubsection{C labels}
*                                                                      *
************************************************************************
-}

-- | A C name (closely related to an assembler label or linker symbol), along
-- with /what/ kind of entity the name refers to and /where/ the the entity
-- lives.
--
-- The \"what\" is whether it refers to a C function or C data (e.g. a C global
-- variable or constant). We track this because we need to know in the backends
-- how to use the name.
--
-- The \"where\" is where the entity that the name refers to lives.
-- Specifically, what shared library it lives in. We track this because on some
-- platforms (especially Windows) the the backend has to generate different
-- code to access symbols depending on whether they are in the current shared
-- library or a different one.
--
-- This is used in Core's representation of 'Literal's, in the 'LitLabel'
-- case, to represent the address of a C entity (function or data) by its name
-- (also called a label or symbol). It gets used in the representation of FFI
-- imports of the address of C names, like:
--
-- > foreign import ccall "foo.h &foo" foo :: Ptr CInt
--
data CLabelSpec
   = CLabelSpec
       !CLabelString            -- name
       !CLabelIsFunctionOrData  -- what
       !CLabelTargetLibrary     -- where
  deriving (Eq, Data)

type CLabelIsFunctionOrData = ForeignLabelIsFunctionOrData

instance Binary CLabelSpec where
    put_ bh (CLabelSpec lbl fod tgt) = do
        put_ bh lbl
        put_ bh fod
        put_ bh tgt
    get bh = do
        lbl <- get bh
        fod <- get bh
        tgt <- get bh
        return (CLabelSpec lbl fod tgt)

instance NFData CLabelSpec where
    rnf (CLabelSpec lbl fod tgt) = rnf lbl `seq` rnf fod `seq` rnf tgt

{-
************************************************************************
*                                                                      *
\subsection{ForeignLabelIsFunctionOrData}
*                                                                      *
************************************************************************
-}

data ForeignLabelIsFunctionOrData = ForeignLabelIsFunction | ForeignLabelIsData
    deriving (Eq, Ord, Data)

instance Outputable ForeignLabelIsFunctionOrData where
    ppr ForeignLabelIsFunction = text "(function)"
    ppr ForeignLabelIsData     = text "(data)"

instance Binary ForeignLabelIsFunctionOrData where
    put_ bh ForeignLabelIsFunction = putByte bh 0
    put_ bh ForeignLabelIsData     = putByte bh 1
    get bh = do
        h <- getByte bh
        case h of
          0 -> return ForeignLabelIsFunction
          1 -> return ForeignLabelIsData
          _ -> panic "Binary ForeignLabelIsFunctionOrData"

instance NFData ForeignLabelIsFunctionOrData where
  rnf ForeignLabelIsFunction = ()
  rnf ForeignLabelIsData = ()

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

instance NFData ForeignCall where
  rnf (CCall c) = rnf c

instance NFData CCallSpec where
  rnf (CCallSpec t c s) = rnf t `seq` rnf c `seq` rnf s

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

-- | Where the target of the C label is: specifically what shared library (if
-- building with shared libraries).
--
-- This information is used in the code generators (on some platforms) to
-- determine whether a reference using the label is in the same shared library
-- as the target, or if the target is in a different library.
--
-- For normal user-specified FFI imports, we do not know, which uses
-- 'CLabelTargetUnknown'. Some internally generated labels where the target is
-- known (e.g. the RTS) use 'CLabelTargetInUnit'.
--
data CLabelTargetLibrary

    -- | The entity (that the name\/label points to) is in an unknown shared
    -- library. In particular it could either be in the current library (where
    -- the label is used) or an external one. This case is used for all
    -- user-written Haskell FFI ccall\/capi imports, because in this case we do
    -- not know where the entity the name refers to lives.
  = CLabelTargetUnknown

    -- | The entity is /known/ to live in a specific Haskell unit (package),
    -- and thus the shared library corresponding to the unit. Uses of this
    -- label within the same unit will be intra-library, and inter-library
    -- otherwise.
  | CLabelTargetInUnit !Unit
  deriving (Data, Eq)

instance Outputable CLabelTargetLibrary where
   ppr CLabelTargetUnknown       = parens (text "unknown library")
   ppr (CLabelTargetInUnit unit) = parens (text "in unit " <> ppr unit)


data StaticTargetGhc = StaticTargetGhc
  { staticTargetLabel :: SourceText
  , staticTargetUnit  :: CLabelTargetLibrary
  }
  deriving (Data, Eq)

data CTypeGhc = CTypeGhc
  { cTypeSourceText :: SourceText
  , cTypeOtherText  :: SourceText
  }
  deriving (Data, Eq)

type instance XStaticTarget   GhcPs      = SourceText
type instance XStaticTarget   GhcRn      = StaticTargetGhc
type instance XStaticTarget   GhcTc      = StaticTargetGhc
type instance XDynamicTarget (GhcPass p) = NoExtField
type instance XXCCallTarget  (GhcPass p) = DataConCantHappen

type instance XCType   (GhcPass p) = CTypeGhc
type instance XXCType  (GhcPass p) = DataConCantHappen

type instance XHeader  (GhcPass p) = SourceText
type instance XXHeader (GhcPass p) = DataConCantHappen

deriving instance Eq (Header (GhcPass p))

instance NFData (CType (GhcPass p)) where
    rnf (CType ext mh fs) =
      rnf ext `seq` rnf mh `seq` rnf fs

instance NFData (Header (GhcPass p)) where
    rnf (Header s h) =
      rnf s `seq` rnf h

instance NFData CLabelTargetLibrary where
    rnf = \case
      CLabelTargetUnknown     -> ()
      CLabelTargetInUnit unit -> rnf unit

instance Binary CLabelTargetLibrary where
    put_ bh = \case
      CLabelTargetUnknown     -> putByte bh 0
      CLabelTargetInUnit unit -> putByte bh 1 *> put_ bh unit

    get bh = getByte bh >>= \case
      0 -> pure CLabelTargetUnknown
      _ -> CLabelTargetInUnit <$> get bh

instance NFData CTypeGhc where
    rnf st =
      rnf (cTypeSourceText st) `seq`
      rnf (cTypeOtherText  st)

instance Binary CTypeGhc where
    put_ bh ct = do
      put_ bh (cTypeSourceText ct)
      put_ bh (cTypeOtherText  ct)
    get bh = do
      str1 <- get bh
      str2  <- get bh
      return $ CTypeGhc
        { cTypeSourceText = str1
        , cTypeOtherText  = str2
        }

instance NFData StaticTargetGhc where
    rnf st =
      rnf (staticTargetLabel st) `seq`
      rnf (staticTargetUnit  st)

instance Binary StaticTargetGhc where
    put_ bh st = do
      put_ bh (staticTargetLabel st)
      put_ bh (staticTargetUnit st)

    get bh = do
      label <- get bh
      unit  <- get bh
      return $ StaticTargetGhc
        { staticTargetLabel = label
        , staticTargetUnit  = unit
        }

instance forall p. IsPass p => Eq (CCallTarget (GhcPass p)) where
    (==) = \case
      DynamicTarget{} -> \case
        DynamicTarget{} -> True
        _ -> False
      StaticTarget x1 a1 b1 -> \case
        StaticTarget x2 a2 b2 -> a1 == a2 && b1 == b2 && case ghcPass @p of
          GhcPs -> x1 == x2
          GhcRn -> x1 == x2
          GhcTc -> x1 == x2
        _ -> False

instance forall p. IsPass p => NFData (CCallTarget (GhcPass p)) where
    rnf = \case
      DynamicTarget NoExtField -> ()
      StaticTarget x a b -> rnf a `seq` rnf b `seq` case ghcPass @p of
        GhcPs -> rnf x
        GhcRn -> rnf x
        GhcTc -> rnf x

instance forall p. IsPass p => Binary (CCallTarget (GhcPass p)) where
    put_ bh = \case
      StaticTarget x a b -> do
        putByte bh 0
        put_ bh a
        put_ bh b
        case ghcPass @p of
          GhcPs -> put_ bh x
          GhcRn -> put_ bh x
          GhcTc -> put_ bh x

      DynamicTarget NoExtField -> putByte bh 1

    get bh = do
      h <- getByte bh
      case h of
        0 -> do
          (a :: CLabelString) <- get bh
          (b :: ForeignKind ) <- get bh
          case ghcPass @p of
            GhcPs -> (\x -> StaticTarget x a b) <$> get bh
            GhcRn -> (\x -> StaticTarget x a b) <$> get bh
            GhcTc -> (\x -> StaticTarget x a b) <$> get bh

        _ -> return $ DynamicTarget NoExtField

instance Binary CExportSpec where
    put_ bh (CExportStatic aa ab) = do
      put_ bh aa
      put_ bh ab
    get bh = do
      aa <- get bh
      ab <- get bh
      return (CExportStatic aa ab)

instance Binary (CType (GhcPass p)) where
    put_ bh (CType ext mh fs) = do
        put_ bh ext
        put_ bh mh
        put_ bh fs
    get bh = do
      ext <- get bh
      mh  <- get bh
      fs  <- get bh
      return (CType ext mh fs)

instance Binary ForeignKind where
    put_ bh = putByte bh . \case
      ForeignValue -> 0
      ForeignFunction -> 1
    get bh = getByte bh <&> \case
      0 -> ForeignValue
      _ -> ForeignFunction

instance Binary (Header (GhcPass p)) where
    put_ bh (Header s h) = put_ bh s >> put_ bh h
    get bh = do
      s <- get bh
      h <- get bh
      return (Header s h)

instance Binary Safety where
    put_ bh = putByte bh . \case
      PlaySafe -> 0
      PlayInterruptible -> 1
      PlayRisky -> 2

    get bh = do
            h <- getByte bh
            case h of
              0 -> return PlaySafe
              1 -> return PlayInterruptible
              _ -> return PlayRisky

instance Outputable CCallConv where
    ppr StdCallConv = text "stdcall"
    ppr CCallConv   = text "ccall"
    ppr CApiConv    = text "capi"
    ppr PrimCallConv = text "prim"
    ppr JavaScriptCallConv = text "javascript"

instance Outputable CExportSpec where
    ppr (CExportStatic str _) = pprCLabelString str

instance Outputable (CType (GhcPass p)) where
    ppr (CType ext mh ct) =
        pprWithSourceText stp (text "{-# CTYPE") <+> hDoc <+>
        pprWithSourceText stct (doubleQuotes (ftext ct)) <+> text "#-}"
      where
        stp  = cTypeSourceText ext
        stct = cTypeOtherText  ext
        hDoc = case mh of
          Nothing -> empty
          Just h -> ppr h

instance Outputable (Header (GhcPass p)) where
    ppr (Header st h) = pprWithSourceText st (doubleQuotes $ ppr h)

instance Outputable Safety where
    ppr PlaySafe = text "safe"
    ppr PlayInterruptible = text "interruptible"
    ppr PlayRisky = text "unsafe"
