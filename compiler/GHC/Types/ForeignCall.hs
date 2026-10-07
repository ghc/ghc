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

import GHC.Hs.Extension
import GHC.Types.SourceText (SourceText(..), pprWithSourceText)
import GHC.Unit.Types
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Language.Haskell.Syntax.Decls.Foreign
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Text

import Data.Char
import Data.Data (Data)
import Data.Functor ((<&>))

import Control.DeepSeq (NFData(..))
import GHC.Parser.Annotation (AnnCType, noAnn)

{-
************************************************************************
*                                                                      *
\subsubsection{ForeignCall}
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

instance Binary ForeignCall where
    put_ bh (CCall aa) = put_ bh aa
    get bh = do aa <- get bh; return (CCall aa)

instance NFData ForeignCall where
  rnf (CCall c) = rnf c

{-
************************************************************************
*                                                                      *
\subsubsection{CCallSpec: calling C}
*                                                                      *
************************************************************************
-}

data CCallSpec
  =  CCallSpec
        (CCallTarget GhcTc) -- What to call
        CCallConv           -- Calling convention to use.
        Safety
  deriving (Eq)

pprCLabelString :: CLabelString -> SDoc
pprCLabelString = ppr

isCLabelString :: CLabelString -> Bool  -- Checks to see if this is a valid C label
isCLabelString lbl
  = all ok (unpackHText lbl)
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

instance NFData CCallSpec where
  rnf (CCallSpec t c s) = rnf t `seq` rnf c `seq` rnf s

{-
************************************************************************
*                                                                      *
\subsubsection{Foreign call calling convention}
*                                                                      *
************************************************************************
-}

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

instance Outputable CCallConv where
    ppr StdCallConv  = text "stdcall"
    ppr CCallConv    = text "ccall"
    ppr CApiConv     = text "capi"
    ppr PrimCallConv = text "prim"
    ppr JavaScriptCallConv = text "javascript"

{-
************************************************************************
*                                                                      *
\subsubsection{Foreign call safety}
*                                                                      *
************************************************************************
-}

playSafe :: Safety -> Bool
playSafe PlaySafe = True
playSafe PlayInterruptible = True
playSafe PlayRisky = False

playInterruptible :: Safety -> Bool
playInterruptible PlayInterruptible = True
playInterruptible _ = False

instance Outputable Safety where
    ppr PlaySafe = text "safe"
    ppr PlayInterruptible = text "interruptible"
    ppr PlayRisky = text "unsafe"

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

{-
************************************************************************
*                                                                      *
\subsubsection{C headers}
*                                                                      *
************************************************************************
-}

type instance XHeader  (GhcPass p) = SourceText
type instance XXHeader (GhcPass p) = DataConCantHappen

deriving instance Eq (Header (GhcPass p))

typeCheckHeader :: Header GhcRn -> Header GhcTc
typeCheckHeader (Header a b) = Header a b

renameHeader :: Header GhcPs -> Header GhcRn
renameHeader (Header a b) = Header a b

instance Binary (Header (GhcPass p)) where
    put_ bh (Header s h) = put_ bh s >> put_ bh h
    get bh = do
      s <- get bh
      h <- get bh
      return (Header s h)

instance Outputable (Header (GhcPass p)) where
    ppr (Header st h) = pprWithSourceText st (doubleQuotes $ ppr h)

{-
************************************************************************
*                                                                      *
\subsubsection{CType}
*                                                                      *
************************************************************************
-}

defaultCType :: String -> CType (GhcPass p)
defaultCType =
  CType (CTypeGhc NoSourceText NoSourceText noAnn) Nothing . packHText

mkCType :: SourceText -> SourceText -> AnnCType -> Maybe (Header (GhcPass p)) -> HText -> CType (GhcPass p)
mkCType x y ann m =
  CType (CTypeGhc x y ann) m

typeCheckCType :: CType GhcRn -> CType GhcTc
typeCheckCType (CType x y z) = CType x (typeCheckHeader <$> y) z

data CTypeGhc = CTypeGhc
  { cTypeSourceText :: SourceText
  , cTypeOtherText  :: SourceText
  , cTypeAnn        :: AnnCType
  }
  deriving (Data, Eq)

type instance XCType   (GhcPass p) = CTypeGhc
type instance XXCType  (GhcPass p) = DataConCantHappen

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
        , cTypeAnn        = noAnn
        }

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

instance Outputable (CType (GhcPass p)) where
    ppr (CType ext mh ct) =
        pprWithSourceText stp (text "{-# CTYPE") <+> hDoc <+>
        pprWithSourceText stct (doubleQuotes (ppr ct)) <+> text "#-}"
      where
        stp  = cTypeSourceText ext
        stct = cTypeOtherText  ext
        hDoc = case mh of
          Nothing -> empty
          Just h -> ppr h

{-
************************************************************************
*                                                                      *
\subsubsection{C labels}
*                                                                      *
************************************************************************
-}

-- Note [Tracking labels' target libraries]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Some platforms (Arch + OS) and their linker schemes (ELF, PE, MachO) either
-- /need/ to know or benefit from knowing whether a symbol used in one linker
-- unit references an entity is the /same/ or a /different/ linker unit. That
-- is, they want to distinguish intra-unit references from inter-unit
-- references.
--
-- In this context a "linker unit" means a unit produced by the linker that
-- is used at runtime. This generally includes executables and shared libraries,
-- but not intermediate object files or static archives.
--
--  * With ELF: ELF executable files or DSO shared library files (.so)
--  * With PE: "PE modules": executables (.exe) and dynamic libraries (.dll)
--  * With MachO: executables and dynamic libraries
--
-- With some linker schemes on some CPU architectures, the code generated for
-- intra-unit and inter-unit references are necessarily different, while on
-- others there is a (small) performance opportunity by making a distinction.
--
-- At one extreme, ELF has enough layers of indirection and magic that it does
-- not require any distinction between intra and inter-DSO references for
-- either calls or data. On the other hand, there are some performance
-- opportunities if we know a reference is to an entity within the same DSO.
--
-- The other extreme: Windows\/PE in general /requires/ a distinction between
-- intra-module and inter-module references. It does not natively have a
-- mechanism to /not/ make such a distinction: it is simply expected that this
-- information is always known. This is what the MS extensions to the C
-- language @__declspec(dllimport)@ and @__declspec(dllexport)@ are about.
-- In classic C code developed for Posix platforms this distinction is rarely
-- if ever made. The GNU and LLVM toolchains on Windows provide various
-- mechanisms to support porting Posix software that do not make this
-- distinction. In GHC we track label targets so that we can make the
-- intra-module\/inter-module distinction where we can, and we take advantage
-- of the GNU\/LLVM mechanism where we must.
--
-- See also Note [Windows dll symbol references].
--
-- In GHC our general rule is that:
--
-- 1. for references within cmm code to other cmm code we /always/ know if the
--    reference is local or external, and /typically/ we know exactly in which
--    linker unit the target resides; while
-- 2. for references within cmm code to foreign code (e.g. C calls) we
--    /typically/ do /not/ know in which linker unit the target resides.
--
-- For cmm code generated by GHC from Haskell code, tracking targets is
-- relatively straightforward since we know the Haskell unit id of each
-- definition site. Tracking the actual target unit allows code to be moved
-- around (via inlining etc) and then during final code generation it is
-- straightforward to determine if a reference is local or external by
-- comparing the local unit id with the label's target unit id. With GHC's
-- dynamic linking scheme, each Haskell unit generally corresponds to a single
-- linker unit, i.e. a Haskell library becomes a single .so or .dll file. Other
-- labels created during code generation are local by construction.
--
-- For hand written cmm code, and for FFI "prim" imports of cmm functions, we
-- have syntax that allows specifying where symbols refer to. For imports of
-- symbols within hand written cmm code, the default is that symbols are local
-- but there is syntax to specify a specific Haskell unit, or to specify a
-- general unknown-but-external library. For the latter this means that
-- strictly speaking we do not know the target unit, but we do know it is
-- external, which is the important distinction.
--
-- For foreign calls (e.g. C calls) however we generally do not know the target
-- linker unit. The Haskell foreign function import syntax follows the Posix
-- convention where no distinction is needed between references to code\/data
-- in the local library\/executable versus references to other libraries. There
-- is syntax in FFI ccall imports to specify a header file, but not a library.
-- Indeed the /only/ C calls where we do know the target linker unit is calls
-- to C code generated by GHC itself, for wrapper C functions generated for
-- @foreign import ccall \"wrapper\"@ and @foreign import capi@ declarations.
--
-- See also Note [Windows dll symbol references]


-- Note [Tracking labels' target type]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Similar to label targets, see Note [Tracking labels' target libraries]),
-- we also track whether a label refers to code or data. In the code generator
-- we sometimes need to know the intended usage of a symbol, independent of an
-- actual use site. Linkers can sometimes treat symbols differently depending
-- on their linker "type": code or data. This is especially relevant when using
-- position independent code and dynamic linking.
--
-- For labels arising from Haskell code we know from the kind of thing it is
-- whether it is code or data. See 'CLabel.labelType' and 'CLabel.CLabelType'.
-- For foreign labels we have to track this explicitly.
-- See 'ForeignLabelIsFunctionOrData'.


-- | A C name (closely related to an assembler label or linker symbol), along
-- with /what/ kind of entity the name refers to and /where/ the the entity
-- lives.
--
-- The \"what\" is whether it refers to a C function or C data (e.g. a C global
-- variable or constant). See Note [Tracking labels' target type].
--
-- The \"where\" is where the entity that the name refers to lives.
-- See Note [Tracking labels' target libraries].
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

-- | Specify whether a label (also called a name or linker symbol) refers to
-- executable code (to be called) or to data (to be read or written). We must
-- track this because on some platforms and library schemes the generated code
-- to access the label is different between the two, or the generated code can
-- be faster or smaller if we know this information.
--
-- See Note [Tracking labels' target type].
--
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
\subsubsection{CCallTarget and extended attributes}
*                                                                      *
************************************************************************
-}

isDynamicTarget :: CCallTarget p -> Bool
isDynamicTarget DynamicTarget{} = True
isDynamicTarget _               = False

type instance XStaticTarget   GhcPs      = SourceText
type instance XStaticTarget   GhcRn      = StaticTargetGhc
type instance XStaticTarget   GhcTc      = StaticTargetGhc
type instance XDynamicTarget (GhcPass p) = NoExtField
type instance XXCCallTarget  (GhcPass p) = DataConCantHappen

data StaticTargetGhc = StaticTargetGhc
  { staticTargetLabel :: SourceText
  , staticTargetUnit  :: CLabelTargetLibrary
    -- ^ What linker unit the target of the label is in.
  }
  deriving (Data, Eq)

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

{-
************************************************************************
*                                                                      *
\subsubsection{CLabelTargetLibrary}
*                                                                      *
************************************************************************
-}

-- | Where the entity referred to by the label lives: specifically what linker
-- unit (i.e. executable or shared library).
--
-- This information is used in the code generators (on some platforms) to
-- determine whether a use of label in some linker unit refers to a target
-- within the same (local) linker unit or to a different (external) linker unit.
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
  | CLabelTargetInUnit !UnitId
  deriving (Data, Eq)

instance Outputable CLabelTargetLibrary where
   ppr CLabelTargetUnknown       = parens (text "unknown library")
   ppr (CLabelTargetInUnit unit) = parens (text "in unit " <> ppr unit)

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

{-
************************************************************************
*                                                                      *
\subsubsection{CCallTarget and extended attributes}
*                                                                      *
************************************************************************
-}

instance Binary CExportSpec where
    put_ bh (CExportStatic aa ab) = do
      put_ bh aa
      put_ bh ab
    get bh = do
      aa <- get bh
      ab <- get bh
      return (CExportStatic aa ab)

instance Outputable CExportSpec where
    ppr (CExportStatic str _) = pprCLabelString str

instance Binary ForeignKind where
    put_ bh = putByte bh . \case
      ForeignValue -> 0
      ForeignFunction -> 1
    get bh = getByte bh <&> \case
      0 -> ForeignValue
      _ -> ForeignFunction

