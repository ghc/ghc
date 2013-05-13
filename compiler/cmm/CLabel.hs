-----------------------------------------------------------------------------
--
-- Object-file symbols (called CLabel for histerical raisins).
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CLabel (
        CLabel, -- abstract type
        ForeignLabelSource(..),
        pprDebugCLabel,

        mkClosureLabel,
        mkSRTLabel,
        mkTopSRTLabel,
        mkInfoTableLabel,
        mkEntryLabel,
        mkSlowEntryLabel,
        mkConEntryLabel,
        mkStaticConEntryLabel,
        mkRednCountsLabel,
        mkConInfoTableLabel,
        mkStaticInfoTableLabel,
        mkLargeSRTLabel,
        mkApEntryLabel,
        mkApInfoTableLabel,
        mkClosureTableLabel,

        mkLocalClosureLabel,
        mkLocalInfoTableLabel,
        mkLocalEntryLabel,
        mkLocalConEntryLabel,
        mkLocalStaticConEntryLabel,
        mkLocalConInfoTableLabel,
        mkLocalStaticInfoTableLabel,
        mkLocalClosureTableLabel,

        mkReturnPtLabel,
        mkReturnInfoLabel,
        mkAltLabel,
        mkDefaultLabel,
        mkBitmapLabel,
        mkStringLitLabel,

        mkAsmTempLabel,

        mkPlainModuleInitLabel,

        mkSplitMarkerLabel,
        mkDirty_MUT_VAR_Label,
        mkUpdInfoLabel,
        mkBHUpdInfoLabel,
        mkIndStaticInfoLabel,
        mkMainCapabilityLabel,
        mkMAP_FROZEN_infoLabel,
        mkMAP_DIRTY_infoLabel,
        mkEMPTY_MVAR_infoLabel,

        mkTopTickyCtrLabel,
        mkCAFBlackHoleInfoTableLabel,
        mkCAFBlackHoleEntryLabel,
        mkRtsPrimOpLabel,
        mkRtsSlowFastTickyCtrLabel,

        mkSelectorInfoLabel,
        mkSelectorEntryLabel,

        mkCmmInfoLabel,
        mkCmmEntryLabel,
        mkCmmRetInfoLabel,
        mkCmmRetLabel,
        mkCmmCodeLabel,
        mkCmmDataLabel,
        mkCmmClosureLabel,

        mkRtsApFastLabel,

        mkPrimCallLabel,

        mkForeignLabel,
        addLabelSize,
        foreignLabelStdcallInfo,

        mkCCLabel, mkCCSLabel,

        DynamicLinkerLabelInfo(..),
        mkDynamicLinkerLabel,
        dynamicLinkerLabelInfo,

        mkPicBaseLabel,
        mkDeadStripPreventer,

        mkHpcTicksLabel,

        hasCAF,
        needsCDecl, isAsmTemp, maybeAsmTemp, externallyVisibleCLabel,
        isMathFun,
        isCFunctionLabel, isGcPtrLabel, labelDynamic,

        -- * Conversions
        toClosureLbl, toSlowEntryLbl, toEntryLbl, toInfoLbl, toRednCountsLbl, hasHaskellName,

        pprCLabel
    ) where

import IdInfo
import BasicTypes
import Packages
import Module
import Name
import Unique
import PrimOp
import Config
import CostCentre
import Outputable
import FastString
import DynFlags
import Platform
import UniqSet

-- -----------------------------------------------------------------------------
-- The CLabel type

{-
  | CLabel is an abstract type that supports the following operations:

  - Pretty printing

  - In a C file, does it need to be declared before use?  (i.e. is it
    guaranteed to be already in scope in the places we need to refer to it?)

  - If it needs to be declared, what type (code or data) should it be
    declared to have?

  - Is it visible outside this object file or not?

  - Is it "dynamic" (see details below)

  - Eq and Ord, so that we can make sets of CLabels (currently only
    used in outputting C as far as I can tell, to avoid generating
    more than one declaration for any given label).

  - Converting an info table label into an entry label.
-}

data CLabel
  = -- | A label related to the definition of a particular Id or Con in a .hs file.
    IdLabel
        Name
        CafInfo
        IdLabelInfo             -- encodes the suffix of the label

  -- | A label from a .cmm file that is not associated with a .hs level Id.
  | CmmLabel
        PackageId               -- what package the label belongs to.
        FastString              -- identifier giving the prefix of the label
        CmmLabelInfo            -- encodes the suffix of the label

  -- | A label with a baked-in \/ algorithmically generated name that definitely
  --    comes from the RTS. The code for it must compile into libHSrts.a \/ libHSrts.so
  --    If it doesn't have an algorithmically generated name then use a CmmLabel
  --    instead and give it an appropriate PackageId argument.
  | RtsLabel
        RtsLabelInfo

  -- | A 'C' (or otherwise foreign) label.
  --
  | ForeignLabel
        FastString              -- name of the imported label.

        (Maybe Int)             -- possible '@n' suffix for stdcall functions
                                -- When generating C, the '@n' suffix is omitted, but when
                                -- generating assembler we must add it to the label.

        ForeignLabelSource      -- what package the foreign label is in.

        FunctionOrData

  -- | A family of labels related to a particular case expression.
  | CaseLabel
        {-# UNPACK #-} !Unique  -- Unique says which case expression
        CaseLabelInfo

  | AsmTempLabel
        {-# UNPACK #-} !Unique

  | StringLitLabel
        {-# UNPACK #-} !Unique

  | PlainModuleInitLabel        -- without the version & way info
        Module

  | CC_Label  CostCentre
  | CCS_Label CostCentreStack


  -- | These labels are generated and used inside the NCG only.
  --    They are special variants of a label used for dynamic linking
  --    see module PositionIndependentCode for details.
  | DynamicLinkerLabel DynamicLinkerLabelInfo CLabel

  -- | This label is generated and used inside the NCG only.
  --    It is used as a base for PIC calculations on some platforms.
  --    It takes the form of a local numeric assembler label '1'; and
  --    is pretty-printed as 1b, referring to the previous definition
  --    of 1: in the assembler source file.
  | PicBaseLabel

  -- | A label before an info table to prevent excessive dead-stripping on darwin
  | DeadStripPreventer CLabel


  -- | Per-module table of tick locations
  | HpcTicksLabel Module

  -- | Static reference table
  | SRTLabel !Unique

  -- | Label of an StgLargeSRT
  | LargeSRTLabel
        {-# UNPACK #-} !Unique

  -- | A bitmap (function or case return)
  | LargeBitmapLabel
        {-# UNPACK #-} !Unique

  deriving (Eq, Ord)


-- | Record where a foreign label is stored.
data ForeignLabelSource

   -- | Label is in a named package
   = ForeignLabelInPackage      PackageId

   -- | Label is in some external, system package that doesn't also
   --   contain compiled Haskell code, and is not associated with any .hi files.
   --   We don't have to worry about Haskell code being inlined from
   --   external packages. It is safe to treat the RTS package as "external".
   | ForeignLabelInExternalPackage

   -- | Label is in the package currenly being compiled.
   --   This is only used for creating hacky tmp labels during code generation.
   --   Don't use it in any code that might be inlined across a package boundary
   --   (ie, core code) else the information will be wrong relative to the
   --   destination module.
   | ForeignLabelInThisPackage

   deriving (Eq, Ord)


-- | For debugging problems with the CLabel representation.
--      We can't make a Show instance for CLabel because lots of its components don't have instances.
--      The regular Outputable instance only shows the label name, and not its other info.
--
pprDebugCLabel :: CLabel -> SDoc
pprDebugCLabel lbl
 = case lbl of
        IdLabel{}       -> ppr lbl <> (parens $ text "IdLabel")
        CmmLabel pkg _name _info
         -> ppr lbl <> (parens $ text "CmmLabel" <+> ppr pkg)

        RtsLabel{}      -> ppr lbl <> (parens $ text "RtsLabel")

        ForeignLabel _name mSuffix src funOrData
            -> ppr lbl <> (parens $ text "ForeignLabel"
                                <+> ppr mSuffix
                                <+> ppr src
                                <+> ppr funOrData)

        _               -> ppr lbl <> (parens $ text "other CLabel)")


data IdLabelInfo
  = Closure             -- ^ Label for closure
  | SRT                 -- ^ Static reference table (TODO: could be removed
                        -- with the old code generator, but might be needed
                        -- when we implement the New SRT Plan)
  | InfoTable           -- ^ Info tables for closures; always read-only
  | Entry               -- ^ Entry point
  | Slow                -- ^ Slow entry point

  | LocalInfoTable      -- ^ Like InfoTable but not externally visible
  | LocalEntry          -- ^ Like Entry but not externally visible

  | RednCounts          -- ^ Label of place to keep Ticky-ticky  info for this Id

  | ConEntry            -- ^ Constructor entry point
  | ConInfoTable        -- ^ Corresponding info table
  | StaticConEntry      -- ^ Static constructor entry point
  | StaticInfoTable     -- ^ Corresponding info table

  | ClosureTable        -- ^ Table of closures for Enum tycons

  deriving (Eq, Ord)


data CaseLabelInfo
  = CaseReturnPt
  | CaseReturnInfo
  | CaseAlt ConTag
  | CaseDefault
  deriving (Eq, Ord)


data RtsLabelInfo
  = RtsSelectorInfoTable Bool{-updatable-} Int{-offset-}  -- ^ Selector thunks
  | RtsSelectorEntry     Bool{-updatable-} Int{-offset-}

  | RtsApInfoTable       Bool{-updatable-} Int{-arity-}    -- ^ AP thunks
  | RtsApEntry           Bool{-updatable-} Int{-arity-}

  | RtsPrimOp PrimOp
  | RtsApFast     FastString    -- ^ _fast versions of generic apply
  | RtsSlowFastTickyCtr String

  deriving (Eq, Ord)
  -- NOTE: Eq on LitString compares the pointer only, so this isn't
  -- a real equality.


-- | What type of Cmm label we're dealing with.
--      Determines the suffix appended to the name when a CLabel.CmmLabel
--      is pretty printed.
data CmmLabelInfo
  = CmmInfo                     -- ^ misc rts info tabless,     suffix _info
  | CmmEntry                    -- ^ misc rts entry points,     suffix _entry
  | CmmRetInfo                  -- ^ misc rts ret info tables,  suffix _info
  | CmmRet                      -- ^ misc rts return points,    suffix _ret
  | CmmData                     -- ^ misc rts data bits, eg CHARLIKE_closure
  | CmmCode                     -- ^ misc rts code
  | CmmClosure                  -- ^ closures eg CHARLIKE_closure
  | CmmPrimCall                 -- ^ a prim call to some hand written Cmm code
  deriving (Eq, Ord)

data DynamicLinkerLabelInfo
  = CodeStub                    -- MachO: Lfoo$stub, ELF: foo@plt
  | SymbolPtr                   -- MachO: Lfoo$non_lazy_ptr, Windows: __imp_foo
  | GotSymbolPtr                -- ELF: foo@got
  | GotSymbolOffset             -- ELF: foo@gotoff

  deriving (Eq, Ord)


-- -----------------------------------------------------------------------------
-- Constructing CLabels
-- -----------------------------------------------------------------------------

-- Constructing IdLabels
-- These are always local:
mkSlowEntryLabel :: Name -> CafInfo -> CLabel
mkSlowEntryLabel        name c         = IdLabel name  c Slow

mkTopSRTLabel     :: Unique -> CLabel
mkTopSRTLabel u = SRTLabel u

mkSRTLabel        :: Name -> CafInfo -> CLabel
mkRednCountsLabel :: Name -> CLabel
mkSRTLabel              name c  = IdLabel name  c SRT
mkRednCountsLabel       name    =
  IdLabel name NoCafRefs RednCounts  -- Note [ticky for LNE]

-- These have local & (possibly) external variants:
mkLocalClosureLabel      :: Name -> CafInfo -> CLabel
mkLocalInfoTableLabel    :: Name -> CafInfo -> CLabel
mkLocalEntryLabel        :: Name -> CafInfo -> CLabel
mkLocalClosureTableLabel :: Name -> CafInfo -> CLabel
mkLocalClosureLabel     name c  = IdLabel name  c Closure
mkLocalInfoTableLabel   name c  = IdLabel name  c LocalInfoTable
mkLocalEntryLabel       name c  = IdLabel name  c LocalEntry
mkLocalClosureTableLabel name c = IdLabel name  c ClosureTable

mkClosureLabel              :: Name -> CafInfo -> CLabel
mkInfoTableLabel            :: Name -> CafInfo -> CLabel
mkEntryLabel                :: Name -> CafInfo -> CLabel
mkClosureTableLabel         :: Name -> CafInfo -> CLabel
mkLocalConInfoTableLabel    :: CafInfo -> Name -> CLabel
mkLocalConEntryLabel        :: CafInfo -> Name -> CLabel
mkLocalStaticInfoTableLabel :: CafInfo -> Name -> CLabel
mkLocalStaticConEntryLabel  :: CafInfo -> Name -> CLabel
mkConInfoTableLabel         :: Name -> CafInfo -> CLabel
mkStaticInfoTableLabel      :: Name -> CafInfo -> CLabel
mkClosureLabel name         c     = IdLabel name c Closure
mkInfoTableLabel name       c     = IdLabel name c InfoTable
mkEntryLabel name           c     = IdLabel name c Entry
mkClosureTableLabel name    c     = IdLabel name c ClosureTable
mkLocalConInfoTableLabel    c con = IdLabel con c ConInfoTable
mkLocalConEntryLabel        c con = IdLabel con c ConEntry
mkLocalStaticInfoTableLabel c con = IdLabel con c StaticInfoTable
mkLocalStaticConEntryLabel  c con = IdLabel con c StaticConEntry
mkConInfoTableLabel name    c     = IdLabel name c ConInfoTable
mkStaticInfoTableLabel name c     = IdLabel name c StaticInfoTable

mkConEntryLabel       :: Name -> CafInfo -> CLabel
mkStaticConEntryLabel :: Name -> CafInfo -> CLabel
mkConEntryLabel name        c     = IdLabel name c ConEntry
mkStaticConEntryLabel name  c     = IdLabel name c StaticConEntry

-- Constructing Cmm Labels
mkDirty_MUT_VAR_Label, mkSplitMarkerLabel, mkUpdInfoLabel,
    mkBHUpdInfoLabel, mkIndStaticInfoLabel, mkMainCapabilityLabel,
    mkMAP_FROZEN_infoLabel, mkMAP_DIRTY_infoLabel,
    mkEMPTY_MVAR_infoLabel, mkTopTickyCtrLabel,
    mkCAFBlackHoleInfoTableLabel, mkCAFBlackHoleEntryLabel :: CLabel
mkDirty_MUT_VAR_Label           = mkForeignLabel (fsLit "dirty_MUT_VAR") Nothing ForeignLabelInExternalPackage IsFunction
mkSplitMarkerLabel              = CmmLabel rtsPackageId (fsLit "__stg_split_marker")    CmmCode
mkUpdInfoLabel                  = CmmLabel rtsPackageId (fsLit "stg_upd_frame")         CmmInfo
mkBHUpdInfoLabel                = CmmLabel rtsPackageId (fsLit "stg_bh_upd_frame" )     CmmInfo
mkIndStaticInfoLabel            = CmmLabel rtsPackageId (fsLit "stg_IND_STATIC")        CmmInfo
mkMainCapabilityLabel           = CmmLabel rtsPackageId (fsLit "MainCapability")        CmmData
mkMAP_FROZEN_infoLabel          = CmmLabel rtsPackageId (fsLit "stg_MUT_ARR_PTRS_FROZEN0") CmmInfo
mkMAP_DIRTY_infoLabel           = CmmLabel rtsPackageId (fsLit "stg_MUT_ARR_PTRS_DIRTY") CmmInfo
mkEMPTY_MVAR_infoLabel          = CmmLabel rtsPackageId (fsLit "stg_EMPTY_MVAR")        CmmInfo
mkTopTickyCtrLabel              = CmmLabel rtsPackageId (fsLit "top_ct")                CmmData
mkCAFBlackHoleInfoTableLabel    = CmmLabel rtsPackageId (fsLit "stg_CAF_BLACKHOLE")     CmmInfo
mkCAFBlackHoleEntryLabel        = CmmLabel rtsPackageId (fsLit "stg_CAF_BLACKHOLE")     CmmEntry

-----
mkCmmInfoLabel,   mkCmmEntryLabel, mkCmmRetInfoLabel, mkCmmRetLabel,
  mkCmmCodeLabel, mkCmmDataLabel,  mkCmmClosureLabel
        :: PackageId -> FastString -> CLabel

mkCmmInfoLabel      pkg str     = CmmLabel pkg str CmmInfo
mkCmmEntryLabel     pkg str     = CmmLabel pkg str CmmEntry
mkCmmRetInfoLabel   pkg str     = CmmLabel pkg str CmmRetInfo
mkCmmRetLabel       pkg str     = CmmLabel pkg str CmmRet
mkCmmCodeLabel      pkg str     = CmmLabel pkg str CmmCode
mkCmmDataLabel      pkg str     = CmmLabel pkg str CmmData
mkCmmClosureLabel   pkg str     = CmmLabel pkg str CmmClosure


-- Constructing RtsLabels
mkRtsPrimOpLabel :: PrimOp -> CLabel
mkRtsPrimOpLabel primop         = RtsLabel (RtsPrimOp primop)

mkSelectorInfoLabel  :: Bool -> Int -> CLabel
mkSelectorEntryLabel :: Bool -> Int -> CLabel
mkSelectorInfoLabel  upd off    = RtsLabel (RtsSelectorInfoTable upd off)
mkSelectorEntryLabel upd off    = RtsLabel (RtsSelectorEntry     upd off)

mkApInfoTableLabel :: Bool -> Int -> CLabel
mkApEntryLabel     :: Bool -> Int -> CLabel
mkApInfoTableLabel   upd off    = RtsLabel (RtsApInfoTable       upd off)
mkApEntryLabel       upd off    = RtsLabel (RtsApEntry           upd off)


-- A call to some primitive hand written Cmm code
mkPrimCallLabel :: PrimCall -> CLabel
mkPrimCallLabel (PrimCall str pkg)
        = CmmLabel pkg str CmmPrimCall


-- Constructing ForeignLabels

-- | Make a foreign label
mkForeignLabel
        :: FastString           -- name
        -> Maybe Int            -- size prefix
        -> ForeignLabelSource   -- what package it's in
        -> FunctionOrData
        -> CLabel

mkForeignLabel str mb_sz src fod
    = ForeignLabel str mb_sz src  fod


-- | Update the label size field in a ForeignLabel
addLabelSize :: CLabel -> Int -> CLabel
addLabelSize (ForeignLabel str _ src  fod) sz
    = ForeignLabel str (Just sz) src fod
addLabelSize label _
    = label

-- | Get the label size field from a ForeignLabel
foreignLabelStdcallInfo :: CLabel -> Maybe Int
foreignLabelStdcallInfo (ForeignLabel _ info _ _) = info
foreignLabelStdcallInfo _lbl = Nothing


-- Constructing Large*Labels
mkLargeSRTLabel :: Unique -> CLabel
mkBitmapLabel   :: Unique -> CLabel
mkLargeSRTLabel uniq            = LargeSRTLabel uniq
mkBitmapLabel   uniq            = LargeBitmapLabel uniq


-- Constructin CaseLabels
mkReturnPtLabel   :: Unique -> CLabel
mkReturnInfoLabel :: Unique -> CLabel
mkAltLabel        :: Unique -> ConTag -> CLabel
mkDefaultLabel    :: Unique -> CLabel
mkReturnPtLabel uniq            = CaseLabel uniq CaseReturnPt
mkReturnInfoLabel uniq          = CaseLabel uniq CaseReturnInfo
mkAltLabel      uniq tag        = CaseLabel uniq (CaseAlt tag)
mkDefaultLabel  uniq            = CaseLabel uniq CaseDefault

-- Constructing Cost Center Labels
mkCCLabel  :: CostCentre      -> CLabel
mkCCSLabel :: CostCentreStack -> CLabel
mkCCLabel           cc          = CC_Label cc
mkCCSLabel          ccs         = CCS_Label ccs

mkRtsApFastLabel :: FastString -> CLabel
mkRtsApFastLabel str = RtsLabel (RtsApFast str)

mkRtsSlowFastTickyCtrLabel :: String -> CLabel
mkRtsSlowFastTickyCtrLabel pat = RtsLabel (RtsSlowFastTickyCtr pat)


-- Constructing Code Coverage Labels
mkHpcTicksLabel :: Module -> CLabel
mkHpcTicksLabel                = HpcTicksLabel


-- Constructing labels used for dynamic linking
mkDynamicLinkerLabel :: DynamicLinkerLabelInfo -> CLabel -> CLabel
mkDynamicLinkerLabel            = DynamicLinkerLabel

dynamicLinkerLabelInfo :: CLabel -> Maybe (DynamicLinkerLabelInfo, CLabel)
dynamicLinkerLabelInfo (DynamicLinkerLabel info lbl) = Just (info, lbl)
dynamicLinkerLabelInfo _        = Nothing

mkPicBaseLabel :: CLabel
mkPicBaseLabel                  = PicBaseLabel


-- Constructing miscellaneous other labels
mkDeadStripPreventer :: CLabel -> CLabel
mkDeadStripPreventer lbl        = DeadStripPreventer lbl

mkStringLitLabel :: Unique -> CLabel
mkStringLitLabel                = StringLitLabel

mkAsmTempLabel :: Uniquable a => a -> CLabel
mkAsmTempLabel a                = AsmTempLabel (getUnique a)

mkPlainModuleInitLabel :: Module -> CLabel
mkPlainModuleInitLabel mod      = PlainModuleInitLabel mod

-- -----------------------------------------------------------------------------
-- Convert between different kinds of label

toClosureLbl :: CLabel -> CLabel
toClosureLbl (IdLabel n c _) = IdLabel n c Closure
toClosureLbl (CmmLabel m str _) = CmmLabel m str CmmClosure
toClosureLbl l = pprPanic "toClosureLbl" (ppr l)

toSlowEntryLbl :: CLabel -> CLabel
toSlowEntryLbl (IdLabel n c _) = IdLabel n c Slow
toSlowEntryLbl l = pprPanic "toSlowEntryLbl" (ppr l)

toEntryLbl :: CLabel -> CLabel
toEntryLbl (IdLabel n c LocalInfoTable)  = IdLabel n c LocalEntry
toEntryLbl (IdLabel n c ConInfoTable)    = IdLabel n c ConEntry
toEntryLbl (IdLabel n c StaticInfoTable) = IdLabel n c StaticConEntry
toEntryLbl (IdLabel n c _)               = IdLabel n c Entry
toEntryLbl (CaseLabel n CaseReturnInfo)  = CaseLabel n CaseReturnPt
toEntryLbl (CmmLabel m str CmmInfo)      = CmmLabel m str CmmEntry
toEntryLbl (CmmLabel m str CmmRetInfo)   = CmmLabel m str CmmRet
toEntryLbl l = pprPanic "toEntryLbl" (ppr l)

toInfoLbl :: CLabel -> CLabel
toInfoLbl (IdLabel n c Entry)          = IdLabel n c InfoTable
toInfoLbl (IdLabel n c LocalEntry)     = IdLabel n c LocalInfoTable
toInfoLbl (IdLabel n c ConEntry)       = IdLabel n c ConInfoTable
toInfoLbl (IdLabel n c StaticConEntry) = IdLabel n c StaticInfoTable
toInfoLbl (IdLabel n c _)              = IdLabel n c InfoTable
toInfoLbl (CaseLabel n CaseReturnPt)   = CaseLabel n CaseReturnInfo
toInfoLbl (CmmLabel m str CmmEntry)    = CmmLabel m str CmmInfo
toInfoLbl (CmmLabel m str CmmRet)      = CmmLabel m str CmmRetInfo
toInfoLbl l = pprPanic "CLabel.toInfoLbl" (ppr l)

toRednCountsLbl :: CLabel -> Maybe CLabel
toRednCountsLbl = fmap mkRednCountsLabel . hasHaskellName

hasHaskellName :: CLabel -> Maybe Name
hasHaskellName (IdLabel n _ _) = Just n
hasHaskellName _               = Nothing

-- -----------------------------------------------------------------------------
-- Does a CLabel's referent itself refer to a CAF?
hasCAF :: CLabel -> Bool
hasCAF (IdLabel _ _ RednCounts) = False -- Note [ticky for LNE]
hasCAF (IdLabel _ MayHaveCafRefs _) = True
hasCAF _                            = False

-- Note [ticky for LNE]
-- ~~~~~~~~~~~~~~~~~~~~~

-- Until 14 Feb 2013, every ticky counter was associated with a
-- closure. Thus, ticky labels used IdLabel. It is odd that
-- CmmBuildInfoTables.cafTransfers would consider such a ticky label
-- reason to add the name to the CAFEnv (and thus eventually the SRT),
-- but it was harmless because the ticky was only used if the closure
-- was also.
--
-- Since we now have ticky counters for LNEs, it is no longer the case
-- that every ticky counter has an actual closure. So I changed the
-- generation of ticky counters' CLabels to not result in their
-- associated id ending up in the SRT.
--
-- NB IdLabel is still appropriate for ticky ids (as opposed to
-- CmmLabel) because the LNE's counter is still related to an .hs Id,
-- that Id just isn't for a proper closure.

-- -----------------------------------------------------------------------------
-- Does a CLabel need declaring before use or not?
--
-- See wiki:Commentary/Compiler/Backends/PprC#Prototypes

needsCDecl :: CLabel -> Bool
  -- False <=> it's pre-declared; don't bother
  -- don't bother declaring Bitmap labels, we always make sure
  -- they are defined before use.
needsCDecl (SRTLabel _)                 = True
needsCDecl (LargeSRTLabel _)            = False
needsCDecl (LargeBitmapLabel _)         = False
needsCDecl (IdLabel _ _ _)              = True
needsCDecl (CaseLabel _ _)              = True
needsCDecl (PlainModuleInitLabel _)     = True

needsCDecl (StringLitLabel _)           = False
needsCDecl (AsmTempLabel _)             = False
needsCDecl (RtsLabel _)                 = False

needsCDecl (CmmLabel pkgId _ _)
        -- Prototypes for labels defined in the runtime system are imported
        --      into HC files via includes/Stg.h.
        | pkgId == rtsPackageId         = False

        -- For other labels we inline one into the HC file directly.
        | otherwise                     = True

needsCDecl l@(ForeignLabel{})           = not (isMathFun l)
needsCDecl (CC_Label _)                 = True
needsCDecl (CCS_Label _)                = True
needsCDecl (HpcTicksLabel _)            = True
needsCDecl (DynamicLinkerLabel {})      = panic "needsCDecl DynamicLinkerLabel"
needsCDecl PicBaseLabel                 = panic "needsCDecl PicBaseLabel"
needsCDecl (DeadStripPreventer {})      = panic "needsCDecl DeadStripPreventer"

-- | Check whether a label is a local temporary for native code generation
isAsmTemp  :: CLabel -> Bool
isAsmTemp (AsmTempLabel _)              = True
isAsmTemp _                             = False


-- | If a label is a local temporary used for native code generation
--      then return just its unique, otherwise nothing.
maybeAsmTemp :: CLabel -> Maybe Unique
maybeAsmTemp (AsmTempLabel uq)          = Just uq
maybeAsmTemp _                          = Nothing


-- | Check whether a label corresponds to a C function that has
--      a prototype in a system header somehere, or is built-in
--      to the C compiler. For these labels we avoid generating our
--      own C prototypes.
isMathFun :: CLabel -> Bool
isMathFun (ForeignLabel fs _ _ _)       = fs `elementOfUniqSet` math_funs
isMathFun _ = False

math_funs :: UniqSet FastString
math_funs = mkUniqSet [
        -- _ISOC99_SOURCE
        (fsLit "acos"),         (fsLit "acosf"),        (fsLit "acosh"),
        (fsLit "acoshf"),       (fsLit "acoshl"),       (fsLit "acosl"),
        (fsLit "asin"),         (fsLit "asinf"),        (fsLit "asinl"),
        (fsLit "asinh"),        (fsLit "asinhf"),       (fsLit "asinhl"),
        (fsLit "atan"),         (fsLit "atanf"),        (fsLit "atanl"),
        (fsLit "atan2"),        (fsLit "atan2f"),       (fsLit "atan2l"),
        (fsLit "atanh"),        (fsLit "atanhf"),       (fsLit "atanhl"),
        (fsLit "cbrt"),         (fsLit "cbrtf"),        (fsLit "cbrtl"),
        (fsLit "ceil"),         (fsLit "ceilf"),        (fsLit "ceill"),
        (fsLit "copysign"),     (fsLit "copysignf"),    (fsLit "copysignl"),
        (fsLit "cos"),          (fsLit "cosf"),         (fsLit "cosl"),
        (fsLit "cosh"),         (fsLit "coshf"),        (fsLit "coshl"),
        (fsLit "erf"),          (fsLit "erff"),         (fsLit "erfl"),
        (fsLit "erfc"),         (fsLit "erfcf"),        (fsLit "erfcl"),
        (fsLit "exp"),          (fsLit "expf"),         (fsLit "expl"),
        (fsLit "exp2"),         (fsLit "exp2f"),        (fsLit "exp2l"),
        (fsLit "expm1"),        (fsLit "expm1f"),       (fsLit "expm1l"),
        (fsLit "fabs"),         (fsLit "fabsf"),        (fsLit "fabsl"),
        (fsLit "fdim"),         (fsLit "fdimf"),        (fsLit "fdiml"),
        (fsLit "floor"),        (fsLit "floorf"),       (fsLit "floorl"),
        (fsLit "fma"),          (fsLit "fmaf"),         (fsLit "fmal"),
        (fsLit "fmax"),         (fsLit "fmaxf"),        (fsLit "fmaxl"),
        (fsLit "fmin"),         (fsLit "fminf"),        (fsLit "fminl"),
        (fsLit "fmod"),         (fsLit "fmodf"),        (fsLit "fmodl"),
        (fsLit "frexp"),        (fsLit "frexpf"),       (fsLit "frexpl"),
        (fsLit "hypot"),        (fsLit "hypotf"),       (fsLit "hypotl"),
        (fsLit "ilogb"),        (fsLit "ilogbf"),       (fsLit "ilogbl"),
        (fsLit "ldexp"),        (fsLit "ldexpf"),       (fsLit "ldexpl"),
        (fsLit "lgamma"),       (fsLit "lgammaf"),      (fsLit "lgammal"),
        (fsLit "llrint"),       (fsLit "llrintf"),      (fsLit "llrintl"),
        (fsLit "llround"),      (fsLit "llroundf"),     (fsLit "llroundl"),
        (fsLit "log"),          (fsLit "logf"),         (fsLit "logl"),
        (fsLit "log10l"),       (fsLit "log10"),        (fsLit "log10f"),
        (fsLit "log1pl"),       (fsLit "log1p"),        (fsLit "log1pf"),
        (fsLit "log2"),         (fsLit "log2f"),        (fsLit "log2l"),
        (fsLit "logb"),         (fsLit "logbf"),        (fsLit "logbl"),
        (fsLit "lrint"),        (fsLit "lrintf"),       (fsLit "lrintl"),
        (fsLit "lround"),       (fsLit "lroundf"),      (fsLit "lroundl"),
        (fsLit "modf"),         (fsLit "modff"),        (fsLit "modfl"),
        (fsLit "nan"),          (fsLit "nanf"),         (fsLit "nanl"),
        (fsLit "nearbyint"),    (fsLit "nearbyintf"),   (fsLit "nearbyintl"),
        (fsLit "nextafter"),    (fsLit "nextafterf"),   (fsLit "nextafterl"),
        (fsLit "nexttoward"),   (fsLit "nexttowardf"),  (fsLit "nexttowardl"),
        (fsLit "pow"),          (fsLit "powf"),         (fsLit "powl"),
        (fsLit "remainder"),    (fsLit "remainderf"),   (fsLit "remainderl"),
        (fsLit "remquo"),       (fsLit "remquof"),      (fsLit "remquol"),
        (fsLit "rint"),         (fsLit "rintf"),        (fsLit "rintl"),
        (fsLit "round"),        (fsLit "roundf"),       (fsLit "roundl"),
        (fsLit "scalbln"),      (fsLit "scalblnf"),     (fsLit "scalblnl"),
        (fsLit "scalbn"),       (fsLit "scalbnf"),      (fsLit "scalbnl"),
        (fsLit "sin"),          (fsLit "sinf"),         (fsLit "sinl"),
        (fsLit "sinh"),         (fsLit "sinhf"),        (fsLit "sinhl"),
        (fsLit "sqrt"),         (fsLit "sqrtf"),        (fsLit "sqrtl"),
        (fsLit "tan"),          (fsLit "tanf"),         (fsLit "tanl"),
        (fsLit "tanh"),         (fsLit "tanhf"),        (fsLit "tanhl"),
        (fsLit "tgamma"),       (fsLit "tgammaf"),      (fsLit "tgammal"),
        (fsLit "trunc"),        (fsLit "truncf"),       (fsLit "truncl"),
        -- ISO C 99 also defines these function-like macros in math.h:
        -- fpclassify, isfinite, isinf, isnormal, signbit, isgreater,
        -- isgreaterequal, isless, islessequal, islessgreater, isunordered

        -- additional symbols from _BSD_SOURCE
        (fsLit "drem"),         (fsLit "dremf"),        (fsLit "dreml"),
        (fsLit "finite"),       (fsLit "finitef"),      (fsLit "finitel"),
        (fsLit "gamma"),        (fsLit "gammaf"),       (fsLit "gammal"),
        (fsLit "isinf"),        (fsLit "isinff"),       (fsLit "isinfl"),
        (fsLit "isnan"),        (fsLit "isnanf"),       (fsLit "isnanl"),
        (fsLit "j0"),           (fsLit "j0f"),          (fsLit "j0l"),
        (fsLit "j1"),           (fsLit "j1f"),          (fsLit "j1l"),
        (fsLit "jn"),           (fsLit "jnf"),          (fsLit "jnl"),
        (fsLit "lgamma_r"),     (fsLit "lgammaf_r"),    (fsLit "lgammal_r"),
        (fsLit "scalb"),        (fsLit "scalbf"),       (fsLit "scalbl"),
        (fsLit "significand"),  (fsLit "significandf"), (fsLit "significandl"),
        (fsLit "y0"),           (fsLit "y0f"),          (fsLit "y0l"),
        (fsLit "y1"),           (fsLit "y1f"),          (fsLit "y1l"),
        (fsLit "yn"),           (fsLit "ynf"),          (fsLit "ynl")
    ]

-- -----------------------------------------------------------------------------
-- | Is a CLabel visible outside this object file or not?
--      From the point of view of the code generator, a name is
--      externally visible if it has to be declared as exported
--      in the .o file's symbol table; that is, made non-static.
externallyVisibleCLabel :: CLabel -> Bool -- not C "static"
externallyVisibleCLabel (CaseLabel _ _)         = False
externallyVisibleCLabel (StringLitLabel _)      = False
externallyVisibleCLabel (AsmTempLabel _)        = False
externallyVisibleCLabel (PlainModuleInitLabel _)= True
externallyVisibleCLabel (RtsLabel _)            = True
externallyVisibleCLabel (CmmLabel _ _ _)        = True
externallyVisibleCLabel (ForeignLabel{})        = True
externallyVisibleCLabel (IdLabel name _ info)   = isExternalName name && externallyVisibleIdLabel info
externallyVisibleCLabel (CC_Label _)            = True
externallyVisibleCLabel (CCS_Label _)           = True
externallyVisibleCLabel (DynamicLinkerLabel _ _)  = False
externallyVisibleCLabel (HpcTicksLabel _)       = True
externallyVisibleCLabel (LargeBitmapLabel _)    = False
externallyVisibleCLabel (SRTLabel _)            = False
externallyVisibleCLabel (LargeSRTLabel _)       = False
externallyVisibleCLabel (PicBaseLabel {}) = panic "externallyVisibleCLabel PicBaseLabel"
externallyVisibleCLabel (DeadStripPreventer {}) = panic "externallyVisibleCLabel DeadStripPreventer"

externallyVisibleIdLabel :: IdLabelInfo -> Bool
externallyVisibleIdLabel SRT             = False
externallyVisibleIdLabel LocalInfoTable  = False
externallyVisibleIdLabel LocalEntry      = False
externallyVisibleIdLabel _               = True

-- -----------------------------------------------------------------------------
-- Finding the "type" of a CLabel

-- For generating correct types in label declarations:

data CLabelType
  = CodeLabel   -- Address of some executable instructions
  | DataLabel   -- Address of data, not a GC ptr
  | GcPtrLabel  -- Address of a (presumably static) GC object

isCFunctionLabel :: CLabel -> Bool
isCFunctionLabel lbl = case labelType lbl of
                        CodeLabel -> True
                        _other    -> False

isGcPtrLabel :: CLabel -> Bool
isGcPtrLabel lbl = case labelType lbl of
                        GcPtrLabel -> True
                        _other     -> False


-- | Work out the general type of data at the address of this label
--    whether it be code, data, or static GC object.
labelType :: CLabel -> CLabelType
labelType (CmmLabel _ _ CmmData)                = DataLabel
labelType (CmmLabel _ _ CmmClosure)             = GcPtrLabel
labelType (CmmLabel _ _ CmmCode)                = CodeLabel
labelType (CmmLabel _ _ CmmInfo)                = DataLabel
labelType (CmmLabel _ _ CmmEntry)               = CodeLabel
labelType (CmmLabel _ _ CmmRetInfo)             = DataLabel
labelType (CmmLabel _ _ CmmRet)                 = CodeLabel
labelType (RtsLabel (RtsSelectorInfoTable _ _)) = DataLabel
labelType (RtsLabel (RtsApInfoTable _ _))       = DataLabel
labelType (RtsLabel (RtsApFast _))              = CodeLabel
labelType (CaseLabel _ CaseReturnInfo)          = DataLabel
labelType (CaseLabel _ _)                       = CodeLabel
labelType (PlainModuleInitLabel _)              = CodeLabel
labelType (SRTLabel _)                          = DataLabel
labelType (LargeSRTLabel _)                     = DataLabel
labelType (LargeBitmapLabel _)                  = DataLabel
labelType (ForeignLabel _ _ _ IsFunction)       = CodeLabel
labelType (IdLabel _ _ info)                    = idInfoLabelType info
labelType _                                     = DataLabel

idInfoLabelType :: IdLabelInfo -> CLabelType
idInfoLabelType info =
  case info of
    InfoTable     -> DataLabel
    LocalInfoTable -> DataLabel
    Closure       -> GcPtrLabel
    ConInfoTable  -> DataLabel
    StaticInfoTable -> DataLabel
    ClosureTable  -> DataLabel
    RednCounts    -> DataLabel
    _             -> CodeLabel


-- -----------------------------------------------------------------------------
-- Does a CLabel need dynamic linkage?

-- When referring to data in code, we need to know whether
-- that data resides in a DLL or not. [Win32 only.]
-- @labelDynamic@ returns @True@ if the label is located
-- in a DLL, be it a data reference or not.

labelDynamic :: DynFlags -> PackageId -> Module -> CLabel -> Bool
labelDynamic dflags this_pkg this_mod lbl =
  case lbl of
   -- is the RTS in a DLL or not?
   RtsLabel _           -> not (gopt Opt_Static dflags) && (this_pkg /= rtsPackageId)

   IdLabel n _ _        -> isDllName dflags this_pkg this_mod n

   -- When compiling in the "dyn" way, each package is to be linked into
   -- its own shared library.
   CmmLabel pkg _ _
    | os == OSMinGW32 ->
       not (gopt Opt_Static dflags) && (this_pkg /= pkg)
    | otherwise ->
       True

   ForeignLabel _ _ source _  ->
       if os == OSMinGW32
       then case source of
            -- Foreign label is in some un-named foreign package (or DLL).
            ForeignLabelInExternalPackage -> True

            -- Foreign label is linked into the same package as the
            -- source file currently being compiled.
            ForeignLabelInThisPackage -> False

            -- Foreign label is in some named package.
            -- When compiling in the "dyn" way, each package is to be
            -- linked into its own DLL.
            ForeignLabelInPackage pkgId ->
                (not (gopt Opt_Static dflags)) && (this_pkg /= pkgId)

       else -- On Mac OS X and on ELF platforms, false positives are OK,
            -- so we claim that all foreign imports come from dynamic
            -- libraries
            True

   PlainModuleInitLabel m -> not (gopt Opt_Static dflags) && this_pkg /= (modulePackageId m)

   -- Note that DynamicLinkerLabels do NOT require dynamic linking themselves.
   _                 -> False
  where os = platformOS (targetPlatform dflags)

{-
OLD?: These GRAN functions are needed for spitting out GRAN_FETCH() at the
right places. It is used to detect when the abstractC statement of an
CCodeBlock actually contains the code for a slow entry point.  -- HWL

We need at least @Eq@ for @CLabels@, because we want to avoid
duplicate declarations in generating C (see @labelSeenTE@ in
@PprAbsC@).
-}

-----------------------------------------------------------------------------
-- Printing out CLabels.

{-
Convention:

      <name>_<type>

where <name> is <Module>_<name> for external names and <unique> for
internal names. <type> is one of the following:

         info                   Info table
         srt                    Static reference table
         srtd                   Static reference table descriptor
         entry                  Entry code (function, closure)
         slow                   Slow entry code (if any)
         ret                    Direct return address
         vtbl                   Vector table
         <n>_alt                Case alternative (tag n)
         dflt                   Default case alternative
         btm                    Large bitmap vector
         closure                Static closure
         con_entry              Dynamic Constructor entry code
         con_info               Dynamic Constructor info table
         static_entry           Static Constructor entry code
         static_info            Static Constructor info table
         sel_info               Selector info table
         sel_entry              Selector entry code
         cc                     Cost centre
         ccs                    Cost centre stack

Many of these distinctions are only for documentation reasons.  For
example, _ret is only distinguished from _entry to make it easy to
tell whether a code fragment is a return point or a closure/function
entry.

Note [Closure and info labels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a function 'foo, we have:
   foo_info    : Points to the info table describing foo's closure
                 (and entry code for foo with tables next to code)
   foo_closure : Static (no-free-var) closure only:
                 points to the statically-allocated closure

For a data constructor (such as Just or Nothing), we have:
    Just_con_info: Info table for the data constructor itself
                   the first word of a heap-allocated Just
    Just_info:     Info table for the *worker function*, an
                   ordinary Haskell function of arity 1 that
                   allocates a (Just x) box:
                      Just = \x -> Just x
    Just_closure:  The closure for this worker

    Nothing_closure: a statically allocated closure for Nothing
    Nothing_static_info: info table for Nothing_closure

All these must be exported symbol, EXCEPT Just_info.  We don't need to
export this because in other modules we either have
       * A reference to 'Just'; use Just_closure
       * A saturated call 'Just x'; allocate using Just_con_info
Not exporting these Just_info labels reduces the number of symbols
somewhat.
-}

instance Outputable CLabel where
  ppr c = sdocWithPlatform $ \platform -> pprCLabel platform c

pprCLabel :: Platform -> CLabel -> SDoc

pprCLabel platform (AsmTempLabel u)
 | cGhcWithNativeCodeGen == "YES"
  =  getPprStyle $ \ sty ->
     if asmStyle sty then
        ptext (asmTempLabelPrefix platform) <> pprUnique u
     else
        char '_' <> pprUnique u

pprCLabel platform (DynamicLinkerLabel info lbl)
 | cGhcWithNativeCodeGen == "YES"
   = pprDynamicLinkerAsmLabel platform info lbl

pprCLabel _ PicBaseLabel
 | cGhcWithNativeCodeGen == "YES"
   = ptext (sLit "1b")

pprCLabel platform (DeadStripPreventer lbl)
 | cGhcWithNativeCodeGen == "YES"
   = pprCLabel platform lbl <> ptext (sLit "_dsp")

pprCLabel platform lbl
   = getPprStyle $ \ sty ->
     if cGhcWithNativeCodeGen == "YES" && asmStyle sty
     then maybe_underscore (pprAsmCLbl platform lbl)
     else pprCLbl lbl

maybe_underscore :: SDoc -> SDoc
maybe_underscore doc
  | underscorePrefix = pp_cSEP <> doc
  | otherwise        = doc

pprAsmCLbl :: Platform -> CLabel -> SDoc
pprAsmCLbl platform (ForeignLabel fs (Just sz) _ _)
 | platformOS platform == OSMinGW32
    -- In asm mode, we need to put the suffix on a stdcall ForeignLabel.
    -- (The C compiler does this itself).
    = ftext fs <> char '@' <> int sz
pprAsmCLbl _ lbl
   = pprCLbl lbl

pprCLbl :: CLabel -> SDoc
pprCLbl (StringLitLabel u)
  = pprUnique u <> ptext (sLit "_str")

pprCLbl (CaseLabel u CaseReturnPt)
  = hcat [pprUnique u, ptext (sLit "_ret")]
pprCLbl (CaseLabel u CaseReturnInfo)
  = hcat [pprUnique u, ptext (sLit "_info")]
pprCLbl (CaseLabel u (CaseAlt tag))
  = hcat [pprUnique u, pp_cSEP, int tag, ptext (sLit "_alt")]
pprCLbl (CaseLabel u CaseDefault)
  = hcat [pprUnique u, ptext (sLit "_dflt")]

pprCLbl (SRTLabel u)
  = pprUnique u <> pp_cSEP <> ptext (sLit "srt")

pprCLbl (LargeSRTLabel u)  = pprUnique u <> pp_cSEP <> ptext (sLit "srtd")
pprCLbl (LargeBitmapLabel u)  = text "b" <> pprUnique u <> pp_cSEP <> ptext (sLit "btm")
-- Some bitsmaps for tuple constructors have a numeric tag (e.g. '7')
-- until that gets resolved we'll just force them to start
-- with a letter so the label will be legal assmbly code.


pprCLbl (CmmLabel _ str CmmCode)        = ftext str
pprCLbl (CmmLabel _ str CmmData)        = ftext str
pprCLbl (CmmLabel _ str CmmPrimCall)    = ftext str

pprCLbl (RtsLabel (RtsApFast str))   = ftext str <> ptext (sLit "_fast")

pprCLbl (RtsLabel (RtsSelectorInfoTable upd_reqd offset))
  = hcat [ptext (sLit "stg_sel_"), text (show offset),
                ptext (if upd_reqd
                        then (sLit "_upd_info")
                        else (sLit "_noupd_info"))
        ]

pprCLbl (RtsLabel (RtsSelectorEntry upd_reqd offset))
  = hcat [ptext (sLit "stg_sel_"), text (show offset),
                ptext (if upd_reqd
                        then (sLit "_upd_entry")
                        else (sLit "_noupd_entry"))
        ]

pprCLbl (RtsLabel (RtsApInfoTable upd_reqd arity))
  = hcat [ptext (sLit "stg_ap_"), text (show arity),
                ptext (if upd_reqd
                        then (sLit "_upd_info")
                        else (sLit "_noupd_info"))
        ]

pprCLbl (RtsLabel (RtsApEntry upd_reqd arity))
  = hcat [ptext (sLit "stg_ap_"), text (show arity),
                ptext (if upd_reqd
                        then (sLit "_upd_entry")
                        else (sLit "_noupd_entry"))
        ]

pprCLbl (CmmLabel _ fs CmmInfo)
  = ftext fs <> ptext (sLit "_info")

pprCLbl (CmmLabel _ fs CmmEntry)
  = ftext fs <> ptext (sLit "_entry")

pprCLbl (CmmLabel _ fs CmmRetInfo)
  = ftext fs <> ptext (sLit "_info")

pprCLbl (CmmLabel _ fs CmmRet)
  = ftext fs <> ptext (sLit "_ret")

pprCLbl (CmmLabel _ fs CmmClosure)
  = ftext fs <> ptext (sLit "_closure")

pprCLbl (RtsLabel (RtsPrimOp primop))
  = ptext (sLit "stg_") <> ppr primop

pprCLbl (RtsLabel (RtsSlowFastTickyCtr pat))
  = ptext (sLit "SLOW_CALL_fast_") <> text pat <> ptext (sLit "_ctr")

pprCLbl (ForeignLabel str _ _ _)
  = ftext str

pprCLbl (IdLabel name _cafs flavor) = ppr name <> ppIdFlavor flavor

pprCLbl (CC_Label cc)           = ppr cc
pprCLbl (CCS_Label ccs)         = ppr ccs

pprCLbl (PlainModuleInitLabel mod)
   = ptext (sLit "__stginit_") <> ppr mod

pprCLbl (HpcTicksLabel mod)
  = ptext (sLit "_hpc_tickboxes_")  <> ppr mod <> ptext (sLit "_hpc")

pprCLbl (AsmTempLabel {})       = panic "pprCLbl AsmTempLabel"
pprCLbl (DynamicLinkerLabel {}) = panic "pprCLbl DynamicLinkerLabel"
pprCLbl (PicBaseLabel {})       = panic "pprCLbl PicBaseLabel"
pprCLbl (DeadStripPreventer {}) = panic "pprCLbl DeadStripPreventer"

ppIdFlavor :: IdLabelInfo -> SDoc
ppIdFlavor x = pp_cSEP <>
               (case x of
                       Closure          -> ptext (sLit "closure")
                       SRT              -> ptext (sLit "srt")
                       InfoTable        -> ptext (sLit "info")
                       LocalInfoTable   -> ptext (sLit "info")
                       Entry            -> ptext (sLit "entry")
                       LocalEntry       -> ptext (sLit "entry")
                       Slow             -> ptext (sLit "slow")
                       RednCounts       -> ptext (sLit "ct")
                       ConEntry         -> ptext (sLit "con_entry")
                       ConInfoTable     -> ptext (sLit "con_info")
                       StaticConEntry   -> ptext (sLit "static_entry")
                       StaticInfoTable  -> ptext (sLit "static_info")
                       ClosureTable     -> ptext (sLit "closure_tbl")
                      )


pp_cSEP :: SDoc
pp_cSEP = char '_'


instance Outputable ForeignLabelSource where
 ppr fs
  = case fs of
        ForeignLabelInPackage pkgId     -> parens $ text "package: " <> ppr pkgId
        ForeignLabelInThisPackage       -> parens $ text "this package"
        ForeignLabelInExternalPackage   -> parens $ text "external package"

-- -----------------------------------------------------------------------------
-- Machine-dependent knowledge about labels.

underscorePrefix :: Bool   -- leading underscore on assembler labels?
underscorePrefix = (cLeadingUnderscore == "YES")

asmTempLabelPrefix :: Platform -> LitString  -- for formatting labels
asmTempLabelPrefix platform =
    if platformOS platform == OSDarwin
    then sLit "L"
    else sLit ".L"

pprDynamicLinkerAsmLabel :: Platform -> DynamicLinkerLabelInfo -> CLabel -> SDoc
pprDynamicLinkerAsmLabel platform dllInfo lbl
 = if platformOS platform == OSDarwin
   then if platformArch platform == ArchX86_64
        then case dllInfo of
             CodeStub        -> char 'L' <> ppr lbl <> text "$stub"
             SymbolPtr       -> char 'L' <> ppr lbl <> text "$non_lazy_ptr"
             GotSymbolPtr    -> ppr lbl <> text "@GOTPCREL"
             GotSymbolOffset -> ppr lbl
        else case dllInfo of
             CodeStub  -> char 'L' <> ppr lbl <> text "$stub"
             SymbolPtr -> char 'L' <> ppr lbl <> text "$non_lazy_ptr"
             _         -> panic "pprDynamicLinkerAsmLabel"

   else if osElfTarget (platformOS platform)
        then if platformArch platform == ArchPPC
             then case dllInfo of
                  CodeStub  -> ppr lbl <> text "@plt"
                  SymbolPtr -> text ".LC_" <> ppr lbl
                  _         -> panic "pprDynamicLinkerAsmLabel"
             else if platformArch platform == ArchX86_64
                  then case dllInfo of
                       CodeStub        -> ppr lbl <> text "@plt"
                       GotSymbolPtr    -> ppr lbl <> text "@gotpcrel"
                       GotSymbolOffset -> ppr lbl
                       SymbolPtr       -> text ".LC_" <> ppr lbl
        else case dllInfo of
             CodeStub        -> ppr lbl <> text "@plt"
             SymbolPtr       -> text ".LC_" <> ppr lbl
             GotSymbolPtr    -> ppr lbl <> text "@got"
             GotSymbolOffset -> ppr lbl <> text "@gotoff"
   else if platformOS platform == OSMinGW32
        then case dllInfo of
             SymbolPtr -> text "__imp_" <> ppr lbl
             _         -> panic "pprDynamicLinkerAsmLabel"
   else panic "pprDynamicLinkerAsmLabel"

