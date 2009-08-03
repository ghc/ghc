{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- Object-file symbols (called CLabel for histerical raisins).
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CLabel (
	CLabel,	-- abstract type

	mkClosureLabel,
	mkSRTLabel,
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

	mkModuleInitLabel,
	mkPlainModuleInitLabel,
	mkModuleInitTableLabel,

	mkSplitMarkerLabel,
	mkDirty_MUT_VAR_Label,
	mkUpdInfoLabel,
	mkIndStaticInfoLabel,
        mkMainCapabilityLabel,
	mkMAP_FROZEN_infoLabel,
	mkMAP_DIRTY_infoLabel,
        mkEMPTY_MVAR_infoLabel,

	mkTopTickyCtrLabel,
        mkCAFBlackHoleInfoTableLabel,
	mkRtsPrimOpLabel,
	mkRtsSlowTickyCtrLabel,

	moduleRegdLabel,
	moduleRegTableLabel,

	mkSelectorInfoLabel,
	mkSelectorEntryLabel,

	mkRtsInfoLabel,
	mkRtsEntryLabel,
	mkRtsRetInfoLabel,
	mkRtsRetLabel,
	mkRtsCodeLabel,
	mkRtsDataLabel,
	mkRtsGcPtrLabel,

	mkRtsInfoLabelFS,
	mkRtsEntryLabelFS,
	mkRtsRetInfoLabelFS,
	mkRtsRetLabelFS,
	mkRtsCodeLabelFS,
	mkRtsDataLabelFS,

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
        mkHpcModuleNameLabel,

        hasCAF,
	infoLblToEntryLbl, entryLblToInfoLbl, cvtToClosureLbl, cvtToSRTLbl,
	needsCDecl, isAsmTemp, maybeAsmTemp, externallyVisibleCLabel,
        isMathFun,
 	isCFunctionLabel, isGcPtrLabel, labelDynamic,

	pprCLabel
    ) where

#include "HsVersions.h"

import IdInfo
import StaticFlags
import BasicTypes
import Literal
import Packages
import DataCon
import PackageConfig
import Module
import Name
import Unique
import PrimOp
import Config
import CostCentre
import Outputable
import FastString
import DynFlags
import UniqSet

-- -----------------------------------------------------------------------------
-- The CLabel type

{-
CLabel is an abstract type that supports the following operations:

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
  = IdLabel	    		-- A family of labels related to the
	Name			-- definition of a particular Id or Con
        CafInfo
	IdLabelInfo

  | CaseLabel			-- A family of labels related to a particular
				-- case expression.
	{-# UNPACK #-} !Unique	-- Unique says which case expression
	CaseLabelInfo

  | AsmTempLabel 
	{-# UNPACK #-} !Unique

  | StringLitLabel
	{-# UNPACK #-} !Unique

  | ModuleInitLabel 
	Module			-- the module name
	String			-- its "way"
	-- at some point we might want some kind of version number in
	-- the module init label, to guard against compiling modules in
	-- the wrong order.  We can't use the interface file version however,
	-- because we don't always recompile modules which depend on a module
	-- whose version has changed.

  | PlainModuleInitLabel	-- without the version & way info
	Module

  | ModuleInitTableLabel	-- table of imported modules to init
	Module

  | ModuleRegdLabel

  | RtsLabel RtsLabelInfo

  | ForeignLabel FastString     -- a 'C' (or otherwise foreign) label
        (Maybe Int)             -- possible '@n' suffix for stdcall functions
                -- When generating C, the '@n' suffix is omitted, but when
                -- generating assembler we must add it to the label.
        Bool                    -- True <=> is dynamic
        FunctionOrData

  | CC_Label  CostCentre
  | CCS_Label CostCentreStack

      -- Dynamic Linking in the NCG:
      -- generated and used inside the NCG only,
      -- see module PositionIndependentCode for details.
      
  | DynamicLinkerLabel DynamicLinkerLabelInfo CLabel
        -- special variants of a label used for dynamic linking

  | PicBaseLabel                -- a label used as a base for PIC calculations
                                -- on some platforms.
                                -- It takes the form of a local numeric
                                -- assembler label '1'; it is pretty-printed
                                -- as 1b, referring to the previous definition
                                -- of 1: in the assembler source file.

  | DeadStripPreventer CLabel
    -- label before an info table to prevent excessive dead-stripping on darwin

  | HpcTicksLabel Module       -- Per-module table of tick locations
  | HpcModuleNameLabel         -- Per-module name of the module for Hpc

  | LargeSRTLabel           -- Label of an StgLargeSRT
        {-# UNPACK #-} !Unique

  | LargeBitmapLabel        -- A bitmap (function or case return)
        {-# UNPACK #-} !Unique

  deriving (Eq, Ord)

data IdLabelInfo
  = Closure		-- Label for closure
  | SRT                 -- Static reference table
  | InfoTable		-- Info tables for closures; always read-only
  | Entry		-- entry point
  | Slow		-- slow entry point

  | RednCounts		-- Label of place to keep Ticky-ticky  info for 
			-- this Id

  | ConEntry	  	-- constructor entry point
  | ConInfoTable 		-- corresponding info table
  | StaticConEntry  	-- static constructor entry point
  | StaticInfoTable   	-- corresponding info table

  | ClosureTable	-- table of closures for Enum tycons

  deriving (Eq, Ord)


data CaseLabelInfo
  = CaseReturnPt
  | CaseReturnInfo
  | CaseAlt ConTag
  | CaseDefault
  deriving (Eq, Ord)


data RtsLabelInfo
  = RtsSelectorInfoTable Bool{-updatable-} Int{-offset-}	-- Selector thunks
  | RtsSelectorEntry   Bool{-updatable-} Int{-offset-}

  | RtsApInfoTable Bool{-updatable-} Int{-arity-}	        -- AP thunks
  | RtsApEntry   Bool{-updatable-} Int{-arity-}

  | RtsPrimOp PrimOp

  | RtsInfo       LitString	-- misc rts info tables
  | RtsEntry      LitString	-- misc rts entry points
  | RtsRetInfo    LitString	-- misc rts ret info tables
  | RtsRet        LitString	-- misc rts return points
  | RtsData       LitString	-- misc rts data bits
  | RtsGcPtr      LitString	-- GcPtrs eg CHARLIKE_closure
  | RtsCode       LitString	-- misc rts code

  | RtsInfoFS     FastString	-- misc rts info tables
  | RtsEntryFS    FastString	-- misc rts entry points
  | RtsRetInfoFS  FastString	-- misc rts ret info tables
  | RtsRetFS      FastString	-- misc rts return points
  | RtsDataFS     FastString	-- misc rts data bits, eg CHARLIKE_closure
  | RtsCodeFS     FastString	-- misc rts code

  | RtsApFast	LitString	-- _fast versions of generic apply

  | RtsSlowTickyCtr String

  deriving (Eq, Ord)
	-- NOTE: Eq on LitString compares the pointer only, so this isn't
	-- a real equality.

data DynamicLinkerLabelInfo
  = CodeStub            -- MachO: Lfoo$stub, ELF: foo@plt
  | SymbolPtr           -- MachO: Lfoo$non_lazy_ptr, Windows: __imp_foo
  | GotSymbolPtr        -- ELF: foo@got
  | GotSymbolOffset     -- ELF: foo@gotoff
  
  deriving (Eq, Ord)
  
-- -----------------------------------------------------------------------------
-- Constructing CLabels

-- These are always local:
mkSRTLabel		name c 	= IdLabel name  c SRT
mkSlowEntryLabel      	name c 	= IdLabel name  c Slow
mkRednCountsLabel     	name c 	= IdLabel name  c RednCounts

-- These have local & (possibly) external variants:
mkLocalClosureLabel	name c 	= IdLabel name  c Closure
mkLocalInfoTableLabel  	name c 	= IdLabel name  c InfoTable
mkLocalEntryLabel	name c 	= IdLabel name  c Entry
mkLocalClosureTableLabel name c = IdLabel name  c ClosureTable

mkClosureLabel name         c     = IdLabel name c Closure
mkInfoTableLabel name       c     = IdLabel name c InfoTable
mkEntryLabel name           c     = IdLabel name c Entry
mkClosureTableLabel name    c     = IdLabel name c ClosureTable
mkLocalConInfoTableLabel    c con = IdLabel con c ConInfoTable
mkLocalConEntryLabel	    c con = IdLabel con c ConEntry
mkLocalStaticInfoTableLabel c con = IdLabel con c StaticInfoTable
mkLocalStaticConEntryLabel  c con = IdLabel con c StaticConEntry
mkConInfoTableLabel name    c     = IdLabel    name c ConInfoTable
mkStaticInfoTableLabel name c     = IdLabel    name c StaticInfoTable

mkConEntryLabel name        c     = IdLabel name c ConEntry
mkStaticConEntryLabel name  c     = IdLabel name c StaticConEntry

mkLargeSRTLabel	uniq 	= LargeSRTLabel uniq
mkBitmapLabel	uniq 	= LargeBitmapLabel uniq

mkReturnPtLabel uniq		= CaseLabel uniq CaseReturnPt
mkReturnInfoLabel uniq		= CaseLabel uniq CaseReturnInfo
mkAltLabel      uniq tag	= CaseLabel uniq (CaseAlt tag)
mkDefaultLabel  uniq 		= CaseLabel uniq CaseDefault

mkStringLitLabel		= StringLitLabel
mkAsmTempLabel :: Uniquable a => a -> CLabel
mkAsmTempLabel a		= AsmTempLabel (getUnique a)

mkModuleInitLabel :: Module -> String -> CLabel
mkModuleInitLabel mod way        = ModuleInitLabel mod way

mkPlainModuleInitLabel :: Module -> CLabel
mkPlainModuleInitLabel mod       = PlainModuleInitLabel mod

mkModuleInitTableLabel :: Module -> CLabel
mkModuleInitTableLabel mod       = ModuleInitTableLabel mod

	-- Some fixed runtime system labels

mkSplitMarkerLabel		= RtsLabel (RtsCode (sLit "__stg_split_marker"))
mkDirty_MUT_VAR_Label		= RtsLabel (RtsCode (sLit "dirty_MUT_VAR"))
mkUpdInfoLabel			= RtsLabel (RtsInfo (sLit "stg_upd_frame"))
mkIndStaticInfoLabel		= RtsLabel (RtsInfo (sLit "stg_IND_STATIC"))
mkMainCapabilityLabel		= RtsLabel (RtsData (sLit "MainCapability"))
mkMAP_FROZEN_infoLabel		= RtsLabel (RtsInfo (sLit "stg_MUT_ARR_PTRS_FROZEN0"))
mkMAP_DIRTY_infoLabel		= RtsLabel (RtsInfo (sLit "stg_MUT_ARR_PTRS_DIRTY"))
mkEMPTY_MVAR_infoLabel		= RtsLabel (RtsInfo (sLit "stg_EMPTY_MVAR"))

mkTopTickyCtrLabel		= RtsLabel (RtsData (sLit "top_ct"))
mkCAFBlackHoleInfoTableLabel	= RtsLabel (RtsInfo (sLit "stg_CAF_BLACKHOLE"))
mkRtsPrimOpLabel primop		= RtsLabel (RtsPrimOp primop)

moduleRegdLabel			= ModuleRegdLabel
moduleRegTableLabel             = ModuleInitTableLabel	

mkSelectorInfoLabel  upd off	= RtsLabel (RtsSelectorInfoTable upd off)
mkSelectorEntryLabel upd off	= RtsLabel (RtsSelectorEntry   upd off)

mkApInfoTableLabel  upd off	= RtsLabel (RtsApInfoTable upd off)
mkApEntryLabel upd off		= RtsLabel (RtsApEntry   upd off)

        -- Primitive / cmm call labels

mkPrimCallLabel :: PrimCall -> CLabel
mkPrimCallLabel (PrimCall str)  = ForeignLabel str Nothing False IsFunction

	-- Foreign labels

mkForeignLabel :: FastString -> Maybe Int -> Bool -> FunctionOrData -> CLabel
mkForeignLabel str mb_sz is_dynamic fod
    = ForeignLabel str mb_sz is_dynamic fod

addLabelSize :: CLabel -> Int -> CLabel
addLabelSize (ForeignLabel str _ is_dynamic fod) sz
  = ForeignLabel str (Just sz) is_dynamic fod
addLabelSize label _
  = label

foreignLabelStdcallInfo :: CLabel -> Maybe Int
foreignLabelStdcallInfo (ForeignLabel _ info _ _) = info
foreignLabelStdcallInfo _lbl = Nothing

	-- Cost centres etc.

mkCCLabel	cc		= CC_Label cc
mkCCSLabel	ccs		= CCS_Label ccs

mkRtsInfoLabel      str = RtsLabel (RtsInfo      str)
mkRtsEntryLabel     str = RtsLabel (RtsEntry     str)
mkRtsRetInfoLabel   str = RtsLabel (RtsRetInfo   str)
mkRtsRetLabel       str = RtsLabel (RtsRet       str)
mkRtsCodeLabel      str = RtsLabel (RtsCode      str)
mkRtsDataLabel      str = RtsLabel (RtsData      str)
mkRtsGcPtrLabel     str = RtsLabel (RtsGcPtr     str)

mkRtsInfoLabelFS    str = RtsLabel (RtsInfoFS    str)
mkRtsEntryLabelFS   str = RtsLabel (RtsEntryFS   str)
mkRtsRetInfoLabelFS str = RtsLabel (RtsRetInfoFS str)
mkRtsRetLabelFS     str = RtsLabel (RtsRetFS     str)
mkRtsCodeLabelFS    str = RtsLabel (RtsCodeFS    str)
mkRtsDataLabelFS    str = RtsLabel (RtsDataFS    str)

mkRtsApFastLabel str = RtsLabel (RtsApFast str)

mkRtsSlowTickyCtrLabel :: String -> CLabel
mkRtsSlowTickyCtrLabel pat = RtsLabel (RtsSlowTickyCtr pat)

        -- Coverage

mkHpcTicksLabel                = HpcTicksLabel
mkHpcModuleNameLabel           = HpcModuleNameLabel

        -- Dynamic linking
        
mkDynamicLinkerLabel :: DynamicLinkerLabelInfo -> CLabel -> CLabel
mkDynamicLinkerLabel = DynamicLinkerLabel

dynamicLinkerLabelInfo :: CLabel -> Maybe (DynamicLinkerLabelInfo, CLabel)
dynamicLinkerLabelInfo (DynamicLinkerLabel info lbl) = Just (info, lbl)
dynamicLinkerLabelInfo _ = Nothing

        -- Position independent code
        
mkPicBaseLabel :: CLabel
mkPicBaseLabel = PicBaseLabel

mkDeadStripPreventer :: CLabel -> CLabel
mkDeadStripPreventer lbl = DeadStripPreventer lbl

-- -----------------------------------------------------------------------------
-- Converting between info labels and entry/ret labels.

infoLblToEntryLbl :: CLabel -> CLabel 
infoLblToEntryLbl (IdLabel n c InfoTable) = IdLabel n c Entry
infoLblToEntryLbl (IdLabel n c ConInfoTable) = IdLabel n c ConEntry
infoLblToEntryLbl (IdLabel n c StaticInfoTable) = IdLabel n c StaticConEntry
infoLblToEntryLbl (CaseLabel n CaseReturnInfo) = CaseLabel n CaseReturnPt
infoLblToEntryLbl (RtsLabel (RtsInfo s)) = RtsLabel (RtsEntry s)
infoLblToEntryLbl (RtsLabel (RtsRetInfo s)) = RtsLabel (RtsRet s)
infoLblToEntryLbl (RtsLabel (RtsInfoFS s)) = RtsLabel (RtsEntryFS s)
infoLblToEntryLbl (RtsLabel (RtsRetInfoFS s)) = RtsLabel (RtsRetFS s)
infoLblToEntryLbl _ = panic "CLabel.infoLblToEntryLbl"

entryLblToInfoLbl :: CLabel -> CLabel 
entryLblToInfoLbl (IdLabel n c Entry) = IdLabel n c InfoTable
entryLblToInfoLbl (IdLabel n c ConEntry) = IdLabel n c ConInfoTable
entryLblToInfoLbl (IdLabel n c StaticConEntry) = IdLabel n c StaticInfoTable
entryLblToInfoLbl (CaseLabel n CaseReturnPt) = CaseLabel n CaseReturnInfo
entryLblToInfoLbl (RtsLabel (RtsEntry s)) = RtsLabel (RtsInfo s)
entryLblToInfoLbl (RtsLabel (RtsRet s)) = RtsLabel (RtsRetInfo s)
entryLblToInfoLbl (RtsLabel (RtsEntryFS s)) = RtsLabel (RtsInfoFS s)
entryLblToInfoLbl (RtsLabel (RtsRetFS s)) = RtsLabel (RtsRetInfoFS s)
entryLblToInfoLbl l = pprPanic "CLabel.entryLblToInfoLbl" (pprCLabel l)

cvtToClosureLbl   (IdLabel n c InfoTable) = IdLabel n c Closure
cvtToClosureLbl   (IdLabel n c Entry)     = IdLabel n c Closure
cvtToClosureLbl   (IdLabel n c ConEntry)  = IdLabel n c Closure
cvtToClosureLbl l@(IdLabel n c Closure)   = l
cvtToClosureLbl l = pprPanic "cvtToClosureLbl" (pprCLabel l)

cvtToSRTLbl   (IdLabel n c InfoTable) = mkSRTLabel n c
cvtToSRTLbl   (IdLabel n c Entry)     = mkSRTLabel n c
cvtToSRTLbl   (IdLabel n c ConEntry)  = mkSRTLabel n c
cvtToSRTLbl l@(IdLabel n c Closure)   = mkSRTLabel n c
cvtToSRTLbl l = pprPanic "cvtToSRTLbl" (pprCLabel l)

-- -----------------------------------------------------------------------------
-- Does a CLabel refer to a CAF?
hasCAF :: CLabel -> Bool
hasCAF (IdLabel _ MayHaveCafRefs _) = True
hasCAF _                            = False

-- -----------------------------------------------------------------------------
-- Does a CLabel need declaring before use or not?
--
-- See wiki:Commentary/Compiler/Backends/PprC#Prototypes

needsCDecl :: CLabel -> Bool
  -- False <=> it's pre-declared; don't bother
  -- don't bother declaring SRT & Bitmap labels, we always make sure
  -- they are defined before use.
needsCDecl (IdLabel _ _ SRT)		= False
needsCDecl (LargeSRTLabel _)		= False
needsCDecl (LargeBitmapLabel _)		= False
needsCDecl (IdLabel _ _ _)		= True
needsCDecl (CaseLabel _ _)	        = True
needsCDecl (ModuleInitLabel _ _)	= True
needsCDecl (PlainModuleInitLabel _)	= True
needsCDecl (ModuleInitTableLabel _)	= True
needsCDecl ModuleRegdLabel		= False

needsCDecl (StringLitLabel _)		= False
needsCDecl (AsmTempLabel _)		= False
needsCDecl (RtsLabel _)			= False
needsCDecl l@(ForeignLabel _ _ _ _)	= not (isMathFun l)
needsCDecl (CC_Label _)			= True
needsCDecl (CCS_Label _)		= True
needsCDecl (HpcTicksLabel _)            = True
needsCDecl HpcModuleNameLabel           = False

-- Whether the label is an assembler temporary:

isAsmTemp  :: CLabel -> Bool    -- is a local temporary for native code generation
isAsmTemp (AsmTempLabel _) = True
isAsmTemp _ 	    	   = False

maybeAsmTemp :: CLabel -> Maybe Unique
maybeAsmTemp (AsmTempLabel uq) = Just uq
maybeAsmTemp _ 	    	       = Nothing

-- some labels have C prototypes in scope when compiling via C, because
-- they are builtin to the C compiler.  For these labels we avoid
-- generating our own C prototypes.
isMathFun :: CLabel -> Bool
isMathFun (ForeignLabel fs _ _ _) = fs `elementOfUniqSet` math_funs
isMathFun _ = False

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
-- Is a CLabel visible outside this object file or not?

-- From the point of view of the code generator, a name is
-- externally visible if it has to be declared as exported
-- in the .o file's symbol table; that is, made non-static.

externallyVisibleCLabel :: CLabel -> Bool -- not C "static"
externallyVisibleCLabel (CaseLabel _ _)	   = False
externallyVisibleCLabel (StringLitLabel _) = False
externallyVisibleCLabel (AsmTempLabel _)   = False
externallyVisibleCLabel (ModuleInitLabel _ _) = True
externallyVisibleCLabel (PlainModuleInitLabel _)= True
externallyVisibleCLabel (ModuleInitTableLabel _)= False
externallyVisibleCLabel ModuleRegdLabel    = False
externallyVisibleCLabel (RtsLabel _)	   = True
externallyVisibleCLabel (ForeignLabel _ _ _ _) = True
externallyVisibleCLabel (IdLabel name _ _)     = isExternalName name
externallyVisibleCLabel (CC_Label _)	   = True
externallyVisibleCLabel (CCS_Label _)	   = True
externallyVisibleCLabel (DynamicLinkerLabel _ _)  = False
externallyVisibleCLabel (HpcTicksLabel _)   = True
externallyVisibleCLabel HpcModuleNameLabel      = False
externallyVisibleCLabel (LargeBitmapLabel _) = False
externallyVisibleCLabel (LargeSRTLabel _) = False

-- -----------------------------------------------------------------------------
-- Finding the "type" of a CLabel 

-- For generating correct types in label declarations:

data CLabelType
  = CodeLabel	-- Address of some executable instructions
  | DataLabel	-- Address of data, not a GC ptr
  | GcPtrLabel	-- Address of a (presumably static) GC object

isCFunctionLabel :: CLabel -> Bool
isCFunctionLabel lbl = case labelType lbl of
			CodeLabel -> True
			_other	  -> False

isGcPtrLabel :: CLabel -> Bool
isGcPtrLabel lbl = case labelType lbl of
			GcPtrLabel -> True
			_other	   -> False

labelType :: CLabel -> CLabelType
labelType (RtsLabel (RtsSelectorInfoTable _ _)) = DataLabel
labelType (RtsLabel (RtsApInfoTable _ _))       = DataLabel
labelType (RtsLabel (RtsData _))              = DataLabel
labelType (RtsLabel (RtsGcPtr _))             = GcPtrLabel
labelType (RtsLabel (RtsCode _))              = CodeLabel
labelType (RtsLabel (RtsInfo _))              = DataLabel
labelType (RtsLabel (RtsEntry _))             = CodeLabel
labelType (RtsLabel (RtsRetInfo _))           = DataLabel
labelType (RtsLabel (RtsRet _))               = CodeLabel
labelType (RtsLabel (RtsDataFS _))            = DataLabel
labelType (RtsLabel (RtsCodeFS _))            = CodeLabel
labelType (RtsLabel (RtsInfoFS _))            = DataLabel
labelType (RtsLabel (RtsEntryFS _))           = CodeLabel
labelType (RtsLabel (RtsRetInfoFS _))         = DataLabel
labelType (RtsLabel (RtsRetFS _))             = CodeLabel
labelType (RtsLabel (RtsApFast _))            = CodeLabel
labelType (CaseLabel _ CaseReturnInfo)        = DataLabel
labelType (CaseLabel _ _)	              = CodeLabel
labelType (ModuleInitLabel _ _)               = CodeLabel
labelType (PlainModuleInitLabel _)            = CodeLabel
labelType (ModuleInitTableLabel _)            = DataLabel
labelType (LargeSRTLabel _)                   = DataLabel
labelType (LargeBitmapLabel _)                = DataLabel
labelType (ForeignLabel _ _ _ IsFunction) = CodeLabel
labelType (IdLabel _ _ info) = idInfoLabelType info
labelType _                = DataLabel

idInfoLabelType info =
  case info of
    InfoTable  	  -> DataLabel
    Closure    	  -> GcPtrLabel
    ConInfoTable  -> DataLabel
    StaticInfoTable -> DataLabel
    ClosureTable  -> DataLabel
    RednCounts    -> DataLabel
    _	          -> CodeLabel


-- -----------------------------------------------------------------------------
-- Does a CLabel need dynamic linkage?

-- When referring to data in code, we need to know whether
-- that data resides in a DLL or not. [Win32 only.]
-- @labelDynamic@ returns @True@ if the label is located
-- in a DLL, be it a data reference or not.

labelDynamic :: PackageId -> CLabel -> Bool
labelDynamic this_pkg lbl =
  case lbl of
   RtsLabel _  	     -> not opt_Static && (this_pkg /= rtsPackageId) -- i.e., is the RTS in a DLL or not?
   IdLabel n _ k       -> isDllName this_pkg n
#if mingw32_TARGET_OS
   ForeignLabel _ _ d _ -> d
#else
   -- On Mac OS X and on ELF platforms, false positives are OK,
   -- so we claim that all foreign imports come from dynamic libraries
   ForeignLabel _ _ _ _ -> True
#endif
   ModuleInitLabel m _    -> not opt_Static && this_pkg /= (modulePackageId m)
   PlainModuleInitLabel m -> not opt_Static && this_pkg /= (modulePackageId m)
   ModuleInitTableLabel m -> not opt_Static && this_pkg /= (modulePackageId m)
   
   -- Note that DynamicLinkerLabels do NOT require dynamic linking themselves.
   _ 		     -> False

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

	 info			Info table
	 srt			Static reference table
	 srtd			Static reference table descriptor
	 entry			Entry code (function, closure)
	 slow			Slow entry code (if any)
	 ret			Direct return address	 
	 vtbl			Vector table
	 <n>_alt		Case alternative (tag n)
	 dflt			Default case alternative
	 btm			Large bitmap vector
	 closure		Static closure
	 con_entry		Dynamic Constructor entry code
	 con_info		Dynamic Constructor info table
	 static_entry		Static Constructor entry code
	 static_info		Static Constructor info table
	 sel_info		Selector info table
	 sel_entry		Selector entry code
	 cc			Cost centre
	 ccs			Cost centre stack

Many of these distinctions are only for documentation reasons.  For
example, _ret is only distinguished from _entry to make it easy to
tell whether a code fragment is a return point or a closure/function
entry.
-}

instance Outputable CLabel where
  ppr = pprCLabel

pprCLabel :: CLabel -> SDoc

#if ! OMIT_NATIVE_CODEGEN
pprCLabel (AsmTempLabel u)
  =  getPprStyle $ \ sty ->
     if asmStyle sty then 
	ptext asmTempLabelPrefix <> pprUnique u
     else
	char '_' <> pprUnique u

pprCLabel (DynamicLinkerLabel info lbl)
   = pprDynamicLinkerAsmLabel info lbl
   
pprCLabel PicBaseLabel
   = ptext (sLit "1b")
   
pprCLabel (DeadStripPreventer lbl)
   = pprCLabel lbl <> ptext (sLit "_dsp")
#endif

pprCLabel lbl = 
#if ! OMIT_NATIVE_CODEGEN
    getPprStyle $ \ sty ->
    if asmStyle sty then 
	maybe_underscore (pprAsmCLbl lbl)
    else
#endif
       pprCLbl lbl

maybe_underscore doc
  | underscorePrefix = pp_cSEP <> doc
  | otherwise        = doc

#ifdef mingw32_TARGET_OS
-- In asm mode, we need to put the suffix on a stdcall ForeignLabel.
-- (The C compiler does this itself).
pprAsmCLbl (ForeignLabel fs (Just sz) _ _)
   = ftext fs <> char '@' <> int sz
#endif
pprAsmCLbl lbl
   = pprCLbl lbl

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

pprCLbl (LargeSRTLabel u)  = pprUnique u <> pp_cSEP <> ptext (sLit "srtd")
pprCLbl (LargeBitmapLabel u)  = text "b" <> pprUnique u <> pp_cSEP <> ptext (sLit "btm")
-- Some bitsmaps for tuple constructors have a numeric tag (e.g. '7')
-- until that gets resolved we'll just force them to start
-- with a letter so the label will be legal assmbly code.
        

pprCLbl (RtsLabel (RtsCode str))   = ptext str
pprCLbl (RtsLabel (RtsData str))   = ptext str
pprCLbl (RtsLabel (RtsGcPtr str))  = ptext str
pprCLbl (RtsLabel (RtsCodeFS str)) = ftext str
pprCLbl (RtsLabel (RtsDataFS str)) = ftext str

pprCLbl (RtsLabel (RtsApFast str)) = ptext str <> ptext (sLit "_fast")

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

pprCLbl (RtsLabel (RtsInfo fs))
  = ptext fs <> ptext (sLit "_info")

pprCLbl (RtsLabel (RtsEntry fs))
  = ptext fs <> ptext (sLit "_entry")

pprCLbl (RtsLabel (RtsRetInfo fs))
  = ptext fs <> ptext (sLit "_info")

pprCLbl (RtsLabel (RtsRet fs))
  = ptext fs <> ptext (sLit "_ret")

pprCLbl (RtsLabel (RtsInfoFS fs))
  = ftext fs <> ptext (sLit "_info")

pprCLbl (RtsLabel (RtsEntryFS fs))
  = ftext fs <> ptext (sLit "_entry")

pprCLbl (RtsLabel (RtsRetInfoFS fs))
  = ftext fs <> ptext (sLit "_info")

pprCLbl (RtsLabel (RtsRetFS fs))
  = ftext fs <> ptext (sLit "_ret")

pprCLbl (RtsLabel (RtsPrimOp primop)) 
  = ptext (sLit "stg_") <> ppr primop

pprCLbl (RtsLabel (RtsSlowTickyCtr pat)) 
  = ptext (sLit "SLOW_CALL_") <> text pat <> ptext (sLit "_ctr")

pprCLbl ModuleRegdLabel
  = ptext (sLit "_module_registered")

pprCLbl (ForeignLabel str _ _ _)
  = ftext str

pprCLbl (IdLabel name cafs flavor) = ppr name <> ppIdFlavor flavor

pprCLbl (CC_Label cc) 		= ppr cc
pprCLbl (CCS_Label ccs) 	= ppr ccs

pprCLbl (ModuleInitLabel mod way)
   = ptext (sLit "__stginit_") <> ppr mod
	<> char '_' <> text way
pprCLbl (PlainModuleInitLabel mod)
   = ptext (sLit "__stginit_") <> ppr mod
pprCLbl (ModuleInitTableLabel mod)
   = ptext (sLit "__stginittable_") <> ppr mod

pprCLbl (HpcTicksLabel mod)
  = ptext (sLit "_hpc_tickboxes_")  <> ppr mod <> ptext (sLit "_hpc")

pprCLbl HpcModuleNameLabel
  = ptext (sLit "_hpc_module_name_str")

ppIdFlavor :: IdLabelInfo -> SDoc
ppIdFlavor x = pp_cSEP <>
	       (case x of
		       Closure	    	-> ptext (sLit "closure")
		       SRT		-> ptext (sLit "srt")
		       InfoTable    	-> ptext (sLit "info")
		       Entry	    	-> ptext (sLit "entry")
		       Slow	    	-> ptext (sLit "slow")
		       RednCounts	-> ptext (sLit "ct")
		       ConEntry	    	-> ptext (sLit "con_entry")
		       ConInfoTable    	-> ptext (sLit "con_info")
		       StaticConEntry  	-> ptext (sLit "static_entry")
		       StaticInfoTable 	-> ptext (sLit "static_info")
		       ClosureTable     -> ptext (sLit "closure_tbl")
		      )


pp_cSEP = char '_'

-- -----------------------------------------------------------------------------
-- Machine-dependent knowledge about labels.

underscorePrefix :: Bool   -- leading underscore on assembler labels?
underscorePrefix = (cLeadingUnderscore == "YES")

asmTempLabelPrefix :: LitString  -- for formatting labels
asmTempLabelPrefix =
#if alpha_TARGET_OS
     {- The alpha assembler likes temporary labels to look like $L123
	instead of L123.  (Don't toss the L, because then Lf28
	turns into $f28.)
     -}
     (sLit "$")
#elif darwin_TARGET_OS
     (sLit "L")
#else
     (sLit ".L")
#endif

pprDynamicLinkerAsmLabel :: DynamicLinkerLabelInfo -> CLabel -> SDoc

#if x86_64_TARGET_ARCH && darwin_TARGET_OS
pprDynamicLinkerAsmLabel CodeStub lbl
  = char 'L' <> pprCLabel lbl <> text "$stub"
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = char 'L' <> pprCLabel lbl <> text "$non_lazy_ptr"
pprDynamicLinkerAsmLabel GotSymbolPtr lbl
  = pprCLabel lbl <> text "@GOTPCREL"
pprDynamicLinkerAsmLabel GotSymbolOffset lbl
  = pprCLabel lbl
pprDynamicLinkerAsmLabel _ _
  = panic "pprDynamicLinkerAsmLabel"
#elif darwin_TARGET_OS
pprDynamicLinkerAsmLabel CodeStub lbl
  = char 'L' <> pprCLabel lbl <> text "$stub"
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = char 'L' <> pprCLabel lbl <> text "$non_lazy_ptr"
pprDynamicLinkerAsmLabel _ _
  = panic "pprDynamicLinkerAsmLabel"
#elif powerpc_TARGET_ARCH && linux_TARGET_OS
pprDynamicLinkerAsmLabel CodeStub lbl
  = pprCLabel lbl <> text "@plt"
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = text ".LC_" <> pprCLabel lbl
pprDynamicLinkerAsmLabel _ _
  = panic "pprDynamicLinkerAsmLabel"
#elif x86_64_TARGET_ARCH && linux_TARGET_OS
pprDynamicLinkerAsmLabel CodeStub lbl
  = pprCLabel lbl <> text "@plt"
pprDynamicLinkerAsmLabel GotSymbolPtr lbl
  = pprCLabel lbl <> text "@gotpcrel"
pprDynamicLinkerAsmLabel GotSymbolOffset lbl
  = pprCLabel lbl
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = text ".LC_" <> pprCLabel lbl
#elif linux_TARGET_OS
pprDynamicLinkerAsmLabel CodeStub lbl
  = pprCLabel lbl <> text "@plt"
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = text ".LC_" <> pprCLabel lbl
pprDynamicLinkerAsmLabel GotSymbolPtr lbl
  = pprCLabel lbl <> text "@got"
pprDynamicLinkerAsmLabel GotSymbolOffset lbl
  = pprCLabel lbl <> text "@gotoff"
#elif mingw32_TARGET_OS
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = text "__imp_" <> pprCLabel lbl
pprDynamicLinkerAsmLabel _ _
  = panic "pprDynamicLinkerAsmLabel"
#else
pprDynamicLinkerAsmLabel _ _
  = panic "pprDynamicLinkerAsmLabel"
#endif
