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
	ForeignLabelSource(..),
	pprDebugCLabel,

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
	mkRtsPrimOpLabel,
	mkRtsSlowTickyCtrLabel,

        mkSelectorInfoLabel,
	mkSelectorEntryLabel,

	mkCmmInfoLabel,
	mkCmmEntryLabel,
	mkCmmRetInfoLabel,
	mkCmmRetLabel,
	mkCmmCodeLabel,
	mkCmmDataLabel,
	mkCmmGcPtrLabel,

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
	infoLblToEntryLbl, entryLblToInfoLbl, cvtToClosureLbl, cvtToSRTLbl,
	localiseLabel,
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
	IdLabelInfo		-- encodes the suffix of the label
  
  -- | A label from a .cmm file that is not associated with a .hs level Id.
  | CmmLabel			
	PackageId		-- what package the label belongs to.
	FastString		-- identifier giving the prefix of the label
	CmmLabelInfo		-- encodes the suffix of the label

  -- | A label with a baked-in \/ algorithmically generated name that definitely
  --    comes from the RTS. The code for it must compile into libHSrts.a \/ libHSrts.so
  --    If it doesn't have an algorithmically generated name then use a CmmLabel 
  --    instead and give it an appropriate PackageId argument.
  | RtsLabel 			
	RtsLabelInfo

  -- | A 'C' (or otherwise foreign) label.
  --
  | ForeignLabel 
  	FastString    		-- name of the imported label.

        (Maybe Int)		-- possible '@n' suffix for stdcall functions
				-- When generating C, the '@n' suffix is omitted, but when
				-- generating assembler we must add it to the label.

	ForeignLabelSource	-- what package the foreign label is in.
	
        FunctionOrData

  -- | A family of labels related to a particular case expression.
  | CaseLabel			
	{-# UNPACK #-} !Unique	-- Unique says which case expression
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
  -- 	They are special variants of a label used for dynamic linking
  --    see module PositionIndependentCode for details.
  | DynamicLinkerLabel DynamicLinkerLabelInfo CLabel
 
  -- | This label is generated and used inside the NCG only. 
  -- 	It is used as a base for PIC calculations on some platforms.
  --    It takes the form of a local numeric assembler label '1'; and 
  --    is pretty-printed as 1b, referring to the previous definition
  --    of 1: in the assembler source file.
  | PicBaseLabel                
 
  -- | A label before an info table to prevent excessive dead-stripping on darwin
  | DeadStripPreventer CLabel


  -- | Per-module table of tick locations
  | HpcTicksLabel Module

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
   = ForeignLabelInPackage	PackageId
  
   -- | Label is in some external, system package that doesn't also
   --	contain compiled Haskell code, and is not associated with any .hi files.
   --	We don't have to worry about Haskell code being inlined from
   --	external packages. It is safe to treat the RTS package as "external".
   | ForeignLabelInExternalPackage 

   -- | Label is in the package currenly being compiled.
   --	This is only used for creating hacky tmp labels during code generation.
   --	Don't use it in any code that might be inlined across a package boundary
   --	(ie, core code) else the information will be wrong relative to the
   --	destination module.
   | ForeignLabelInThisPackage
      
   deriving (Eq, Ord)   


-- | For debugging problems with the CLabel representation.
--	We can't make a Show instance for CLabel because lots of its components don't have instances.
--	The regular Outputable instance only shows the label name, and not its other info.
--
pprDebugCLabel :: CLabel -> SDoc
pprDebugCLabel lbl
 = case lbl of
 	IdLabel{}	-> ppr lbl <> (parens $ text "IdLabel")
	CmmLabel pkg name _info	
	 -> ppr lbl <> (parens $ text "CmmLabel" <+> ppr pkg)

	RtsLabel{}	-> ppr lbl <> (parens $ text "RtsLabel")

	ForeignLabel name mSuffix src funOrData
	 -> ppr lbl <> (parens 
	 			$ text "ForeignLabel" 
	 			<+> ppr mSuffix
				<+> ppr src  
				<+> ppr funOrData)

	_		-> ppr lbl <> (parens $ text "other CLabel)")


-- True if a local IdLabel that we won't mark as exported
type IsLocal = Bool

data IdLabelInfo
  = Closure		-- ^ Label for closure
  | SRT                 -- ^ Static reference table
  | InfoTable IsLocal	-- ^ Info tables for closures; always read-only
  | Entry IsLocal	-- ^ Entry point
  | Slow		-- ^ Slow entry point

  | RednCounts		-- ^ Label of place to keep Ticky-ticky  info for this Id

  | ConEntry	  	-- ^ Constructor entry point
  | ConInfoTable 	-- ^ Corresponding info table
  | StaticConEntry  	-- ^ Static constructor entry point
  | StaticInfoTable   	-- ^ Corresponding info table

  | ClosureTable	-- ^ Table of closures for Enum tycons

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
  | RtsApFast	  FastString	-- ^ _fast versions of generic apply
  | RtsSlowTickyCtr String

  deriving (Eq, Ord)
  -- NOTE: Eq on LitString compares the pointer only, so this isn't
  -- a real equality.


-- | What type of Cmm label we're dealing with.
-- 	Determines the suffix appended to the name when a CLabel.CmmLabel
--	is pretty printed.
data CmmLabelInfo
  = CmmInfo       		-- ^ misc rts info tabless,	suffix _info
  | CmmEntry      		-- ^ misc rts entry points,	suffix _entry
  | CmmRetInfo    		-- ^ misc rts ret info tables,	suffix _info
  | CmmRet        		-- ^ misc rts return points,	suffix _ret
  | CmmData       		-- ^ misc rts data bits, eg CHARLIKE_closure
  | CmmCode       		-- ^ misc rts code
  | CmmGcPtr			-- ^ GcPtrs eg CHARLIKE_closure  
  | CmmPrimCall			-- ^ a prim call to some hand written Cmm code
  deriving (Eq, Ord)

data DynamicLinkerLabelInfo
  = CodeStub			-- MachO: Lfoo$stub, ELF: foo@plt
  | SymbolPtr			-- MachO: Lfoo$non_lazy_ptr, Windows: __imp_foo
  | GotSymbolPtr		-- ELF: foo@got
  | GotSymbolOffset		-- ELF: foo@gotoff
  
  deriving (Eq, Ord)
 

-- -----------------------------------------------------------------------------
-- Constructing CLabels
-- -----------------------------------------------------------------------------

-- Constructing IdLabels 
-- These are always local:
mkSRTLabel		name c	= IdLabel name  c SRT
mkSlowEntryLabel      	name c 	= IdLabel name  c Slow
mkRednCountsLabel     	name c 	= IdLabel name  c RednCounts

-- These have local & (possibly) external variants:
mkLocalClosureLabel	name c 	= IdLabel name  c Closure
mkLocalInfoTableLabel  	name c 	= IdLabel name  c (InfoTable True)
mkLocalEntryLabel	name c 	= IdLabel name  c (Entry True)
mkLocalClosureTableLabel name c = IdLabel name  c ClosureTable

mkClosureLabel name         c     = IdLabel name c Closure
mkInfoTableLabel name       c     = IdLabel name c (InfoTable False)
mkEntryLabel name           c     = IdLabel name c (Entry False)
mkClosureTableLabel name    c     = IdLabel name c ClosureTable
mkLocalConInfoTableLabel    c con = IdLabel con c ConInfoTable
mkLocalConEntryLabel	    c con = IdLabel con c ConEntry
mkLocalStaticInfoTableLabel c con = IdLabel con c StaticInfoTable
mkLocalStaticConEntryLabel  c con = IdLabel con c StaticConEntry
mkConInfoTableLabel name    c     = IdLabel    name c ConInfoTable
mkStaticInfoTableLabel name c     = IdLabel    name c StaticInfoTable

mkConEntryLabel name        c     = IdLabel name c ConEntry
mkStaticConEntryLabel name  c     = IdLabel name c StaticConEntry

-- Constructing Cmm Labels
mkSplitMarkerLabel		= CmmLabel rtsPackageId (fsLit "__stg_split_marker")	CmmCode
mkDirty_MUT_VAR_Label		= CmmLabel rtsPackageId (fsLit "dirty_MUT_VAR")		CmmCode
mkUpdInfoLabel			= CmmLabel rtsPackageId (fsLit "stg_upd_frame")		CmmInfo
mkBHUpdInfoLabel		= CmmLabel rtsPackageId (fsLit "stg_bh_upd_frame" )     CmmInfo
mkIndStaticInfoLabel		= CmmLabel rtsPackageId (fsLit "stg_IND_STATIC")	CmmInfo
mkMainCapabilityLabel		= CmmLabel rtsPackageId (fsLit "MainCapability")	CmmData
mkMAP_FROZEN_infoLabel		= CmmLabel rtsPackageId (fsLit "stg_MUT_ARR_PTRS_FROZEN0") CmmInfo
mkMAP_DIRTY_infoLabel		= CmmLabel rtsPackageId (fsLit "stg_MUT_ARR_PTRS_DIRTY") CmmInfo
mkEMPTY_MVAR_infoLabel		= CmmLabel rtsPackageId (fsLit "stg_EMPTY_MVAR")	CmmInfo
mkTopTickyCtrLabel		= CmmLabel rtsPackageId (fsLit "top_ct")		CmmData
mkCAFBlackHoleInfoTableLabel	= CmmLabel rtsPackageId (fsLit "stg_CAF_BLACKHOLE")	CmmInfo

-----
mkCmmInfoLabel,   mkCmmEntryLabel, mkCmmRetInfoLabel, mkCmmRetLabel,
  mkCmmCodeLabel, mkCmmDataLabel,  mkCmmGcPtrLabel
	:: PackageId -> FastString -> CLabel

mkCmmInfoLabel      pkg str 	= CmmLabel pkg str CmmInfo
mkCmmEntryLabel     pkg str 	= CmmLabel pkg str CmmEntry
mkCmmRetInfoLabel   pkg str 	= CmmLabel pkg str CmmRetInfo
mkCmmRetLabel       pkg str 	= CmmLabel pkg str CmmRet
mkCmmCodeLabel      pkg str	= CmmLabel pkg str CmmCode
mkCmmDataLabel      pkg str	= CmmLabel pkg str CmmData
mkCmmGcPtrLabel     pkg str	= CmmLabel pkg str CmmGcPtr


-- Constructing RtsLabels
mkRtsPrimOpLabel primop		= RtsLabel (RtsPrimOp primop)

mkSelectorInfoLabel  upd off	= RtsLabel (RtsSelectorInfoTable upd off)
mkSelectorEntryLabel upd off	= RtsLabel (RtsSelectorEntry     upd off)

mkApInfoTableLabel   upd off	= RtsLabel (RtsApInfoTable       upd off)
mkApEntryLabel       upd off	= RtsLabel (RtsApEntry           upd off)


-- A call to some primitive hand written Cmm code
mkPrimCallLabel :: PrimCall -> CLabel
mkPrimCallLabel (PrimCall str pkg)  
	= CmmLabel pkg str CmmPrimCall


-- Constructing ForeignLabels

-- | Make a foreign label
mkForeignLabel 
	:: FastString 		-- name
	-> Maybe Int 		-- size prefix
	-> ForeignLabelSource	-- what package it's in
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
mkLargeSRTLabel	uniq		= LargeSRTLabel uniq
mkBitmapLabel	uniq		= LargeBitmapLabel uniq


-- Constructin CaseLabels
mkReturnPtLabel uniq		= CaseLabel uniq CaseReturnPt
mkReturnInfoLabel uniq		= CaseLabel uniq CaseReturnInfo
mkAltLabel      uniq tag	= CaseLabel uniq (CaseAlt tag)
mkDefaultLabel  uniq 		= CaseLabel uniq CaseDefault

-- Constructing Cost Center Labels
mkCCLabel	    cc		= CC_Label cc
mkCCSLabel	    ccs		= CCS_Label ccs

mkRtsApFastLabel str = RtsLabel (RtsApFast str)

mkRtsSlowTickyCtrLabel :: String -> CLabel
mkRtsSlowTickyCtrLabel pat = RtsLabel (RtsSlowTickyCtr pat)


-- Constructing Code Coverage Labels
mkHpcTicksLabel                = HpcTicksLabel


-- Constructing labels used for dynamic linking
mkDynamicLinkerLabel :: DynamicLinkerLabelInfo -> CLabel -> CLabel
mkDynamicLinkerLabel 		= DynamicLinkerLabel

dynamicLinkerLabelInfo :: CLabel -> Maybe (DynamicLinkerLabelInfo, CLabel)
dynamicLinkerLabelInfo (DynamicLinkerLabel info lbl) = Just (info, lbl)
dynamicLinkerLabelInfo _ 	= Nothing
    
mkPicBaseLabel :: CLabel
mkPicBaseLabel 			= PicBaseLabel


-- Constructing miscellaneous other labels
mkDeadStripPreventer :: CLabel -> CLabel
mkDeadStripPreventer lbl	= DeadStripPreventer lbl

mkStringLitLabel :: Unique -> CLabel
mkStringLitLabel		= StringLitLabel

mkAsmTempLabel :: Uniquable a => a -> CLabel
mkAsmTempLabel a		= AsmTempLabel (getUnique a)

mkPlainModuleInitLabel :: Module -> CLabel
mkPlainModuleInitLabel mod	= PlainModuleInitLabel mod

-- -----------------------------------------------------------------------------
-- Converting between info labels and entry/ret labels.

infoLblToEntryLbl :: CLabel -> CLabel 
infoLblToEntryLbl (IdLabel n c (InfoTable lcl))	= IdLabel n c (Entry lcl)
infoLblToEntryLbl (IdLabel n c ConInfoTable)	= IdLabel n c ConEntry
infoLblToEntryLbl (IdLabel n c StaticInfoTable)	= IdLabel n c StaticConEntry
infoLblToEntryLbl (CaseLabel n CaseReturnInfo)	= CaseLabel n CaseReturnPt
infoLblToEntryLbl (CmmLabel m str CmmInfo)	= CmmLabel m str CmmEntry
infoLblToEntryLbl (CmmLabel m str CmmRetInfo)	= CmmLabel m str CmmRet
infoLblToEntryLbl _
	= panic "CLabel.infoLblToEntryLbl"


entryLblToInfoLbl :: CLabel -> CLabel 
entryLblToInfoLbl (IdLabel n c (Entry lcl))	= IdLabel n c (InfoTable lcl)
entryLblToInfoLbl (IdLabel n c ConEntry)	= IdLabel n c ConInfoTable
entryLblToInfoLbl (IdLabel n c StaticConEntry)	= IdLabel n c StaticInfoTable
entryLblToInfoLbl (CaseLabel n CaseReturnPt)	= CaseLabel n CaseReturnInfo
entryLblToInfoLbl (CmmLabel m str CmmEntry)	= CmmLabel m str CmmInfo
entryLblToInfoLbl (CmmLabel m str CmmRet)	= CmmLabel m str CmmRetInfo
entryLblToInfoLbl l				
	= pprPanic "CLabel.entryLblToInfoLbl" (pprCLabel l)


cvtToClosureLbl   (IdLabel n c (InfoTable _))	= IdLabel n c Closure
cvtToClosureLbl   (IdLabel n c (Entry _))	= IdLabel n c Closure
cvtToClosureLbl   (IdLabel n c ConEntry)	= IdLabel n c Closure
cvtToClosureLbl   (IdLabel n c RednCounts)	= IdLabel n c Closure
cvtToClosureLbl l@(IdLabel n c Closure)		= l
cvtToClosureLbl l 
	= pprPanic "cvtToClosureLbl" (pprCLabel l)


cvtToSRTLbl   (IdLabel n c (InfoTable _))	= mkSRTLabel n c
cvtToSRTLbl   (IdLabel n c (Entry _))		= mkSRTLabel n c
cvtToSRTLbl   (IdLabel n c ConEntry)		= mkSRTLabel n c
cvtToSRTLbl l@(IdLabel n c Closure)		= mkSRTLabel n c
cvtToSRTLbl l 
	= pprPanic "cvtToSRTLbl" (pprCLabel l)

localiseLabel :: CLabel -> CLabel
localiseLabel (IdLabel n c (Entry _))     = IdLabel n c (Entry True)
localiseLabel (IdLabel n c (InfoTable _)) = IdLabel n c (InfoTable True)
localiseLabel l = l


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
needsCDecl (PlainModuleInitLabel _)     = True

needsCDecl (StringLitLabel _)		= False
needsCDecl (AsmTempLabel _)		= False
needsCDecl (RtsLabel _)			= False

needsCDecl (CmmLabel pkgId _ _)		
	-- Prototypes for labels defined in the runtime system are imported
	--	into HC files via includes/Stg.h.
	| pkgId == rtsPackageId		= False
	
	-- For other labels we inline one into the HC file directly.
	| otherwise			= True

needsCDecl l@(ForeignLabel{})		= not (isMathFun l)
needsCDecl (CC_Label _)			= True
needsCDecl (CCS_Label _)		= True
needsCDecl (HpcTicksLabel _)            = True


-- | Check whether a label is a local temporary for native code generation
isAsmTemp  :: CLabel -> Bool    
isAsmTemp (AsmTempLabel _) 		= True
isAsmTemp _ 	    	   		= False


-- | If a label is a local temporary used for native code generation
--      then return just its unique, otherwise nothing.
maybeAsmTemp :: CLabel -> Maybe Unique
maybeAsmTemp (AsmTempLabel uq) 		= Just uq
maybeAsmTemp _ 	    	       		= Nothing


-- | Check whether a label corresponds to a C function that has 
--      a prototype in a system header somehere, or is built-in
--      to the C compiler. For these labels we avoid generating our
--      own C prototypes.
isMathFun :: CLabel -> Bool
isMathFun (ForeignLabel fs _ _ _) 	= fs `elementOfUniqSet` math_funs
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
-- | Is a CLabel visible outside this object file or not?
-- 	From the point of view of the code generator, a name is
-- 	externally visible if it has to be declared as exported
-- 	in the .o file's symbol table; that is, made non-static.
externallyVisibleCLabel :: CLabel -> Bool -- not C "static"
externallyVisibleCLabel (CaseLabel _ _)		= False
externallyVisibleCLabel (StringLitLabel _)	= False
externallyVisibleCLabel (AsmTempLabel _)	= False
externallyVisibleCLabel (PlainModuleInitLabel _)= True
externallyVisibleCLabel (RtsLabel _)            = True
externallyVisibleCLabel (CmmLabel _ _ _)	= True
externallyVisibleCLabel (ForeignLabel{})	= True
externallyVisibleCLabel (IdLabel name _ info)	= isExternalName name && externallyVisibleIdLabel info
externallyVisibleCLabel (CC_Label _)		= True
externallyVisibleCLabel (CCS_Label _)		= True
externallyVisibleCLabel (DynamicLinkerLabel _ _)  = False
externallyVisibleCLabel (HpcTicksLabel _)	= True
externallyVisibleCLabel (LargeBitmapLabel _)    = False
externallyVisibleCLabel (LargeSRTLabel _)	= False

externallyVisibleIdLabel :: IdLabelInfo -> Bool
externallyVisibleIdLabel SRT             = False
externallyVisibleIdLabel (Entry lcl)     = not lcl
externallyVisibleIdLabel (InfoTable lcl) = not lcl
externallyVisibleIdLabel _               = True

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


-- | Work out the general type of data at the address of this label
--    whether it be code, data, or static GC object.
labelType :: CLabel -> CLabelType
labelType (CmmLabel _ _ CmmData)		= DataLabel
labelType (CmmLabel _ _ CmmGcPtr)		= GcPtrLabel
labelType (CmmLabel _ _ CmmCode)		= CodeLabel
labelType (CmmLabel _ _ CmmInfo)		= DataLabel
labelType (CmmLabel _ _ CmmEntry)		= CodeLabel
labelType (CmmLabel _ _ CmmRetInfo)		= DataLabel
labelType (CmmLabel _ _ CmmRet)			= CodeLabel
labelType (RtsLabel (RtsSelectorInfoTable _ _)) = DataLabel
labelType (RtsLabel (RtsApInfoTable _ _))       = DataLabel
labelType (RtsLabel (RtsApFast _))              = CodeLabel
labelType (CaseLabel _ CaseReturnInfo)          = DataLabel
labelType (CaseLabel _ _)	                = CodeLabel
labelType (PlainModuleInitLabel _)              = CodeLabel
labelType (LargeSRTLabel _)                     = DataLabel
labelType (LargeBitmapLabel _)                  = DataLabel
labelType (ForeignLabel _ _ _ IsFunction)	= CodeLabel
labelType (IdLabel _ _ info)                    = idInfoLabelType info
labelType _                                     = DataLabel

idInfoLabelType info =
  case info of
    InfoTable _   -> DataLabel
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
   -- is the RTS in a DLL or not?
   RtsLabel _  	     	-> not opt_Static && (this_pkg /= rtsPackageId)

   IdLabel n _ k     	-> isDllName this_pkg n

#if mingw32_TARGET_OS
   -- When compiling in the "dyn" way, eack package is to be linked into its own shared library.
   CmmLabel pkg _ _
    -> not opt_Static && (this_pkg /= pkg)

   -- Foreign label is in some un-named foreign package (or DLL)
   ForeignLabel _ _ ForeignLabelInExternalPackage _  -> True

   -- Foreign label is linked into the same package as the source file currently being compiled.
   ForeignLabel _ _ ForeignLabelInThisPackage  _     -> False
      
   -- Foreign label is in some named package.
   --	When compiling in the "dyn" way, each package is to be linked into its own DLL.
   ForeignLabel _ _ (ForeignLabelInPackage pkgId) _
    -> (not opt_Static) && (this_pkg /= pkgId)

#else
   -- On Mac OS X and on ELF platforms, false positives are OK,
   -- so we claim that all foreign imports come from dynamic libraries
   ForeignLabel _ _ _ _ -> True

   CmmLabel pkg _ _     -> True 

#endif
   PlainModuleInitLabel m -> not opt_Static && this_pkg /= (modulePackageId m)

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

pprCLabel (AsmTempLabel u)
 | cGhcWithNativeCodeGen == "YES"
  =  getPprStyle $ \ sty ->
     if asmStyle sty then 
	ptext asmTempLabelPrefix <> pprUnique u
     else
	char '_' <> pprUnique u

pprCLabel (DynamicLinkerLabel info lbl)
 | cGhcWithNativeCodeGen == "YES"
   = pprDynamicLinkerAsmLabel info lbl
   
pprCLabel PicBaseLabel
 | cGhcWithNativeCodeGen == "YES"
   = ptext (sLit "1b")
   
pprCLabel (DeadStripPreventer lbl)
 | cGhcWithNativeCodeGen == "YES"
   = pprCLabel lbl <> ptext (sLit "_dsp")

pprCLabel lbl
   = getPprStyle $ \ sty ->
     if cGhcWithNativeCodeGen == "YES" && asmStyle sty
     then maybe_underscore (pprAsmCLbl lbl)
     else pprCLbl lbl

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
        

pprCLbl (CmmLabel _ str CmmCode)	= ftext str
pprCLbl (CmmLabel _ str CmmData)	= ftext str
pprCLbl (CmmLabel _ str CmmGcPtr)	= ftext str
pprCLbl (CmmLabel _ str CmmPrimCall)	= ftext str

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

pprCLbl (RtsLabel (RtsPrimOp primop)) 
  = ptext (sLit "stg_") <> ppr primop

pprCLbl (RtsLabel (RtsSlowTickyCtr pat)) 
  = ptext (sLit "SLOW_CALL_") <> text pat <> ptext (sLit "_ctr")

pprCLbl (ForeignLabel str _ _ _)
  = ftext str

pprCLbl (IdLabel name cafs flavor) = ppr name <> ppIdFlavor flavor

pprCLbl (CC_Label cc) 		= ppr cc
pprCLbl (CCS_Label ccs) 	= ppr ccs

pprCLbl (PlainModuleInitLabel mod)
   = ptext (sLit "__stginit_") <> ppr mod

pprCLbl (HpcTicksLabel mod)
  = ptext (sLit "_hpc_tickboxes_")  <> ppr mod <> ptext (sLit "_hpc")

ppIdFlavor :: IdLabelInfo -> SDoc
ppIdFlavor x = pp_cSEP <>
	       (case x of
		       Closure	    	-> ptext (sLit "closure")
		       SRT		-> ptext (sLit "srt")
		       InfoTable _	-> ptext (sLit "info")
		       Entry _	    	-> ptext (sLit "entry")
		       Slow	    	-> ptext (sLit "slow")
		       RednCounts	-> ptext (sLit "ct")
		       ConEntry	    	-> ptext (sLit "con_entry")
		       ConInfoTable    	-> ptext (sLit "con_info")
		       StaticConEntry  	-> ptext (sLit "static_entry")
		       StaticInfoTable 	-> ptext (sLit "static_info")
		       ClosureTable     -> ptext (sLit "closure_tbl")
		      )


pp_cSEP = char '_'


instance Outputable ForeignLabelSource where
 ppr fs
  = case fs of
  	ForeignLabelInPackage pkgId	-> parens $ text "package: " <> ppr pkgId 
	ForeignLabelInThisPackage	-> parens $ text "this package"
	ForeignLabelInExternalPackage	-> parens $ text "external package"

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

#elif powerpc_TARGET_ARCH && elf_OBJ_FORMAT
pprDynamicLinkerAsmLabel CodeStub lbl
  = pprCLabel lbl <> text "@plt"
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = text ".LC_" <> pprCLabel lbl
pprDynamicLinkerAsmLabel _ _
  = panic "pprDynamicLinkerAsmLabel"

#elif x86_64_TARGET_ARCH && elf_OBJ_FORMAT
pprDynamicLinkerAsmLabel CodeStub lbl
  = pprCLabel lbl <> text "@plt"
pprDynamicLinkerAsmLabel GotSymbolPtr lbl
  = pprCLabel lbl <> text "@gotpcrel"
pprDynamicLinkerAsmLabel GotSymbolOffset lbl
  = pprCLabel lbl
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = text ".LC_" <> pprCLabel lbl

#elif elf_OBJ_FORMAT
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
