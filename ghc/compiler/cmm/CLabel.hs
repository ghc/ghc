-----------------------------------------------------------------------------
--
-- Object-file symbols (called CLabel for histerical raisins).
--
-- (c) The University of Glasgow 2004
--
-----------------------------------------------------------------------------

module CLabel (
	CLabel,	-- abstract type

	mkClosureLabel,
	mkSRTLabel,
	mkSRTDescLabel,
	mkInfoTableLabel,
	mkEntryLabel,
	mkSlowEntryLabel,
	mkConEntryLabel,
	mkStaticConEntryLabel,
	mkRednCountsLabel,
	mkConInfoTableLabel,
	mkStaticInfoTableLabel,
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

	mkSplitMarkerLabel,
	mkDirty_MUT_VAR_Label,
	mkUpdInfoLabel,
	mkSeqInfoLabel,
	mkIndStaticInfoLabel,
        mkMainCapabilityLabel,
	mkMAP_FROZEN_infoLabel,
	mkMAP_DIRTY_infoLabel,
        mkEMPTY_MVAR_infoLabel,

	mkTopTickyCtrLabel,
        mkCAFBlackHoleInfoTableLabel,
        mkSECAFBlackHoleInfoTableLabel,
	mkRtsPrimOpLabel,
	mkRtsSlowTickyCtrLabel,

	moduleRegdLabel,

	mkSelectorInfoLabel,
	mkSelectorEntryLabel,

	mkRtsInfoLabel,
	mkRtsEntryLabel,
	mkRtsRetInfoLabel,
	mkRtsRetLabel,
	mkRtsCodeLabel,
	mkRtsDataLabel,

	mkRtsInfoLabelFS,
	mkRtsEntryLabelFS,
	mkRtsRetInfoLabelFS,
	mkRtsRetLabelFS,
	mkRtsCodeLabelFS,
	mkRtsDataLabelFS,

	mkForeignLabel,

	mkCCLabel, mkCCSLabel,

        DynamicLinkerLabelInfo(..),
        mkDynamicLinkerLabel,
        dynamicLinkerLabelInfo,
        
        mkPicBaseLabel,
        mkDeadStripPreventer,

	infoLblToEntryLbl, entryLblToInfoLbl,
	needsCDecl, isAsmTemp, externallyVisibleCLabel,
	CLabelType(..), labelType, labelDynamic,

	pprCLabel
    ) where


#include "HsVersions.h"

import Packages		( HomeModules )
import StaticFlags	( opt_Static, opt_DoTickyProfiling )
import Packages		( isHomeModule, isDllName )
import DataCon		( ConTag )
import Module		( moduleFS, Module )
import Name		( Name, isExternalName )
import Unique		( pprUnique, Unique )
import PrimOp		( PrimOp )
import Config		( cLeadingUnderscore )
import CostCentre	( CostCentre, CostCentreStack )
import Outputable
import FastString

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
	IdLabelInfo

  | DynIdLabel			-- like IdLabel, but in a separate package,
	Name			-- and might therefore need a dynamic
	IdLabelInfo		-- reference.

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
	Bool			-- True <=> is in a different package
	-- at some point we might want some kind of version number in
	-- the module init label, to guard against compiling modules in
	-- the wrong order.  We can't use the interface file version however,
	-- because we don't always recompile modules which depend on a module
	-- whose version has changed.

  | PlainModuleInitLabel	-- without the vesrion & way info
	Module
	Bool			-- True <=> is in a different package

  | ModuleRegdLabel

  | RtsLabel RtsLabelInfo

  | ForeignLabel FastString	-- a 'C' (or otherwise foreign) label
	(Maybe Int) 		-- possible '@n' suffix for stdcall functions
		-- When generating C, the '@n' suffix is omitted, but when
		-- generating assembler we must add it to the label.
	Bool			-- True <=> is dynamic

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

  deriving (Eq, Ord)

data IdLabelInfo
  = Closure		-- Label for closure
  | SRT                 -- Static reference table
  | SRTDesc             -- Static reference table descriptor
  | InfoTable		-- Info tables for closures; always read-only
  | Entry		-- entry point
  | Slow		-- slow entry point

  | RednCounts		-- Label of place to keep Ticky-ticky  info for 
			-- this Id

  | Bitmap		-- A bitmap (function or case return)

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
  | RtsData       LitString	-- misc rts data bits, eg CHARLIKE_closure
  | RtsCode       LitString	-- misc rts code

  | RtsInfoFS     FastString	-- misc rts info tables
  | RtsEntryFS    FastString	-- misc rts entry points
  | RtsRetInfoFS  FastString	-- misc rts ret info tables
  | RtsRetFS      FastString	-- misc rts return points
  | RtsDataFS     FastString	-- misc rts data bits, eg CHARLIKE_closure
  | RtsCodeFS     FastString	-- misc rts code

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
mkSRTLabel		name 	= IdLabel name  SRT
mkSRTDescLabel		name 	= IdLabel name  SRTDesc
mkSlowEntryLabel      	name 	= IdLabel name  Slow
mkBitmapLabel   	name 	= IdLabel name  Bitmap
mkRednCountsLabel     	name 	= IdLabel name  RednCounts

-- These have local & (possibly) external variants:
mkLocalClosureLabel	name 	= IdLabel name  Closure
mkLocalInfoTableLabel  	name 	= IdLabel name  InfoTable
mkLocalEntryLabel	name 	= IdLabel name  Entry
mkLocalClosureTableLabel name	= IdLabel name ClosureTable

mkClosureLabel hmods name
  | isDllName hmods name = DynIdLabel    name Closure
  | otherwise             = IdLabel name Closure

mkInfoTableLabel hmods name
  | isDllName hmods name = DynIdLabel    name InfoTable
  | otherwise		 = IdLabel name InfoTable

mkEntryLabel hmods name
  | isDllName hmods name = DynIdLabel    name Entry
  | otherwise             = IdLabel name Entry

mkClosureTableLabel hmods name
  | isDllName hmods name = DynIdLabel    name ClosureTable
  | otherwise             = IdLabel name ClosureTable

mkLocalConInfoTableLabel     con = IdLabel con ConInfoTable
mkLocalConEntryLabel	     con = IdLabel con ConEntry
mkLocalStaticInfoTableLabel  con = IdLabel con StaticInfoTable
mkLocalStaticConEntryLabel   con = IdLabel con StaticConEntry

mkConInfoTableLabel name False = IdLabel    name ConInfoTable
mkConInfoTableLabel name True  = DynIdLabel name ConInfoTable

mkStaticInfoTableLabel name False = IdLabel    name StaticInfoTable
mkStaticInfoTableLabel name True  = DynIdLabel name StaticInfoTable

mkConEntryLabel hmods name
  | isDllName hmods name = DynIdLabel    name ConEntry
  | otherwise             = IdLabel name ConEntry

mkStaticConEntryLabel hmods name
  | isDllName hmods name = DynIdLabel    name StaticConEntry
  | otherwise             = IdLabel name StaticConEntry


mkReturnPtLabel uniq		= CaseLabel uniq CaseReturnPt
mkReturnInfoLabel uniq		= CaseLabel uniq CaseReturnInfo
mkAltLabel      uniq tag	= CaseLabel uniq (CaseAlt tag)
mkDefaultLabel  uniq 		= CaseLabel uniq CaseDefault

mkStringLitLabel		= StringLitLabel
mkAsmTempLabel 			= AsmTempLabel

mkModuleInitLabel :: HomeModules -> Module -> String -> CLabel
mkModuleInitLabel hmods mod way
  = ModuleInitLabel mod way $! (not (isHomeModule hmods mod))

mkPlainModuleInitLabel :: HomeModules -> Module -> CLabel
mkPlainModuleInitLabel hmods mod
  = PlainModuleInitLabel mod $! (not (isHomeModule hmods mod))

	-- Some fixed runtime system labels

mkSplitMarkerLabel		= RtsLabel (RtsCode SLIT("__stg_split_marker"))
mkDirty_MUT_VAR_Label		= RtsLabel (RtsCode SLIT("dirty_MUT_VAR"))
mkUpdInfoLabel			= RtsLabel (RtsInfo SLIT("stg_upd_frame"))
mkSeqInfoLabel			= RtsLabel (RtsInfo SLIT("stg_seq_frame"))
mkIndStaticInfoLabel		= RtsLabel (RtsInfo SLIT("stg_IND_STATIC"))
mkMainCapabilityLabel		= RtsLabel (RtsData SLIT("MainCapability"))
mkMAP_FROZEN_infoLabel		= RtsLabel (RtsInfo SLIT("stg_MUT_ARR_PTRS_FROZEN0"))
mkMAP_DIRTY_infoLabel		= RtsLabel (RtsInfo SLIT("stg_MUT_ARR_PTRS_DIRTY"))
mkEMPTY_MVAR_infoLabel		= RtsLabel (RtsInfo SLIT("stg_EMPTY_MVAR"))

mkTopTickyCtrLabel		= RtsLabel (RtsData SLIT("top_ct"))
mkCAFBlackHoleInfoTableLabel	= RtsLabel (RtsInfo SLIT("stg_CAF_BLACKHOLE"))
mkSECAFBlackHoleInfoTableLabel	= if opt_DoTickyProfiling then
                                    RtsLabel (RtsInfo SLIT("stg_SE_CAF_BLACKHOLE"))
                                  else  -- RTS won't have info table unless -ticky is on
                                    panic "mkSECAFBlackHoleInfoTableLabel requires -ticky"
mkRtsPrimOpLabel primop		= RtsLabel (RtsPrimOp primop)

moduleRegdLabel			= ModuleRegdLabel

mkSelectorInfoLabel  upd off	= RtsLabel (RtsSelectorInfoTable upd off)
mkSelectorEntryLabel upd off	= RtsLabel (RtsSelectorEntry   upd off)

mkApInfoTableLabel  upd off	= RtsLabel (RtsApInfoTable upd off)
mkApEntryLabel upd off		= RtsLabel (RtsApEntry   upd off)

	-- Foreign labels

mkForeignLabel :: FastString -> Maybe Int -> Bool -> CLabel
mkForeignLabel str mb_sz  is_dynamic = ForeignLabel str mb_sz is_dynamic

	-- Cost centres etc.

mkCCLabel	cc		= CC_Label cc
mkCCSLabel	ccs		= CCS_Label ccs

mkRtsInfoLabel      str = RtsLabel (RtsInfo      str)
mkRtsEntryLabel     str = RtsLabel (RtsEntry     str)
mkRtsRetInfoLabel   str = RtsLabel (RtsRetInfo   str)
mkRtsRetLabel       str = RtsLabel (RtsRet       str)
mkRtsCodeLabel      str = RtsLabel (RtsCode      str)
mkRtsDataLabel      str = RtsLabel (RtsData      str)

mkRtsInfoLabelFS    str = RtsLabel (RtsInfoFS    str)
mkRtsEntryLabelFS   str = RtsLabel (RtsEntryFS   str)
mkRtsRetInfoLabelFS str = RtsLabel (RtsRetInfoFS str)
mkRtsRetLabelFS     str = RtsLabel (RtsRetFS     str)
mkRtsCodeLabelFS    str = RtsLabel (RtsCodeFS    str)
mkRtsDataLabelFS    str = RtsLabel (RtsDataFS    str)

mkRtsSlowTickyCtrLabel :: String -> CLabel
mkRtsSlowTickyCtrLabel pat = RtsLabel (RtsSlowTickyCtr pat)

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
-- Converting info labels to entry labels.

infoLblToEntryLbl :: CLabel -> CLabel 
infoLblToEntryLbl (IdLabel n InfoTable) = IdLabel n Entry
infoLblToEntryLbl (IdLabel n ConInfoTable) = IdLabel n ConEntry
infoLblToEntryLbl (IdLabel n StaticInfoTable) = IdLabel n StaticConEntry
infoLblToEntryLbl (DynIdLabel n InfoTable) = DynIdLabel n Entry
infoLblToEntryLbl (DynIdLabel n ConInfoTable) = DynIdLabel n ConEntry
infoLblToEntryLbl (DynIdLabel n StaticInfoTable) = DynIdLabel n StaticConEntry
infoLblToEntryLbl (CaseLabel n CaseReturnInfo) = CaseLabel n CaseReturnPt
infoLblToEntryLbl (RtsLabel (RtsInfo s)) = RtsLabel (RtsEntry s)
infoLblToEntryLbl (RtsLabel (RtsRetInfo s)) = RtsLabel (RtsRet s)
infoLblToEntryLbl (RtsLabel (RtsInfoFS s)) = RtsLabel (RtsEntryFS s)
infoLblToEntryLbl (RtsLabel (RtsRetInfoFS s)) = RtsLabel (RtsRetFS s)
infoLblToEntryLbl _ = panic "CLabel.infoLblToEntryLbl"

entryLblToInfoLbl :: CLabel -> CLabel 
entryLblToInfoLbl (IdLabel n Entry) = IdLabel n InfoTable
entryLblToInfoLbl (IdLabel n ConEntry) = IdLabel n ConInfoTable
entryLblToInfoLbl (IdLabel n StaticConEntry) = IdLabel n StaticInfoTable
entryLblToInfoLbl (DynIdLabel n Entry) = DynIdLabel n InfoTable
entryLblToInfoLbl (DynIdLabel n ConEntry) = DynIdLabel n ConInfoTable
entryLblToInfoLbl (DynIdLabel n StaticConEntry) = DynIdLabel n StaticInfoTable
entryLblToInfoLbl (CaseLabel n CaseReturnPt) = CaseLabel n CaseReturnInfo
entryLblToInfoLbl (RtsLabel (RtsEntry s)) = RtsLabel (RtsInfo s)
entryLblToInfoLbl (RtsLabel (RtsRet s)) = RtsLabel (RtsRetInfo s)
entryLblToInfoLbl (RtsLabel (RtsEntryFS s)) = RtsLabel (RtsInfoFS s)
entryLblToInfoLbl (RtsLabel (RtsRetFS s)) = RtsLabel (RtsRetInfoFS s)
entryLblToInfoLbl l = pprPanic "CLabel.entryLblToInfoLbl" (pprCLabel l)

-- -----------------------------------------------------------------------------
-- Does a CLabel need declaring before use or not?

needsCDecl :: CLabel -> Bool
  -- False <=> it's pre-declared; don't bother
  -- don't bother declaring SRT & Bitmap labels, we always make sure
  -- they are defined before use.
needsCDecl (IdLabel _ SRT)		= False
needsCDecl (IdLabel _ SRTDesc)		= False
needsCDecl (IdLabel _ Bitmap)		= False
needsCDecl (IdLabel _ _)		= True
needsCDecl (DynIdLabel _ _)		= True
needsCDecl (CaseLabel _ _)	        = True
needsCDecl (ModuleInitLabel _ _ _)	= True
needsCDecl (PlainModuleInitLabel _ _)	= True
needsCDecl ModuleRegdLabel		= False

needsCDecl (StringLitLabel _)		= False
needsCDecl (AsmTempLabel _)		= False
needsCDecl (RtsLabel _)			= False
needsCDecl (ForeignLabel _ _ _)		= False
needsCDecl (CC_Label _)			= True
needsCDecl (CCS_Label _)		= True

-- Whether the label is an assembler temporary:

isAsmTemp  :: CLabel -> Bool    -- is a local temporary for native code generation
isAsmTemp (AsmTempLabel _) = True
isAsmTemp _ 	    	   = False

-- -----------------------------------------------------------------------------
-- Is a CLabel visible outside this object file or not?

-- From the point of view of the code generator, a name is
-- externally visible if it has to be declared as exported
-- in the .o file's symbol table; that is, made non-static.

externallyVisibleCLabel :: CLabel -> Bool -- not C "static"
externallyVisibleCLabel (CaseLabel _ _)	   = False
externallyVisibleCLabel (StringLitLabel _) = False
externallyVisibleCLabel (AsmTempLabel _)   = False
externallyVisibleCLabel (ModuleInitLabel _ _ _)= True
externallyVisibleCLabel (PlainModuleInitLabel _ _)= True
externallyVisibleCLabel ModuleRegdLabel    = False
externallyVisibleCLabel (RtsLabel _)	   = True
externallyVisibleCLabel (ForeignLabel _ _ _) = True
externallyVisibleCLabel (IdLabel name _)     = isExternalName name
externallyVisibleCLabel (DynIdLabel name _)  = isExternalName name
externallyVisibleCLabel (CC_Label _)	   = True
externallyVisibleCLabel (CCS_Label _)	   = True
externallyVisibleCLabel (DynamicLinkerLabel _ _)  = False

-- -----------------------------------------------------------------------------
-- Finding the "type" of a CLabel 

-- For generating correct types in label declarations:

data CLabelType
  = CodeLabel
  | DataLabel

labelType :: CLabel -> CLabelType
labelType (RtsLabel (RtsSelectorInfoTable _ _)) = DataLabel
labelType (RtsLabel (RtsApInfoTable _ _))       = DataLabel
labelType (RtsLabel (RtsData _))              = DataLabel
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
labelType (CaseLabel _ CaseReturnInfo)        = DataLabel
labelType (CaseLabel _ _)	              = CodeLabel
labelType (ModuleInitLabel _ _ _)             = CodeLabel
labelType (PlainModuleInitLabel _ _)          = CodeLabel

labelType (IdLabel _ info) = idInfoLabelType info
labelType (DynIdLabel _ info) = idInfoLabelType info
labelType _        = DataLabel

idInfoLabelType info =
  case info of
    InfoTable  	  -> DataLabel
    Closure    	  -> DataLabel
    Bitmap     	  -> DataLabel
    ConInfoTable  -> DataLabel
    StaticInfoTable -> DataLabel
    ClosureTable  -> DataLabel
    _	          -> CodeLabel


-- -----------------------------------------------------------------------------
-- Does a CLabel need dynamic linkage?

-- When referring to data in code, we need to know whether
-- that data resides in a DLL or not. [Win32 only.]
-- @labelDynamic@ returns @True@ if the label is located
-- in a DLL, be it a data reference or not.

labelDynamic :: CLabel -> Bool
labelDynamic lbl = 
  case lbl of
   RtsLabel _  	     -> not opt_Static  -- i.e., is the RTS in a DLL or not?
   IdLabel n k       -> False
   DynIdLabel n k    -> True
#if mingw32_TARGET_OS
   ForeignLabel _ _ d  -> d
#else
   -- On Mac OS X and on ELF platforms, false positives are OK,
   -- so we claim that all foreign imports come from dynamic libraries
   ForeignLabel _ _ _ -> True
#endif
   ModuleInitLabel m _ dyn    -> not opt_Static && dyn
   PlainModuleInitLabel m dyn -> not opt_Static && dyn
   
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
   = ptext SLIT("1b")
   
pprCLabel (DeadStripPreventer lbl)
   = pprCLabel lbl <> ptext SLIT("_dsp")
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
pprAsmCLbl (ForeignLabel fs (Just sz) _)
   = ftext fs <> char '@' <> int sz
#endif
pprAsmCLbl lbl
   = pprCLbl lbl

pprCLbl (StringLitLabel u)
  = pprUnique u <> ptext SLIT("_str")

pprCLbl (CaseLabel u CaseReturnPt)
  = hcat [pprUnique u, ptext SLIT("_ret")]
pprCLbl (CaseLabel u CaseReturnInfo)
  = hcat [pprUnique u, ptext SLIT("_info")]
pprCLbl (CaseLabel u (CaseAlt tag))
  = hcat [pprUnique u, pp_cSEP, int tag, ptext SLIT("_alt")]
pprCLbl (CaseLabel u CaseDefault)
  = hcat [pprUnique u, ptext SLIT("_dflt")]

pprCLbl (RtsLabel (RtsCode str))   = ptext str
pprCLbl (RtsLabel (RtsData str))   = ptext str
pprCLbl (RtsLabel (RtsCodeFS str)) = ftext str
pprCLbl (RtsLabel (RtsDataFS str)) = ftext str

pprCLbl (RtsLabel (RtsSelectorInfoTable upd_reqd offset))
  = hcat [ptext SLIT("stg_sel_"), text (show offset),
		ptext (if upd_reqd 
			then SLIT("_upd_info") 
			else SLIT("_noupd_info"))
	]

pprCLbl (RtsLabel (RtsSelectorEntry upd_reqd offset))
  = hcat [ptext SLIT("stg_sel_"), text (show offset),
		ptext (if upd_reqd 
			then SLIT("_upd_entry") 
			else SLIT("_noupd_entry"))
	]

pprCLbl (RtsLabel (RtsApInfoTable upd_reqd arity))
  = hcat [ptext SLIT("stg_ap_"), text (show arity),
		ptext (if upd_reqd 
			then SLIT("_upd_info") 
			else SLIT("_noupd_info"))
	]

pprCLbl (RtsLabel (RtsApEntry upd_reqd arity))
  = hcat [ptext SLIT("stg_ap_"), text (show arity),
		ptext (if upd_reqd 
			then SLIT("_upd_entry") 
			else SLIT("_noupd_entry"))
	]

pprCLbl (RtsLabel (RtsInfo fs))
  = ptext fs <> ptext SLIT("_info")

pprCLbl (RtsLabel (RtsEntry fs))
  = ptext fs <> ptext SLIT("_entry")

pprCLbl (RtsLabel (RtsRetInfo fs))
  = ptext fs <> ptext SLIT("_info")

pprCLbl (RtsLabel (RtsRet fs))
  = ptext fs <> ptext SLIT("_ret")

pprCLbl (RtsLabel (RtsInfoFS fs))
  = ftext fs <> ptext SLIT("_info")

pprCLbl (RtsLabel (RtsEntryFS fs))
  = ftext fs <> ptext SLIT("_entry")

pprCLbl (RtsLabel (RtsRetInfoFS fs))
  = ftext fs <> ptext SLIT("_info")

pprCLbl (RtsLabel (RtsRetFS fs))
  = ftext fs <> ptext SLIT("_ret")

pprCLbl (RtsLabel (RtsPrimOp primop)) 
  = ppr primop <> ptext SLIT("_fast")

pprCLbl (RtsLabel (RtsSlowTickyCtr pat)) 
  = ptext SLIT("SLOW_CALL_") <> text pat <> ptext SLIT("_ctr")

pprCLbl ModuleRegdLabel
  = ptext SLIT("_module_registered")

pprCLbl (ForeignLabel str _ _)
  = ftext str

pprCLbl (IdLabel name  flavor) = ppr name <> ppIdFlavor flavor
pprCLbl (DynIdLabel name  flavor) = ppr name <> ppIdFlavor flavor

pprCLbl (CC_Label cc) 		= ppr cc
pprCLbl (CCS_Label ccs) 	= ppr ccs

pprCLbl (ModuleInitLabel mod way _)	
   = ptext SLIT("__stginit_") <> ppr mod
	<> char '_' <> text way
pprCLbl (PlainModuleInitLabel mod _)	
   = ptext SLIT("__stginit_") <> ppr mod

ppIdFlavor :: IdLabelInfo -> SDoc
ppIdFlavor x = pp_cSEP <>
	       (case x of
		       Closure	    	-> ptext SLIT("closure")
		       SRT		-> ptext SLIT("srt")
		       SRTDesc		-> ptext SLIT("srtd")
		       InfoTable    	-> ptext SLIT("info")
		       Entry	    	-> ptext SLIT("entry")
		       Slow	    	-> ptext SLIT("slow")
		       RednCounts	-> ptext SLIT("ct")
		       Bitmap		-> ptext SLIT("btm")
		       ConEntry	    	-> ptext SLIT("con_entry")
		       ConInfoTable    	-> ptext SLIT("con_info")
		       StaticConEntry  	-> ptext SLIT("static_entry")
		       StaticInfoTable 	-> ptext SLIT("static_info")
		       ClosureTable     -> ptext SLIT("closure_tbl")
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
     SLIT("$")
#elif darwin_TARGET_OS
     SLIT("L")
#else
     SLIT(".L")
#endif

pprDynamicLinkerAsmLabel :: DynamicLinkerLabelInfo -> CLabel -> SDoc

#if darwin_TARGET_OS
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = char 'L' <> pprCLabel lbl <> text "$non_lazy_ptr"
pprDynamicLinkerAsmLabel CodeStub lbl
  = char 'L' <> pprCLabel lbl <> text "$stub"
#elif powerpc_TARGET_ARCH && linux_TARGET_OS
pprDynamicLinkerAsmLabel CodeStub lbl
  = pprCLabel lbl <> text "@plt"
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = text ".LC_" <> pprCLabel lbl
#elif linux_TARGET_OS
pprDynamicLinkerAsmLabel CodeStub lbl
  = pprCLabel lbl <> text "@plt"
pprDynamicLinkerAsmLabel GotSymbolPtr lbl
  = pprCLabel lbl <> text "@got"
pprDynamicLinkerAsmLabel GotSymbolOffset lbl
  = pprCLabel lbl <> text "@gotoff"
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = text ".LC_" <> pprCLabel lbl
#elif mingw32_TARGET_OS
pprDynamicLinkerAsmLabel SymbolPtr lbl
  = text "__imp_" <> pprCLabel lbl
#endif
pprDynamicLinkerAsmLabel _ _
  = panic "pprDynamicLinkerAsmLabel"
