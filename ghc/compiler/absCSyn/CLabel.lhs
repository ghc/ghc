%
% (c) The University of Glasgow, 1992-2002
%
\section[CLabel]{@CLabel@: Information to make C Labels}

\begin{code}
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

	mkReturnPtLabel,
	mkReturnInfoLabel,
	mkVecTblLabel,
	mkAltLabel,
	mkDefaultLabel,
	mkBitmapLabel,

	mkClosureTblLabel,

	mkAsmTempLabel,

	mkModuleInitLabel,
	mkPlainModuleInitLabel,

	mkErrorStdEntryLabel,

	mkStgUpdatePAPLabel,
	mkSplitMarkerLabel,
	mkUpdInfoLabel,
	mkSeqInfoLabel,
	mkIndInfoLabel,
	mkIndStaticInfoLabel,
	mkRtsGCEntryLabel,
        mkMainCapabilityLabel,
	mkCharlikeClosureLabel,
	mkIntlikeClosureLabel,
	mkMAP_FROZEN_infoLabel,
        mkEMPTY_MVAR_infoLabel,

	mkTopTickyCtrLabel,
	mkBlackHoleInfoTableLabel,
	mkBlackHoleBQInfoTableLabel,
        mkCAFBlackHoleInfoTableLabel,
        mkSECAFBlackHoleInfoTableLabel,
	mkRtsPrimOpLabel,

	moduleRegdLabel,

	mkSelectorInfoLabel,
	mkSelectorEntryLabel,

	mkRtsApplyInfoLabel,
	mkRtsApplyEntryLabel,

	mkForeignLabel,

	mkCC_Label, mkCCS_Label,
	
	needsCDecl, isAsmTemp, externallyVisibleCLabel,

	CLabelType(..), labelType, labelDynamic,

	pprCLabel
    ) where


#include "HsVersions.h"

#if ! OMIT_NATIVE_CODEGEN
import {-# SOURCE #-} MachMisc ( underscorePrefix, fmtAsmLbl )
#endif

import CmdLineOpts      ( opt_Static, opt_DoTickyProfiling )
import CStrings		( pp_cSEP )
import DataCon		( ConTag )
import Module		( moduleName, moduleNameFS, 
			  Module, isHomeModule )
import Name		( Name, getName, isDllName, isExternalName )
import TyCon		( TyCon )
import Unique		( pprUnique, Unique )
import PrimOp		( PrimOp )
import CostCentre	( CostCentre, CostCentreStack )
import Outputable
import FastString
\end{code}

things we want to find out:

* should the labelled things be declared "static" (visible only in this file)?

* should it be declared "const" (read-only text space)?

* does it need declarations at all? (v common Prelude things are pre-declared)

* what type does it have? (for generating accurate enough C declarations
  so that the C compiler won't complain).

\begin{code}
data CLabel
  = IdLabel	    		-- A family of labels related to the
	Name			-- definition of a particular Id
	IdLabelInfo

  | DataConLabel		-- Ditto data constructors
	Name
	DataConLabelInfo

  | CaseLabel			-- A family of labels related to a particular case expression
	Unique			-- Unique says which case expression
	CaseLabelInfo

  | TyConLabel TyCon		-- currently only one kind of TyconLabel,
				-- a 'Closure Table'.

  | AsmTempLabel    Unique

  | ModuleInitLabel 
	Module			-- the module name
	String			-- its "way"
	-- at some point we might want some kind of version number in
	-- the module init label, to guard against compiling modules in
	-- the wrong order.  We can't use the interface file version however,
	-- because we don't always recompile modules which depend on a module
	-- whose version has changed.

  | PlainModuleInitLabel Module	 -- without the vesrion & way info

  | RtsLabel	    RtsLabelInfo

  | ForeignLabel FastString Bool  -- a 'C' (or otherwise foreign) label
				   -- Bool <=> is dynamic

  | CC_Label CostCentre
  | CCS_Label CostCentreStack

  deriving (Eq, Ord)
\end{code}

\begin{code}
data IdLabelInfo
  = Closure		-- Label for (static???) closure
  | SRT                 -- Static reference table
  | SRTDesc             -- Static reference table descriptor
  | InfoTbl		-- Info tables for closures; always read-only
  | Entry		-- entry point
  | Slow		-- slow entry point

			-- Ticky-ticky counting
  | RednCounts		-- Label of place to keep reduction-count info for 
			-- this Id

  | Bitmap		-- A bitmap (function or case return)

  deriving (Eq, Ord)

data DataConLabelInfo
  = ConEntry		-- the only kind of entry pt for constructors
  | ConInfoTbl		-- corresponding info table
  | StaticConEntry  	-- static constructor entry point
  | StaticInfoTbl   	-- corresponding info table
  deriving (Eq, Ord)

data CaseLabelInfo
  = CaseReturnPt
  | CaseReturnInfo
  | CaseVecTbl
  | CaseAlt ConTag
  | CaseDefault
  deriving (Eq, Ord)

data RtsLabelInfo
  = RtsShouldNeverHappenCode

  | RtsBlackHoleInfoTbl LitString  -- black hole with info table name

  | RtsUpdInfo            	-- upd_frame_info
  | RtsSeqInfo			-- seq_frame_info
  | RtsGCEntryLabel String 	-- a heap check fail handler, eg  stg_chk_2
  | RtsMainCapability           -- MainCapability
  | Rts_Closure String		-- misc rts closures, eg CHARLIKE_closure
  | Rts_Info String		-- misc rts itbls, eg MUT_ARR_PTRS_FROZEN_info
  | Rts_Code String		-- misc rts code

  | RtsSelectorInfoTbl Bool{-updatable-} Int{-offset-}	-- Selector thunks
  | RtsSelectorEntry   Bool{-updatable-} Int{-offset-}

  | RtsApInfoTbl Bool{-updatable-} Int{-arity-}	        -- AP thunks
  | RtsApEntry   Bool{-updatable-} Int{-arity-}

  | RtsPrimOp PrimOp

  | RtsTopTickyCtr

  | RtsModuleRegd

  | RtsApplyInfoLabel  LitString
  | RtsApplyEntryLabel LitString

  deriving (Eq, Ord)

-- Label Type: for generating C declarations.

data CLabelType
  = RetInfoTblType
  | InfoTblType
  | ClosureType
  | VecTblType
  | ClosureTblType
  | CodeType
  | DataType
\end{code}

\begin{code}
mkClosureLabel	      	id 		= IdLabel id  Closure
mkSRTLabel		id		= IdLabel id  SRT
mkSRTDescLabel		id		= IdLabel id  SRTDesc
mkInfoTableLabel  	id 		= IdLabel id  InfoTbl
mkEntryLabel	      	id 		= IdLabel id  Entry
mkSlowEntryLabel      	id 		= IdLabel id  Slow
mkBitmapLabel   	id		= IdLabel id  Bitmap
mkRednCountsLabel     	id		= IdLabel id  RednCounts

mkStaticInfoTableLabel  con		= DataConLabel con StaticInfoTbl
mkConInfoTableLabel     con		= DataConLabel con ConInfoTbl
mkConEntryLabel	      	con		= DataConLabel con ConEntry
mkStaticConEntryLabel 	con		= DataConLabel con StaticConEntry


mkReturnPtLabel uniq		= CaseLabel uniq CaseReturnPt
mkReturnInfoLabel uniq		= CaseLabel uniq CaseReturnInfo
mkVecTblLabel   uniq		= CaseLabel uniq CaseVecTbl
mkAltLabel      uniq tag	= CaseLabel uniq (CaseAlt tag)
mkDefaultLabel  uniq 		= CaseLabel uniq CaseDefault


mkClosureTblLabel tycon		= TyConLabel tycon

mkAsmTempLabel 			= AsmTempLabel

mkModuleInitLabel		= ModuleInitLabel
mkPlainModuleInitLabel		= PlainModuleInitLabel

	-- Some fixed runtime system labels

mkErrorStdEntryLabel 		= RtsLabel RtsShouldNeverHappenCode
mkStgUpdatePAPLabel		= RtsLabel (Rts_Code "stg_update_PAP")
mkSplitMarkerLabel		= RtsLabel (Rts_Code "__stg_split_marker")
mkUpdInfoLabel			= RtsLabel RtsUpdInfo
mkSeqInfoLabel			= RtsLabel RtsSeqInfo
mkIndInfoLabel			= RtsLabel (Rts_Info "stg_IND_info")
mkIndStaticInfoLabel		= RtsLabel (Rts_Info "stg_IND_STATIC_info")
mkRtsGCEntryLabel str		= RtsLabel (RtsGCEntryLabel str)
mkMainCapabilityLabel		= RtsLabel RtsMainCapability
mkCharlikeClosureLabel		= RtsLabel (Rts_Closure "stg_CHARLIKE_closure")
mkIntlikeClosureLabel		= RtsLabel (Rts_Closure "stg_INTLIKE_closure")
mkMAP_FROZEN_infoLabel		= RtsLabel (Rts_Info "stg_MUT_ARR_PTRS_FROZEN_info")
mkEMPTY_MVAR_infoLabel		= RtsLabel (Rts_Info "stg_EMPTY_MVAR_info")

mkTopTickyCtrLabel		= RtsLabel RtsTopTickyCtr
mkBlackHoleInfoTableLabel	= RtsLabel (RtsBlackHoleInfoTbl SLIT("stg_BLACKHOLE_info"))
mkBlackHoleBQInfoTableLabel	= RtsLabel (RtsBlackHoleInfoTbl SLIT("stg_BLACKHOLE_BQ_info"))
mkCAFBlackHoleInfoTableLabel	= RtsLabel (RtsBlackHoleInfoTbl SLIT("stg_CAF_BLACKHOLE_info"))
mkSECAFBlackHoleInfoTableLabel	= if opt_DoTickyProfiling then
                                    RtsLabel (RtsBlackHoleInfoTbl SLIT("stg_SE_CAF_BLACKHOLE_info"))
                                  else  -- RTS won't have info table unless -ticky is on
                                    panic "mkSECAFBlackHoleInfoTableLabel requires -ticky"
mkRtsPrimOpLabel primop		= RtsLabel (RtsPrimOp primop)

moduleRegdLabel			= RtsLabel RtsModuleRegd

mkSelectorInfoLabel  upd off	= RtsLabel (RtsSelectorInfoTbl upd off)
mkSelectorEntryLabel upd off	= RtsLabel (RtsSelectorEntry   upd off)

mkApInfoTableLabel  upd off	= RtsLabel (RtsApInfoTbl upd off)
mkApEntryLabel upd off		= RtsLabel (RtsApEntry   upd off)

	-- Foreign labels

mkForeignLabel :: FastString -> Bool -> CLabel
mkForeignLabel str is_dynamic	= ForeignLabel str is_dynamic

	-- Cost centres etc.

mkCC_Label	cc		= CC_Label cc
mkCCS_Label	ccs		= CCS_Label ccs

-- Std RTS application routines

mkRtsApplyInfoLabel  = RtsLabel . RtsApplyInfoLabel
mkRtsApplyEntryLabel = RtsLabel . RtsApplyEntryLabel
\end{code}

\begin{code}
needsCDecl :: CLabel -> Bool	-- False <=> it's pre-declared; don't bother
isAsmTemp  :: CLabel -> Bool    -- is a local temporary for native code generation
externallyVisibleCLabel :: CLabel -> Bool -- not C "static"
\end{code}

@needsCDecl@ is @True@ unless the thing is a deeply-@PreludeCore@-ish
object.  {\em Also:} No need to spit out labels for things generated
by the flattener (in @AbsCUtils@)---it is careful to ensure references
to them are always backwards.  These are return-point and vector-table
labels.

Declarations for (non-prelude) @Id@-based things are needed because of
mutual recursion.

Declarations for direct return points are needed, because they may be
let-no-escapes, which can be recursive.

\begin{code}
  -- don't bother declaring SRT & Bitmap labels, we always make sure
  -- they are defined before use.
needsCDecl (IdLabel _ SRT)		= False
needsCDecl (IdLabel _ SRTDesc)		= False
needsCDecl (IdLabel _ Bitmap)		= False
needsCDecl (IdLabel _ _)		= True
needsCDecl (CaseLabel _ CaseReturnPt)	= True
needsCDecl (DataConLabel _ _)		= True
needsCDecl (TyConLabel _)		= True
needsCDecl (ModuleInitLabel _ _)	= True
needsCDecl (PlainModuleInitLabel _)	= True

needsCDecl (CaseLabel _ _)		= False
needsCDecl (AsmTempLabel _)		= False
needsCDecl (RtsLabel _)			= False
needsCDecl (ForeignLabel _ _)		= False
needsCDecl (CC_Label _)			= False
needsCDecl (CCS_Label _)		= False
\end{code}

Whether the label is an assembler temporary:

\begin{code}
isAsmTemp (AsmTempLabel _) = True
isAsmTemp _ 	    	   = False
\end{code}

C ``static'' or not...
From the point of view of the code generator, a name is
externally visible if it has to be declared as exported
in the .o file's symbol table; that is, made non-static.

\begin{code}
externallyVisibleCLabel (DataConLabel _ _) = True
externallyVisibleCLabel (TyConLabel tc)    = True
externallyVisibleCLabel (CaseLabel _ _)	   = False
externallyVisibleCLabel (AsmTempLabel _)   = False
externallyVisibleCLabel (ModuleInitLabel _ _)= True
externallyVisibleCLabel (PlainModuleInitLabel _)= True
externallyVisibleCLabel (RtsLabel RtsModuleRegd) = False --hack
externallyVisibleCLabel (RtsLabel _)	   = True
externallyVisibleCLabel (ForeignLabel _ _) = True
externallyVisibleCLabel (IdLabel id _)     = isExternalName id
externallyVisibleCLabel (CC_Label _)	   = False -- not strictly true
externallyVisibleCLabel (CCS_Label _)	   = False -- not strictly true
\end{code}

For generating correct types in label declarations, and also for
deciding whether the C compiler would like us to use '&' before the
label to get its address:

\begin{code}
labelType :: CLabel -> CLabelType
labelType (RtsLabel (RtsBlackHoleInfoTbl _))  = InfoTblType
labelType (RtsLabel (RtsSelectorInfoTbl _ _)) = InfoTblType
labelType (RtsLabel (RtsApInfoTbl _ _))       = InfoTblType
labelType (RtsLabel RtsUpdInfo)       	      = RetInfoTblType
labelType (RtsLabel RtsSeqInfo)       	      = RetInfoTblType
labelType (RtsLabel RtsTopTickyCtr)	      = CodeType -- XXX
labelType (RtsLabel (Rts_Info _))             = InfoTblType
labelType (RtsLabel (RtsApplyInfoLabel _))    = RetInfoTblType
labelType (RtsLabel (RtsApplyEntryLabel _))   = CodeType
labelType (CaseLabel _ CaseReturnInfo)        = RetInfoTblType
labelType (CaseLabel _ CaseReturnPt)	      = CodeType
labelType (CaseLabel _ CaseVecTbl)            = VecTblType
labelType (TyConLabel _)		      = ClosureTblType
labelType (ModuleInitLabel _ _)               = CodeType
labelType (PlainModuleInitLabel _)            = CodeType
labelType (CC_Label _)			      = CodeType -- hack
labelType (CCS_Label _)			      = CodeType -- hack

labelType (IdLabel _ info) = 
  case info of
    InfoTbl   -> InfoTblType
    Closure   -> ClosureType
    Bitmap    -> DataType
    _	      -> CodeType

labelType (DataConLabel _ info) = 
  case info of
     ConInfoTbl    -> InfoTblType
     StaticInfoTbl -> InfoTblType
     _		   -> CodeType

labelType _        = DataType
\end{code}

When referring to data in code, we need to know whether
that data resides in a DLL or not. [Win32 only.]
@labelDynamic@ returns @True@ if the label is located
in a DLL, be it a data reference or not.

\begin{code}
labelDynamic :: CLabel -> Bool
labelDynamic lbl = 
  case lbl of
   -- The special case for RtsShouldNeverHappenCode is because the associated address is
   -- NULL, i.e. not a DLL entry point
   RtsLabel RtsShouldNeverHappenCode -> False
   RtsLabel _  	     -> not opt_Static  -- i.e., is the RTS in a DLL or not?
   IdLabel n k       -> isDllName n
   DataConLabel n k  -> isDllName n
   TyConLabel tc     -> isDllName (getName tc)
   ForeignLabel _ d  -> d
   ModuleInitLabel m _  -> (not opt_Static) && (not (isHomeModule m))
   PlainModuleInitLabel m -> (not opt_Static) && (not (isHomeModule m))
   _ 		     -> False
\end{code}


OLD?: These GRAN functions are needed for spitting out GRAN_FETCH() at the
right places. It is used to detect when the abstractC statement of an
CCodeBlock actually contains the code for a slow entry point.  -- HWL

We need at least @Eq@ for @CLabels@, because we want to avoid
duplicate declarations in generating C (see @labelSeenTE@ in
@PprAbsC@).

-----------------------------------------------------------------------------
Printing out CLabels.

Convention:

      <name>_<type>

where <name> is <Module>_<name> for external names and <unique> for
internal names. <type> is one of the following:

	 info			Info table
	 srt			Static reference table
	 srtd			Static reference table descriptor
	 entry			Entry code
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

\begin{code}
pprCLabel :: CLabel -> SDoc

#if ! OMIT_NATIVE_CODEGEN
pprCLabel (AsmTempLabel u)
  = text (fmtAsmLbl (show u))
#endif

pprCLabel lbl = 
#if ! OMIT_NATIVE_CODEGEN
    getPprStyle $ \ sty ->
    if asmStyle sty && underscorePrefix then
       pp_cSEP <> pprCLbl lbl
    else
#endif
       pprCLbl lbl

pprCLbl (CaseLabel u CaseReturnPt)
  = hcat [pprUnique u, pp_cSEP, ptext SLIT("ret")]
pprCLbl (CaseLabel u CaseReturnInfo)
  = hcat [pprUnique u, pp_cSEP, ptext SLIT("info")]
pprCLbl (CaseLabel u CaseVecTbl)
  = hcat [pprUnique u, pp_cSEP, ptext SLIT("vtbl")]
pprCLbl (CaseLabel u (CaseAlt tag))
  = hcat [pprUnique u, pp_cSEP, int tag, pp_cSEP, ptext SLIT("alt")]
pprCLbl (CaseLabel u CaseDefault)
  = hcat [pprUnique u, pp_cSEP, ptext SLIT("dflt")]

pprCLbl (RtsLabel RtsShouldNeverHappenCode) = ptext SLIT("NULL")
-- used to be stg_error_entry but Windows can't have DLL entry points as static
-- initialisers, and besides, this ShouldNeverHappen, right?

pprCLbl (RtsLabel RtsUpdInfo)            = ptext SLIT("stg_upd_frame_info")
pprCLbl (RtsLabel RtsSeqInfo)            = ptext SLIT("stg_seq_frame_info")
pprCLbl (RtsLabel RtsMainCapability)     = ptext SLIT("MainCapability")
pprCLbl (RtsLabel (RtsGCEntryLabel str)) = text str
pprCLbl (RtsLabel (Rts_Closure str))     = text str
pprCLbl (RtsLabel (Rts_Info str))        = text str
pprCLbl (RtsLabel (Rts_Code str))        = text str

pprCLbl (RtsLabel RtsTopTickyCtr) = ptext SLIT("top_ct")

pprCLbl (RtsLabel (RtsBlackHoleInfoTbl info)) = ptext info

pprCLbl (RtsLabel (RtsSelectorInfoTbl upd_reqd offset))
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

pprCLbl (RtsLabel (RtsApInfoTbl upd_reqd arity))
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

pprCLbl (RtsLabel (RtsApplyInfoLabel  fs))
  = ptext SLIT("stg_ap_") <> ptext fs <> ptext SLIT("_info")

pprCLbl (RtsLabel (RtsApplyEntryLabel fs))
  = ptext SLIT("stg_ap_") <> ptext fs <> ptext SLIT("_ret")

pprCLbl (RtsLabel (RtsPrimOp primop)) 
  = ppr primop <> ptext SLIT("_fast")

pprCLbl (RtsLabel RtsModuleRegd)
  = ptext SLIT("module_registered")

pprCLbl (ForeignLabel str _)
  = ftext str

pprCLbl (TyConLabel tc)
  = hcat [ppr tc, pp_cSEP, ptext SLIT("closure_tbl")]

pprCLbl (IdLabel      id  flavor) = ppr id <> ppIdFlavor flavor
pprCLbl (DataConLabel con flavor) = ppr con <> ppConFlavor flavor

pprCLbl (CC_Label cc) 		= ppr cc
pprCLbl (CCS_Label ccs) 	= ppr ccs

pprCLbl (ModuleInitLabel mod way)	
   = ptext SLIT("__stginit_") <> ftext (moduleNameFS (moduleName mod))
	<> char '_' <> text way
pprCLbl (PlainModuleInitLabel mod)	
   = ptext SLIT("__stginit_") <> ftext (moduleNameFS (moduleName mod))

ppIdFlavor :: IdLabelInfo -> SDoc

ppIdFlavor x = pp_cSEP <>
	       (case x of
		       Closure	    	-> ptext SLIT("closure")
		       SRT		-> ptext SLIT("srt")
		       SRTDesc		-> ptext SLIT("srtd")
		       InfoTbl    	-> ptext SLIT("info")
		       Entry	    	-> ptext SLIT("entry")
		       Slow	    	-> ptext SLIT("slow")
		       RednCounts	-> ptext SLIT("ct")
		       Bitmap		-> ptext SLIT("btm")
		      )

ppConFlavor x = pp_cSEP <>
	     	(case x of
		       ConEntry	    	-> ptext SLIT("con_entry")
		       ConInfoTbl    	-> ptext SLIT("con_info")
		       StaticConEntry  	-> ptext SLIT("static_entry")
		       StaticInfoTbl 	-> ptext SLIT("static_info")
		)
\end{code}
