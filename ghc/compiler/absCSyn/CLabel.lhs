%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CLabel.lhs,v 1.36 2000/05/22 17:05:57 simonmar Exp $
%
\section[CLabel]{@CLabel@: Information to make C Labels}

\begin{code}
module CLabel (
	CLabel,	-- abstract type

	mkClosureLabel,
	mkSRTLabel,
	mkInfoTableLabel,
	mkStdEntryLabel,
	mkFastEntryLabel,
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

	mkErrorStdEntryLabel,

	mkStgUpdatePAPLabel,
	mkUpdInfoLabel,
	mkSeqInfoLabel,
	mkIndInfoLabel,
	mkIndStaticInfoLabel,
	mkRtsGCEntryLabel,
        mkMainRegTableLabel,
	mkCharlikeClosureLabel,
	mkIntlikeClosureLabel,
	mkTopClosureLabel,
	mkErrorIO_innardsLabel,
	mkMAP_FROZEN_infoLabel,

	mkTopTickyCtrLabel,
	mkBlackHoleInfoTableLabel,
        mkCAFBlackHoleInfoTableLabel,
        mkSECAFBlackHoleInfoTableLabel,
	mkRtsPrimOpLabel,

	moduleRegdLabel,

	mkSelectorInfoLabel,
	mkSelectorEntryLabel,

	mkForeignLabel,

	mkCC_Label, mkCCS_Label,
	
	needsCDecl, isAsmTemp, externallyVisibleCLabel,

	CLabelType(..), labelType, labelDynamic,

	pprCLabel
#if ! OMIT_NATIVE_CODEGEN
	, pprCLabel_asm
#endif
    ) where


#include "HsVersions.h"

#if ! OMIT_NATIVE_CODEGEN
import {-# SOURCE #-} MachMisc ( underscorePrefix, fmtAsmLbl )
#endif

import CmdLineOpts      ( opt_Static, opt_DoTickyProfiling )
import CStrings		( pp_cSEP )
import DataCon		( ConTag, DataCon )
import Module		( ModuleName )
import Name		( Name, getName, isDllName, isExternallyVisibleName )
import TyCon		( TyCon )
import Unique		( pprUnique, Unique )
import PrimOp		( PrimOp, pprPrimOp )
import CostCentre	( CostCentre, CostCentreStack )
import Util
import Outputable
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

  | ModuleInitLabel ModuleName

  | RtsLabel	    RtsLabelInfo

  | ForeignLabel FAST_STRING Bool  -- a 'C' (or otherwise foreign) label
				   -- Bool <=> is dynamic

  | CC_Label CostCentre
  | CCS_Label CostCentreStack

  deriving (Eq, Ord)
\end{code}

\begin{code}
data IdLabelInfo
  = Closure		-- Label for (static???) closure

  | SRT                 -- Static reference table

  | InfoTbl		-- Info table for a closure; always read-only

  | EntryStd		-- Thunk, or "slow", code entry point

  | EntryFast Int	-- entry pt when no arg satisfaction chk needed;
			-- Int is the arity of the function (to be
			-- encoded into the name)

			-- Ticky-ticky counting
  | RednCounts		-- Label of place to keep reduction-count info for 
			-- this Id
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
  | CaseBitmap
  deriving (Eq, Ord)

data RtsLabelInfo
  = RtsShouldNeverHappenCode

  | RtsBlackHoleInfoTbl FAST_STRING  -- black hole with info table name

  | RtsUpdInfo            	-- upd_frame_info
  | RtsSeqInfo			-- seq_frame_info
  | RtsGCEntryLabel String 	-- a heap check fail handler, eg  stg_chk_2
  | RtsMainRegTable             -- MainRegTable (??? Capabilities wurble ???)
  | Rts_Closure String		-- misc rts closures, eg CHARLIKE_closure
  | Rts_Info String		-- misc rts itbls, eg MUT_ARR_PTRS_FROZEN_info
  | Rts_Code String		-- misc rts code, eg ErrorIO_innards

  | RtsSelectorInfoTbl Bool{-updatable-} Int{-offset-}	-- Selector thunks
  | RtsSelectorEntry   Bool{-updatable-} Int{-offset-}

  | RtsApInfoTbl Bool{-updatable-} Int{-arity-}	        -- AP thunks
  | RtsApEntry   Bool{-updatable-} Int{-arity-}

  | RtsPrimOp PrimOp

  | RtsTopTickyCtr

  | RtsModuleRegd

  deriving (Eq, Ord)

-- Label Type: for generating C declarations.

data CLabelType
  = InfoTblType
  | ClosureType
  | VecTblType
  | ClosureTblType
  | CodeType
  | DataType
\end{code}

\begin{code}
mkClosureLabel	      	id 		= IdLabel id  Closure
mkSRTLabel		id		= IdLabel id  SRT
mkInfoTableLabel      	id 		= IdLabel id  InfoTbl
mkStdEntryLabel	      	id 		= IdLabel id  EntryStd
mkFastEntryLabel      	id arity	= ASSERT(arity > 0)
					  IdLabel id  (EntryFast arity)

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
mkBitmapLabel   uniq		= CaseLabel uniq CaseBitmap

mkClosureTblLabel tycon		= TyConLabel tycon

mkAsmTempLabel 			= AsmTempLabel

mkModuleInitLabel		= ModuleInitLabel

	-- Some fixed runtime system labels

mkErrorStdEntryLabel		= RtsLabel RtsShouldNeverHappenCode

mkStgUpdatePAPLabel		= RtsLabel (Rts_Code "stg_update_PAP")
mkUpdInfoLabel			= RtsLabel RtsUpdInfo
mkSeqInfoLabel			= RtsLabel RtsSeqInfo
mkIndInfoLabel			= RtsLabel (Rts_Info "IND_info")
mkIndStaticInfoLabel		= RtsLabel (Rts_Info "IND_STATIC_info")
mkRtsGCEntryLabel str		= RtsLabel (RtsGCEntryLabel str)
mkMainRegTableLabel		= RtsLabel RtsMainRegTable
mkCharlikeClosureLabel		= RtsLabel (Rts_Closure "CHARLIKE_closure")
mkIntlikeClosureLabel		= RtsLabel (Rts_Closure "INTLIKE_closure")
mkTopClosureLabel		= RtsLabel (Rts_Closure "TopClosure")
mkErrorIO_innardsLabel		= RtsLabel (Rts_Code "ErrorIO_innards")
mkMAP_FROZEN_infoLabel		= RtsLabel (Rts_Info "MUT_ARR_PTRS_FROZEN_info")

mkTopTickyCtrLabel		= RtsLabel RtsTopTickyCtr
mkBlackHoleInfoTableLabel	= RtsLabel (RtsBlackHoleInfoTbl SLIT("BLACKHOLE_info"))
mkCAFBlackHoleInfoTableLabel	= RtsLabel (RtsBlackHoleInfoTbl SLIT("CAF_BLACKHOLE_info"))
mkSECAFBlackHoleInfoTableLabel	= if opt_DoTickyProfiling then
                                    RtsLabel (RtsBlackHoleInfoTbl SLIT("SE_CAF_BLACKHOLE_info"))
                                  else  -- RTS won't have info table unless -ticky is on
                                    panic "mkSECAFBlackHoleInfoTableLabel requires -ticky"
mkRtsPrimOpLabel primop		= RtsLabel (RtsPrimOp primop)

moduleRegdLabel			= RtsLabel RtsModuleRegd

mkSelectorInfoLabel  upd off	= RtsLabel (RtsSelectorInfoTbl upd off)
mkSelectorEntryLabel upd off	= RtsLabel (RtsSelectorEntry   upd off)

mkApInfoTableLabel  upd off	= RtsLabel (RtsApInfoTbl upd off)
mkApEntryLabel upd off		= RtsLabel (RtsApEntry   upd off)

	-- Foreign labels

mkForeignLabel :: FAST_STRING -> Bool -> CLabel
mkForeignLabel str is_dynamic	= ForeignLabel str is_dynamic

	-- Cost centres etc.

mkCC_Label	cc		= CC_Label cc
mkCCS_Label	ccs		= CCS_Label ccs
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
needsCDecl (IdLabel _ _)		= True
needsCDecl (CaseLabel _ CaseReturnPt)	= True
needsCDecl (DataConLabel _ _)		= True
needsCDecl (CaseLabel _ _)		= False
needsCDecl (TyConLabel _)		= True

needsCDecl (AsmTempLabel _)		= False
needsCDecl (ModuleInitLabel _)		= False
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
externallyVisibleCLabel (ModuleInitLabel _)= True
externallyVisibleCLabel (RtsLabel RtsModuleRegd) = False --hack
externallyVisibleCLabel (RtsLabel _)	   = True
externallyVisibleCLabel (ForeignLabel _ _) = True
externallyVisibleCLabel (IdLabel id _)     = isExternallyVisibleName id
externallyVisibleCLabel (CC_Label _)	   = False -- not strictly true
externallyVisibleCLabel (CCS_Label _)	   = False -- not strictly true
\end{code}

For generating correct types in label declarations...

\begin{code}
labelType :: CLabel -> CLabelType
labelType (RtsLabel (RtsBlackHoleInfoTbl _))  = InfoTblType
labelType (RtsLabel (RtsSelectorInfoTbl _ _)) = InfoTblType
labelType (RtsLabel (RtsApInfoTbl _ _))       = InfoTblType
labelType (RtsLabel RtsUpdInfo)       	      = InfoTblType
labelType (CaseLabel _ CaseReturnInfo)        = InfoTblType
labelType (CaseLabel _ CaseReturnPt)	      = CodeType
labelType (CaseLabel _ CaseVecTbl)            = VecTblType
labelType (TyConLabel _)		      = ClosureTblType

labelType (IdLabel _ info) = 
  case info of
    InfoTbl       -> InfoTblType
    Closure	  -> ClosureType
    _		  -> CodeType

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
   RtsLabel _  	    -> not opt_Static  -- i.e., is the RTS in a DLL or not?
   IdLabel n k      -> isDllName n
   DataConLabel n k -> isDllName n
   TyConLabel tc    -> isDllName (getName tc)
   ForeignLabel _ d -> d
   _ 		    -> False
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
	 entry			Entry code
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
-- specialised for PprAsm: saves lots of arg passing in NCG
#if ! OMIT_NATIVE_CODEGEN
pprCLabel_asm = pprCLabel
#endif

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
pprCLbl (CaseLabel u CaseBitmap)
  = hcat [pprUnique u, pp_cSEP, ptext SLIT("btm")]

pprCLbl (RtsLabel RtsShouldNeverHappenCode) = ptext SLIT("stg_error_entry")

pprCLbl (RtsLabel RtsUpdInfo)            = ptext SLIT("upd_frame_info")
pprCLbl (RtsLabel RtsSeqInfo)            = ptext SLIT("seq_frame_info")
pprCLbl (RtsLabel RtsMainRegTable)       = ptext SLIT("MainRegTable")
pprCLbl (RtsLabel (RtsGCEntryLabel str)) = text str
pprCLbl (RtsLabel (Rts_Closure str))     = text str
pprCLbl (RtsLabel (Rts_Info str))        = text str
pprCLbl (RtsLabel (Rts_Code str))        = text str

pprCLbl (RtsLabel RtsTopTickyCtr) = ptext SLIT("top_ct")

pprCLbl (RtsLabel (RtsBlackHoleInfoTbl info)) = ptext info

pprCLbl (RtsLabel (RtsSelectorInfoTbl upd_reqd offset))
  = hcat [ptext SLIT("__sel_"), text (show offset),
		ptext (if upd_reqd 
			then SLIT("_upd_info") 
			else SLIT("_noupd_info"))
	]

pprCLbl (RtsLabel (RtsSelectorEntry upd_reqd offset))
  = hcat [ptext SLIT("__sel_"), text (show offset),
		ptext (if upd_reqd 
			then SLIT("_upd_entry") 
			else SLIT("_noupd_entry"))
	]

pprCLbl (RtsLabel (RtsApInfoTbl upd_reqd arity))
  = hcat [ptext SLIT("__ap_"), text (show arity),
		ptext (if upd_reqd 
			then SLIT("_upd_info") 
			else SLIT("_noupd_info"))
	]

pprCLbl (RtsLabel (RtsApEntry upd_reqd arity))
  = hcat [ptext SLIT("__ap_"), text (show arity),
		ptext (if upd_reqd 
			then SLIT("_upd_entry") 
			else SLIT("_noupd_entry"))
	]

pprCLbl (RtsLabel (RtsPrimOp primop)) 
  = pprPrimOp primop <> ptext SLIT("_fast")

pprCLbl (RtsLabel RtsModuleRegd)
  = ptext SLIT("module_registered")

pprCLbl (ForeignLabel str _)
  = ptext str

pprCLbl (TyConLabel tc)
  = hcat [ppr tc, pp_cSEP, ptext SLIT("closure_tbl")]

pprCLbl (IdLabel      id  flavor) = ppr id <> ppIdFlavor flavor
pprCLbl (DataConLabel con flavor) = ppr con <> ppConFlavor flavor

pprCLbl (CC_Label cc) 		= ppr cc
pprCLbl (CCS_Label ccs) 	= ppr ccs

pprCLbl (ModuleInitLabel mod)	= ptext SLIT("__init_") <> ptext mod

ppIdFlavor :: IdLabelInfo -> SDoc

ppIdFlavor x = pp_cSEP <>
	       (case x of
		       Closure	    	-> ptext SLIT("closure")
		       SRT		-> ptext SLIT("srt")
		       InfoTbl	    	-> ptext SLIT("info")
		       EntryStd	    	-> ptext SLIT("entry")
		       EntryFast arity	-> --false:ASSERT (arity > 0)
					   (<>) (ptext SLIT("fast")) (int arity)
		       RednCounts	-> ptext SLIT("ct")
		      )

ppConFlavor x = pp_cSEP <>
	     	(case x of
		       ConEntry	    	-> ptext SLIT("con_entry")
		       ConInfoTbl    	-> ptext SLIT("con_info")
		       StaticConEntry  	-> ptext SLIT("static_entry")
		       StaticInfoTbl 	-> ptext SLIT("static_info")
		)
\end{code}

