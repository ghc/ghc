%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CLabel]{@CLabel@: Information to make C Labels}

\begin{code}
module CLabel (
	CLabel,	-- abstract type

	mkClosureLabel,
	mkInfoTableLabel,
	mkStdEntryLabel,
	mkFastEntryLabel,
	mkConEntryLabel,
	mkStaticConEntryLabel,
	mkRednCountsLabel,
	mkConInfoTableLabel,
	mkPhantomInfoTableLabel,
	mkStaticClosureLabel,
	mkStaticInfoTableLabel,
	mkVapEntryLabel,
	mkVapInfoTableLabel,

	mkConUpdCodePtrVecLabel,
	mkStdUpdCodePtrVecLabel,

	mkInfoTableVecTblLabel,
	mkStdUpdVecTblLabel,

	mkReturnPtLabel,
	mkVecTblLabel,
	mkAltLabel,
	mkDefaultLabel,

	mkAsmTempLabel,

	mkErrorStdEntryLabel,
	mkBlackHoleInfoTableLabel,

	needsCDecl, isReadOnly, isAsmTemp, externallyVisibleCLabel,

	pprCLabel
#if ! OMIT_NATIVE_CODEGEN
	, pprCLabel_asm
#endif
    ) where


#include "HsVersions.h"

#if ! OMIT_NATIVE_CODEGEN
import {-# SOURCE #-} MachMisc ( underscorePrefix, fmtAsmLbl )
#endif

import CgRetConv	( CtrlReturnConvention(..), ctrlReturnConvAlg )
import CStrings		( pp_cSEP )
import Id		( externallyVisibleId,
			  isDataCon, isDictFunId,
			  isDefaultMethodId_maybe,
			  fIRST_TAG,
			  ConTag,
			  Id
			)
import Maybes		( maybeToBool )
import PprType		( showTyCon, GenType{-instance Outputable-} )
import TyCon		( TyCon{-instance Eq-} )
import Unique		( showUnique, pprUnique, Unique{-instance Eq-} )
import Util		( assertPanic{-, pprTraceToDo:rm-} )
import Outputable
\end{code}

things we want to find out:

* should the labelled things be declared "static" (visible only in this file)?

* should it be declared "const" (read-only text space)?

* does it need declarations at all? (v common Prelude things are pre-declared)

\begin{code}
data CLabel
  = IdLabel	    		-- A family of labels related to the
	CLabelId		-- definition of a particular Id
	IdLabelInfo		-- Includes DataCon

  | TyConLabel			-- A family of labels related to the
	TyCon			-- definition of a data type
	TyConLabelInfo

  | CaseLabel			-- A family of labels related to a particular case expression
	Unique			-- Unique says which case expression
	CaseLabelInfo

  | AsmTempLabel    Unique

  | RtsLabel	    RtsLabelInfo

  deriving (Eq, Ord)
\end{code}

The CLabelId is simply so we can declare alternative Eq and Ord
instances which use cmpId_SpecDataCon (instead of cmpId). This avoids
comparing the Uniques of two specialised data constructors (which have
the same as the uniques their respective unspecialised data
constructors). Instead, the specialising types and the uniques of the
unspecialised constructors are compared.

\begin{code}
data CLabelId = CLabelId Id

instance Eq CLabelId where
    CLabelId a == CLabelId b = case (a `compare` b) of { EQ -> True;  _ -> False }
    CLabelId a /= CLabelId b = case (a `compare` b) of { EQ -> False; _ -> True  }

instance Ord CLabelId where
    CLabelId a <= CLabelId b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    CLabelId a <  CLabelId b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    CLabelId a >= CLabelId b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    CLabelId a >  CLabelId b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare (CLabelId a) (CLabelId b) = a `compare` b
\end{code}

\begin{code}
data IdLabelInfo
  = Closure		-- Label for (static???) closure
  | StaticClosure	-- Static closure -- e.g., nullary constructor

  | InfoTbl		-- Info table for a closure; always read-only

  | EntryStd		-- Thunk, or "slow", code entry point (requires arg satis check)
  | EntryFast Int	-- entry pt when no arg satisfaction chk needed;
			-- Int is the arity of the function (to be
			-- encoded into the name)

  | ConEntry		-- the only kind of entry pt for constructors
  | ConInfoTbl		-- corresponding info table

  | StaticConEntry  	-- static constructor entry point
  | StaticInfoTbl   	-- corresponding info table

  | PhantomInfoTbl  	-- for phantom constructors that only exist in regs

  | VapInfoTbl Bool	-- True <=> the update-reqd version; False <=> the no-update-reqd version
  | VapEntry   Bool

	-- Ticky-ticky counting
  | RednCounts		-- Label of place to keep reduction-count info for this Id
  deriving (Eq, Ord)


data TyConLabelInfo
  = UnvecConUpdCode	 -- Update code for the data type if it's unvectored

  | VecConUpdCode ConTag -- One for each constructor which returns in
		    	 -- regs; this code actually performs an update

  | StdUpdCode ConTag	 -- Update code for all constructors which return
    	    	    	 -- in heap.  There are a small number of variants,
    	    	    	 -- so that the update code returns (vectored/n or
			 -- unvectored) in the right way.
			 -- ToDo: maybe replace TyCon/Int with return conv.

  | InfoTblVecTbl	 -- For tables of info tables

  | StdUpdVecTbl	 -- Labels the update code, or table of update codes,
			 -- for a particular type.
  deriving (Eq, Ord)

data CaseLabelInfo
  = CaseReturnPt
  | CaseVecTbl
  | CaseAlt ConTag
  | CaseDefault
  deriving (Eq, Ord)

data RtsLabelInfo
  = RtsShouldNeverHappenCode

  | RtsBlackHoleInfoTbl

  | RtsSelectorInfoTbl	-- Selectors
	Bool		-- True <=> the update-reqd version;
			-- False <=> the no-update-reqd version
	Int		-- 0-indexed Offset from the "goods"

  | RtsSelectorEntry	-- Ditto entry code
	Bool
	Int
  deriving (Eq, Ord)
\end{code}

\begin{code}
mkClosureLabel	      	id 		= IdLabel (CLabelId id)  Closure
mkInfoTableLabel      	id 		= IdLabel (CLabelId id)  InfoTbl
mkStdEntryLabel	      	id 		= IdLabel (CLabelId id)  EntryStd
mkFastEntryLabel      	id arity	= ASSERT(arity > 0)
					  IdLabel (CLabelId id)  (EntryFast arity)

mkStaticClosureLabel	con		= ASSERT(isDataCon con)
					  IdLabel (CLabelId con) StaticClosure
mkStaticInfoTableLabel  con		= ASSERT(isDataCon con)
					  IdLabel (CLabelId con) StaticInfoTbl
mkConInfoTableLabel     con		= ASSERT(isDataCon con)
					  IdLabel (CLabelId con) ConInfoTbl
mkPhantomInfoTableLabel con		= ASSERT(isDataCon con)
					  IdLabel (CLabelId con) PhantomInfoTbl
mkConEntryLabel	      	con		= ASSERT(isDataCon con)
					  IdLabel (CLabelId con) ConEntry
mkStaticConEntryLabel 	con		= ASSERT(isDataCon con)
					  IdLabel (CLabelId con) StaticConEntry

mkRednCountsLabel     	id		= IdLabel (CLabelId id)  RednCounts
mkVapEntryLabel		id upd_flag	= IdLabel (CLabelId id)  (VapEntry upd_flag)
mkVapInfoTableLabel	id upd_flag	= IdLabel (CLabelId id)  (VapInfoTbl upd_flag)

mkConUpdCodePtrVecLabel   tycon tag = TyConLabel tycon (VecConUpdCode tag)
mkStdUpdCodePtrVecLabel   tycon tag = TyConLabel tycon (StdUpdCode tag)

mkInfoTableVecTblLabel	  tycon     = TyConLabel tycon InfoTblVecTbl
mkStdUpdVecTblLabel	  tycon     = TyConLabel tycon StdUpdVecTbl

mkReturnPtLabel uniq		= CaseLabel uniq CaseReturnPt
mkVecTblLabel   uniq		= CaseLabel uniq CaseVecTbl
mkAltLabel      uniq tag	= CaseLabel uniq (CaseAlt tag)
mkDefaultLabel  uniq 		= CaseLabel uniq CaseDefault

mkAsmTempLabel 			= AsmTempLabel

	-- Some fixed runtime system labels

mkErrorStdEntryLabel		= RtsLabel RtsShouldNeverHappenCode
mkBlackHoleInfoTableLabel	= RtsLabel RtsBlackHoleInfoTbl
\end{code}

\begin{code}
needsCDecl :: CLabel -> Bool	-- False <=> it's pre-declared; don't bother
isReadOnly :: CLabel -> Bool	-- lives in C "text space"
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
\begin{code}
needsCDecl (IdLabel _ _)	       = True
needsCDecl (CaseLabel _ _)	       = False

needsCDecl (TyConLabel _ (StdUpdCode _)) = False
needsCDecl (TyConLabel _ InfoTblVecTbl)  = False
needsCDecl (TyConLabel _ other)          = True

needsCDecl (AsmTempLabel _)            = False
needsCDecl (RtsLabel _)                = False

needsCDecl other		       = True
\end{code}

Whether the labelled thing can be put in C "text space":
\begin{code}
isReadOnly (IdLabel _ InfoTbl)		= True  -- info-tables: yes
isReadOnly (IdLabel _ ConInfoTbl)	= True -- and so on, for other
isReadOnly (IdLabel _ StaticInfoTbl)	= True 
isReadOnly (IdLabel _ PhantomInfoTbl)	= True
isReadOnly (IdLabel _ (VapInfoTbl _))	= True
isReadOnly (IdLabel _ other)		= False -- others: pessimistically, no

isReadOnly (TyConLabel _ _)    = True
isReadOnly (CaseLabel _ _)     = True
isReadOnly (AsmTempLabel _)    = True
isReadOnly (RtsLabel _)        = True
\end{code}

Whether the label is an assembler temporary:
\begin{code}
isAsmTemp (AsmTempLabel _) = True
isAsmTemp _ 	    	   = False
\end{code}

C ``static'' or not...
From the point of view of the code generator, a name is
externally visible if it should be given put in the .o file's 
symbol table; that is, made static.

\begin{code}
externallyVisibleCLabel (TyConLabel tc _) = True
externallyVisibleCLabel (CaseLabel _ _)	  = False
externallyVisibleCLabel (AsmTempLabel _)  = False
externallyVisibleCLabel (RtsLabel _)	  = True
externallyVisibleCLabel (IdLabel (CLabelId id) _) = externallyVisibleId id
\end{code}

OLD?: These GRAN functions are needed for spitting out GRAN_FETCH() at the
right places. It is used to detect when the abstractC statement of an
CCodeBlock actually contains the code for a slow entry point.  -- HWL

We need at least @Eq@ for @CLabels@, because we want to avoid
duplicate declarations in generating C (see @labelSeenTE@ in
@PprAbsC@).

\begin{code}
-- specialised for PprAsm: saves lots of arg passing in NCG
#if ! OMIT_NATIVE_CODEGEN
pprCLabel_asm = pprCLabel
#endif

pprCLabel :: CLabel -> SDoc

#if ! OMIT_NATIVE_CODEGEN
pprCLabel (AsmTempLabel u)
  = text (fmtAsmLbl (showUnique u))
#endif

pprCLabel lbl = 
#if ! OMIT_NATIVE_CODEGEN
    getPprStyle $ \ sty ->
    if asmStyle sty && underscorePrefix then
       pp_cSEP <> pprCLbl lbl
    else
#endif
       pprCLbl lbl


pprCLbl (TyConLabel tc UnvecConUpdCode)
  = hcat [ptext SLIT("ret"), pp_cSEP, ppr_tycon tc,
	       pp_cSEP, ptext SLIT("upd")]

pprCLbl (TyConLabel tc (VecConUpdCode tag))
  = hcat [ptext SLIT("ret"), pp_cSEP, ppr_tycon tc, pp_cSEP,
		     int tag, pp_cSEP, ptext SLIT("upd")]

pprCLbl (TyConLabel tc (StdUpdCode tag))
  = case (ctrlReturnConvAlg tc) of
	UnvectoredReturn _ -> ptext SLIT("IndUpdRetDir")
    	VectoredReturn _ -> (<>) (ptext SLIT("IndUpdRetV")) (int (tag - fIRST_TAG))

pprCLbl (TyConLabel tc InfoTblVecTbl)
  = hcat [ppr_tycon tc, pp_cSEP, ptext SLIT("itblvtbl")]

pprCLbl (TyConLabel tc StdUpdVecTbl)
  = hcat [ptext SLIT("vtbl"), pp_cSEP, ppr_tycon tc,
    	       pp_cSEP, ptext SLIT("upd")]

pprCLbl (CaseLabel u CaseReturnPt)
  = hcat [ptext SLIT("ret"), pp_cSEP, ppr_u u]
pprCLbl (CaseLabel u CaseVecTbl)
  = hcat [ptext SLIT("vtbl"), pp_cSEP, ppr_u u]
pprCLbl (CaseLabel u (CaseAlt tag))
  = hcat [ptext SLIT("djn"), pp_cSEP, ppr_u u, pp_cSEP, int tag]
pprCLbl (CaseLabel u CaseDefault)
  = hcat [ptext SLIT("djn"), pp_cSEP, ppr_u u]

pprCLbl (RtsLabel RtsShouldNeverHappenCode) = ptext SLIT("StdErrorCode")

pprCLbl (RtsLabel RtsBlackHoleInfoTbl) = ptext SLIT("BH_UPD_info")

pprCLbl (RtsLabel (RtsSelectorInfoTbl upd_reqd offset))
  = hcat [ptext SLIT("__sel_info_"), text (show offset),
		ptext (if upd_reqd then SLIT("upd") else SLIT("noupd")),
		ptext SLIT("__")]

pprCLbl (RtsLabel (RtsSelectorEntry upd_reqd offset))
  = hcat [ptext SLIT("__sel_entry_"), text (show offset),
		ptext (if upd_reqd then SLIT("upd") else SLIT("noupd")),
		ptext SLIT("__")]

pprCLbl (IdLabel (CLabelId id) flavor)
  = ppr id <> ppFlavor flavor


ppr_u u = pprUnique u

ppr_tycon :: TyCon -> SDoc
ppr_tycon tc = ppr tc
{- 
  = let
	str = showTyCon tc
    in
    --pprTrace "ppr_tycon:" (text str) $
    text str
-}

ppFlavor :: IdLabelInfo -> SDoc

ppFlavor x = (<>) pp_cSEP
	     	      (case x of
		       Closure	    	-> ptext SLIT("closure")
		       InfoTbl	    	-> ptext SLIT("info")
		       EntryStd	    	-> ptext SLIT("entry")
		       EntryFast arity	-> --false:ASSERT (arity > 0)
					   (<>) (ptext SLIT("fast")) (int arity)
		       StaticClosure   	-> ptext SLIT("static_closure")
		       ConEntry	    	-> ptext SLIT("con_entry")
		       ConInfoTbl    	-> ptext SLIT("con_info")
		       StaticConEntry  	-> ptext SLIT("static_entry")
		       StaticInfoTbl 	-> ptext SLIT("static_info")
		       PhantomInfoTbl 	-> ptext SLIT("inregs_info")
		       VapInfoTbl True  -> ptext SLIT("vap_info")
		       VapInfoTbl False -> ptext SLIT("vap_noupd_info")
		       VapEntry True    -> ptext SLIT("vap_entry")
		       VapEntry False   -> ptext SLIT("vap_noupd_entry")
		       RednCounts	-> ptext SLIT("ct")
		      )
\end{code}
