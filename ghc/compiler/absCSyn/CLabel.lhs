%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CLabel]{@CLabel@: Information to make C Labels}

\begin{code}
#include "HsVersions.h"

module CLabel (
	CLabel,	-- abstract type

	mkClosureLabel,
	mkInfoTableLabel,
	mkStdEntryLabel,
	mkFastEntryLabel,
	mkConEntryLabel,
	mkStaticConEntryLabel,
	mkRednCountsLabel,
	mkPhantomInfoTableLabel,
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

#ifdef GRAN
	, isSlowEntryCCodeBlock
#endif
    ) where

import Ubiq{-uitous-}
import AbsCLoop		( CtrlReturnConvention(..),
			  ctrlReturnConvAlg
			)
#if ! OMIT_NATIVE_CODEGEN
import NcgLoop		( underscorePrefix, fmtAsmLbl )
#endif

import CStrings		( pp_cSEP )
import Id		( externallyVisibleId, cmpId_withSpecDataCon,
			  isDataCon, isDictFunId,
			  isConstMethodId_maybe,
			  isDefaultMethodId_maybe,
			  isSuperDictSelId_maybe, fIRST_TAG,
			  ConTag(..), GenId{-instance Outputable-}
			)
import Maybes		( maybeToBool )
import PprStyle		( PprStyle(..) )
import PprType		( showTyCon, GenType{-instance Outputable-} )
import Pretty		( prettyToUn )
import TyCon		( TyCon{-instance Eq-} )
import Unique		( showUnique, pprUnique, Unique{-instance Eq-} )
import Unpretty		-- NOTE!! ********************
import Util		( assertPanic )
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
    CLabelId a == CLabelId b = case cmpId_withSpecDataCon a b of { EQ_ -> True;  _ -> False }
    CLabelId a /= CLabelId b = case cmpId_withSpecDataCon a b of { EQ_ -> False; _ -> True  }

instance Ord CLabelId where
    CLabelId a <= CLabelId b = case cmpId_withSpecDataCon a b
	 of { LT_ -> True;  EQ_ -> True;  GT__ -> False }
    CLabelId a <  CLabelId b = case cmpId_withSpecDataCon a b
	 of { LT_ -> True;  EQ_ -> False; GT__ -> False }
    CLabelId a >= CLabelId b = case cmpId_withSpecDataCon a b
	 of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    CLabelId a >  CLabelId b = case cmpId_withSpecDataCon a b
	 of { LT_ -> False; EQ_ -> False; GT__ -> True  }
    _tagCmp (CLabelId a) (CLabelId b) = case cmpId_withSpecDataCon a b
	 of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
\end{code}

\begin{code}
data IdLabelInfo
  = Closure		-- Label for (static???) closure

  | InfoTbl		-- Info table for a closure; always read-only

  | EntryStd		-- Thunk, or "slow", code entry point (requires arg satis check)
  | EntryFast Int	-- entry pt when no arg satisfaction chk needed;
			-- Int is the arity of the function (to be
			-- encoded into the name)

  | ConEntry		-- the only kind of entry pt for constructors
  | StaticConEntry  	-- static constructor entry point

  | StaticInfoTbl   	-- corresponding info table

  | PhantomInfoTbl  	-- for phantom constructors that only exist in regs

  | VapInfoTbl Bool	-- True <=> the update-reqd version; False <=> the no-update-reqd version
  | VapEntry Bool

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
mkClosureLabel	      	id 		= IdLabel (CLabelId id) Closure
mkInfoTableLabel      	id 		= IdLabel (CLabelId id) InfoTbl
mkStdEntryLabel	      	id 		= IdLabel (CLabelId id) EntryStd
mkFastEntryLabel      	id arity	= ASSERT(arity > 0)
					  IdLabel (CLabelId id) (EntryFast arity)
mkConEntryLabel	      	id		= IdLabel (CLabelId id) ConEntry
mkStaticConEntryLabel 	id		= IdLabel (CLabelId id) StaticConEntry
mkRednCountsLabel     	id		= IdLabel (CLabelId id) RednCounts
mkPhantomInfoTableLabel id		= IdLabel (CLabelId id) PhantomInfoTbl
mkStaticInfoTableLabel  id		= IdLabel (CLabelId id) StaticInfoTbl
mkVapEntryLabel		id upd_flag	= IdLabel (CLabelId id) (VapEntry upd_flag)
mkVapInfoTableLabel	id upd_flag	= IdLabel (CLabelId id) (VapInfoTbl upd_flag)

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
isReadOnly (IdLabel _ InfoTbl)	       = True  -- info-tables: yes
isReadOnly (IdLabel _ StaticInfoTbl)   = True  -- and so on, for other
isReadOnly (IdLabel _ PhantomInfoTbl)  = True
isReadOnly (IdLabel _ (VapInfoTbl _))  = True
isReadOnly (IdLabel _ other)	       = False -- others: pessimistically, no

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
\begin{code}
externallyVisibleCLabel (TyConLabel tc _) = True
externallyVisibleCLabel (CaseLabel _ _)	  = False
externallyVisibleCLabel (AsmTempLabel _)  = False
externallyVisibleCLabel (RtsLabel _)	  = True
externallyVisibleCLabel (IdLabel (CLabelId id) _)
  | isDataCon id 	  = True
  | is_ConstMethodId id   = True  -- These are here to ensure splitting works
  | isDictFunId id 	  = True  -- when these values have not been exported
  | is_DefaultMethodId id = True
  | is_SuperDictSelId id  = True
  | otherwise    	  = externallyVisibleId id
  where
    is_ConstMethodId   id = maybeToBool (isConstMethodId_maybe   id)
    is_DefaultMethodId id = maybeToBool (isDefaultMethodId_maybe id)
    is_SuperDictSelId  id = maybeToBool (isSuperDictSelId_maybe  id)
\end{code}

These GRAN functions are needed for spitting out GRAN_FETCH() at the
right places. It is used to detect when the abstractC statement of an
CCodeBlock actually contains the code for a slow entry point.  -- HWL

\begin{code}
#ifdef GRAN

isSlowEntryCCodeBlock :: CLabel -> Bool
isSlowEntryCCodeBlock _ = False
-- Worth keeping?  ToDo (WDP)

#endif {-GRAN-}
\end{code}

We need at least @Eq@ for @CLabels@, because we want to avoid
duplicate declarations in generating C (see @labelSeenTE@ in
@PprAbsC@).

\begin{code}
-- specialised for PprAsm: saves lots of arg passing in NCG
#if ! OMIT_NATIVE_CODEGEN
pprCLabel_asm = pprCLabel (PprForAsm underscorePrefix fmtAsmLbl)
#endif

pprCLabel :: PprStyle -> CLabel -> Unpretty

pprCLabel (PprForAsm _ fmtAsmLbl) (AsmTempLabel u)
  = uppStr (fmtAsmLbl (_UNPK_ (showUnique u)))

pprCLabel (PprForAsm prepend_cSEP _) lbl
  = if prepend_cSEP
    then uppBeside pp_cSEP prLbl
    else prLbl
  where
    prLbl = pprCLabel PprForC lbl

pprCLabel sty (TyConLabel tc UnvecConUpdCode)
  = uppBesides [uppPStr SLIT("ret"), pp_cSEP, uppStr (showTyCon sty tc),
	       pp_cSEP, uppPStr SLIT("upd")]

pprCLabel sty (TyConLabel tc (VecConUpdCode tag))
  = uppBesides [uppPStr SLIT("ret"), pp_cSEP, uppStr (showTyCon sty tc), pp_cSEP,
		     uppInt tag, pp_cSEP, uppPStr SLIT("upd")]

pprCLabel sty (TyConLabel tc (StdUpdCode tag))
  = case (ctrlReturnConvAlg tc) of
	UnvectoredReturn _ -> uppPStr SLIT("IndUpdRetDir")
    	VectoredReturn _ -> uppBeside (uppPStr SLIT("IndUpdRetV")) (uppInt (tag - fIRST_TAG))

pprCLabel sty (TyConLabel tc InfoTblVecTbl)
  = uppBesides [uppStr (showTyCon sty tc), pp_cSEP, uppPStr SLIT("itblvtbl")]

pprCLabel sty (TyConLabel tc StdUpdVecTbl)
  = uppBesides [uppPStr SLIT("vtbl"), pp_cSEP, uppStr (showTyCon sty tc),
    	       pp_cSEP, uppPStr SLIT("upd")]

pprCLabel sty (CaseLabel u CaseReturnPt)
  = uppBesides [uppPStr SLIT("ret"), pp_cSEP, ppr_u u]
pprCLabel sty (CaseLabel u CaseVecTbl)
  = uppBesides [uppPStr SLIT("vtbl"), pp_cSEP, ppr_u u]
pprCLabel sty (CaseLabel u (CaseAlt tag))
  = uppBesides [uppPStr SLIT("djn"), pp_cSEP, ppr_u u, pp_cSEP, uppInt tag]
pprCLabel sty (CaseLabel u CaseDefault)
  = uppBesides [uppPStr SLIT("djn"), pp_cSEP, ppr_u u]

pprCLabel sty (RtsLabel RtsShouldNeverHappenCode) = uppPStr SLIT("StdErrorCode")

pprCLabel sty (RtsLabel RtsBlackHoleInfoTbl) = uppPStr SLIT("BH_UPD_info")

pprCLabel sty (RtsLabel (RtsSelectorInfoTbl upd_reqd offset))
  = uppBesides [uppPStr SLIT("__sel_info_"), uppStr (show offset),
		uppStr (if upd_reqd then "upd" else "noupd"),
		uppPStr SLIT("__")]

pprCLabel sty (RtsLabel (RtsSelectorEntry upd_reqd offset))
  = uppBesides [uppPStr SLIT("__sel_entry_"), uppStr (show offset),
		uppStr (if upd_reqd then "upd" else "noupd"),
		uppPStr SLIT("__")]

pprCLabel sty (IdLabel (CLabelId id) flavor)
  = uppBeside (prettyToUn (ppr sty id)) (ppFlavor flavor)

ppr_u u = prettyToUn (pprUnique u)

ppFlavor :: IdLabelInfo -> Unpretty

ppFlavor x = uppBeside pp_cSEP
	     	      (case x of
		       Closure	    	-> uppPStr SLIT("closure")
		       InfoTbl	    	-> uppPStr SLIT("info")
		       EntryStd	    	-> uppPStr SLIT("entry")
		       EntryFast arity	-> --false:ASSERT (arity > 0)
					   uppBeside (uppPStr SLIT("fast")) (uppInt arity)
		       ConEntry	    	-> uppPStr SLIT("entry")
		       StaticConEntry  	-> uppPStr SLIT("static_entry")
		       StaticInfoTbl 	-> uppPStr SLIT("static_info")
		       PhantomInfoTbl 	-> uppPStr SLIT("inregs_info")
		       VapInfoTbl True  -> uppPStr SLIT("vap_info")
		       VapInfoTbl False -> uppPStr SLIT("vap_noupd_info")
		       VapEntry True    -> uppPStr SLIT("vap_entry")
		       VapEntry False   -> uppPStr SLIT("vap_noupd_entry")
		       RednCounts	-> uppPStr SLIT("ct")
		      )
\end{code}
