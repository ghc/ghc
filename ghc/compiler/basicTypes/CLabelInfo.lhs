%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[CLabelInfo]{@CLabelInfo@: Information to make C Labels}

\begin{code}
#include "HsVersions.h"

module CLabelInfo (
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

--UNUSED: mkConUpdCodePtrUnvecLabel,
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
--UNUSED: mkSelectorInfoTableLabel,
--UNUSED: mkSelectorEntryLabel,

#ifdef DPH
	mkLocalLabel, isLocalLabel, isNestableBlockLabel,
	isGlobalDataLabel, isDataLabel, 
	needsApalDecl, isVectorTableLabel, isSlowFastLabelPair,
#endif {- Data Parallel Haskell -}

	needsCDecl, isReadOnly, isAsmTemp, externallyVisibleCLabel,

	cSEP, identToC, modnameToC, stringToC, charToC, charToEasyHaskell,
	pprCLabel,

#ifdef GRAN
	isSlowEntryCCodeBlock,
#endif

	-- and to make the interface self-sufficient...
	Id, TyCon, Unique
    ) where

import AbsUniType	( showTyCon, cmpTyCon, isBigTupleTyCon,
			  TyCon, Unique
			)
import Id		( externallyVisibleId, cmpId_withSpecDataCon,
			  isDataCon, isDictFunId, isConstMethodId_maybe,
			  isClassOpId, isDefaultMethodId_maybe, isSuperDictSelId_maybe,
			  Id, Class, ClassOp, DataCon(..), ConTag(..), fIRST_TAG
#ifdef DPH
			 ,isInventedTopLevId
#endif {- Data Parallel Haskell -}
			)
import Maybes
import Outputable
import Pretty		( ppNil, ppChar, ppStr, ppPStr, ppDouble, ppInt,
			  ppInteger, ppBeside, ppIntersperse, prettyToUn
			)
#ifdef USE_ATTACK_PRAGMAS
import CharSeq
#endif
import Unpretty		-- NOTE!! ********************
import Unique		( cmpUnique, showUnique, pprUnique, Unique )
import Util

#ifdef DPH
import AbsCSyn		( MagicId )
import PprAbsC		( pprMagicId )
#endif {- Data Parallel Haskell -}

-- Sigh...  Shouldn't this file (CLabelInfo) live in codeGen?
import CgRetConv    	( CtrlReturnConvention(..), ctrlReturnConvAlg )

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

#ifdef DPH
  | ALocalLabel     Unique	-- Label within a code block.
		    String
#endif {- Data Parallel Haskell -}
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
#ifdef __GLASGOW_HASKELL__
    _tagCmp (CLabelId a) (CLabelId b) = case cmpId_withSpecDataCon a b
	 of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
#endif
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

--UNUSED:mkConUpdCodePtrUnvecLabel tycon     = TyConLabel tycon UnvecConUpdCode
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
--UNUSED:mkSelectorInfoTableLabel upd_reqd offset = RtsLabel (RtsSelectorInfoTbl upd_reqd offset)
--UNUSED: mkSelectorEntryLabel upd_reqd offset     = RtsLabel (RtsSelectorEntry upd_reqd offset)

#ifdef DPH
mkLocalLabel = ALocalLabel
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
needsCDecl :: CLabel -> Bool	-- False <=> it's pre-declared; don't bother
isReadOnly :: CLabel -> Bool	-- lives in C "text space"
isAsmTemp  :: CLabel -> Bool    -- is a local temporary for native code generation
externallyVisibleCLabel :: CLabel -> Bool -- not C "static"
\end{code}

@needsCDecl@ is @True@ unless the thing is a deeply-@PreludeCore@-ish
object.  {\em Also:} No need to spit out labels for things generated
by the flattener (in @AbsCFuns@)---it is careful to ensure references
to them are always backwards.  These are return-point and vector-table
labels.

Declarations for (non-prelude) @Id@-based things are needed because of
mutual recursion.
\begin{code}
needsCDecl (IdLabel _ _)	       = True -- OLD: not (fromPreludeCore id)
needsCDecl (CaseLabel _ _)	       = False

needsCDecl (TyConLabel _ (StdUpdCode _)) = False
needsCDecl (TyConLabel _ InfoTblVecTbl)  = False
needsCDecl (TyConLabel _ other)          = True

needsCDecl (AsmTempLabel _)            = False
needsCDecl (RtsLabel _)                = False

#ifdef DPH
needsCDecl (ALocalLabel _ _)           = panic "needsCDecl: Shouldn't call"
#endif {- Data Parallel Haskell -}

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

#ifdef DPH
isReadOnly (ALocalLabel _ _)   = panic "isReadOnly: Shouldn't call"
#endif {- Data Parallel Haskell -}
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

#ifndef DPH

externallyVisibleCLabel (IdLabel (CLabelId id) _)
  | isDataCon id 	  = True
  | is_ConstMethodId id   = True  -- These are here to ensure splitting works
  | isDictFunId id 	  = True  -- when these values have not been exported
  | isClassOpId id	  = True
  | is_DefaultMethodId id = True
  | is_SuperDictSelId id  = True
  | otherwise    	  = externallyVisibleId id
  where
    is_ConstMethodId id   = maybeToBool (isConstMethodId_maybe id)
    is_DefaultMethodId id = maybeToBool (isDefaultMethodId_maybe id)
    is_SuperDictSelId id  = maybeToBool (isSuperDictSelId_maybe id)
#else
-- DPH pays a big price for exported identifiers. For example with
-- a statically allocated closure, if it is local to a file it will
-- only take up 1 word of storage; exported closures have to go
-- in a data section of their own, which gets padded out to a plane size---
-- on the DAP510 this is 32 words, DAP610 128 words, DAP710 512 words :-(
-- NOTE:16/07/93 Used isInvented (these worker things are globally visible).
-- Local labels (i.e ones within a code block) are not visible outside
-- a file.

externallyVisibleCLabel (IdLabel (CLabelId id) _) = isInventedTopLevId id || isExported id
externallyVisibleCLabel (ALocalLabel _ _)	  = False
#endif {- Data Parallel Haskell -}
\end{code}

@isLocalLabel@ determines if a label is local to a block---a different
machine code jump is generated.

Note(hack after 0.16): Blocks with direct entry points can appear
		       within blocks labelled with a direct entry
		       point --- something todo with let-no-escape.
		       Fast entry blocks arent nestable, however we
		       special case fall through.
\begin{code}
#ifdef DPH
isLocalLabel::CLabel -> Bool
isLocalLabel (ALocalLabel _ _) = True
isLocalLabel _		       = False

isNestableBlockLabel (ALocalLabel _ _)          = True
isNestableBlockLabel (IdLabel _ EntryStd)       = True
isNestableBlockLabel (IdLabel _ ConEntry)       = True
isNestableBlockLabel (IdLabel _ StaticConEntry) = True
isNestableBlockLabel _                          = False

isSlowFastLabelPair :: CLabel -> CLabel -> Bool
isSlowFastLabelPair (IdLabel clid EntryStd) (IdLabel clid' (EntryFast _)) = clid == clid'
isSlowFastLabelPair _                       _			          = False
#endif {- Data Parallel Haskell -}
\end{code}

We need to determine if a label represents a code entity, an ordinary 
data entity, or a special global data entity (placed at an absolute
address by the runtime system that ensures fast loading of variable
contents---global ``registers'' such as SuA are placed here as well)
(different instructions are used in the DAP machine code). 
\begin{code}
#ifdef DPH
isGlobalDataLabel _ = False

isDataLabel :: CLabel -> Bool
isDataLabel (IdLabel _ Closure) = True
isDataLabel _			= False

isVectorTableLabel :: CLabel -> Bool
isVectorTableLabel (VecTblCLabel _)   = True
isVectorTableLabel _                  = False
#endif {- Data Parallel Haskell -}
\end{code}

Sort of like the needsCDecl, we need to stop the assembler from complaining
about various data sections :-)
\begin{code}
#ifdef DPH
needsApalDecl :: CLabel -> Bool
needsApalDecl (IdLabel (CLabelId id) Closure)  = not (isLocallyDefined id)
needsApalDecl _		                       = False
#endif {- Data Parallel Haskell -}
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
pprCLabel :: PprStyle -> CLabel -> Unpretty

pprCLabel (PprForAsm _ _ fmtAsmLbl) (AsmTempLabel u) 
  = uppStr (fmtAsmLbl (_UNPK_ (showUnique u)))

pprCLabel (PprForAsm sw_chker prepend_cSEP _) lbl
  = if prepend_cSEP
    then uppBeside pp_cSEP prLbl
    else prLbl
  where
    prLbl = pprCLabel (PprForC sw_chker) lbl

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

#ifdef DPH
pprCLabel sty (ALocalLabel u str) = uppBeside (uppStr str) (ppr_u u)
#endif {- Data Parallel Haskell -}

ppr_u u = prettyToUn (pprUnique u)

ppFlavor :: IdLabelInfo -> Unpretty
#ifndef DPH
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
#else
ppFlavor x = uppStr (case x of
		       Closure		-> "_clos"
		       InfoTbl		-> "_info"
		       EntryStd		-> "_entry"
		       EntryFast arity  -> "_fast" ++ show arity
		       ConEntry		-> "_entry"
		       StaticConEntry  	-> "_statentr"
		       StaticInfoTbl 	-> "_statinfo"
		       PhantomInfoTbl 	-> "_irinfo"
		       -- ToDo: add more
		    )
#endif {- Data Parallel Haskell -}

\end{code}

ToDo:
use Z as escape char
\begin{verbatim}
_	main separator

orig		becomes
****		*******
_		Zu
'		Zq (etc for ops ??)
<funny char>	Z[hex-digit][hex-digit]
Prelude<x>	ZP<x>
<std class>	ZC<?>
<std tycon>	ZT<?>
\end{verbatim}

\begin{code}
cSEP = SLIT("_")	-- official C separator
pp_cSEP = uppChar '_'

identToC    :: FAST_STRING -> Pretty
modnameToC  :: FAST_STRING -> FAST_STRING
stringToC   :: String -> String
charToC, charToEasyHaskell :: Char -> String

-- stringToC: the hassle is what to do w/ strings like "ESC 0"...

stringToC ""  = "" 
stringToC [c] = charToC c
stringToC (c:cs)
    -- if we have something "octifiable" in "c", we'd better "octify"
    -- the rest of the string, too.
  = if (c < ' ' || c > '~')
    then (charToC c) ++ (concat (map char_to_C cs))
    else (charToC c) ++ (stringToC cs)
  where
    char_to_C c | c == '\n' = "\\n"	-- use C escapes when we can
		| c == '\a' = "\\a"
		| c == '\b' = "\\b"	-- ToDo: chk some of these...
		| c == '\r' = "\\r"
		| c == '\t' = "\\t"
		| c == '\f' = "\\f"
		| c == '\v' = "\\v"
		| otherwise = '\\' : (octify (ord c))

-- OLD?: stringToC str = concat (map charToC str)

charToC c = if (c >= ' ' && c <= '~')	-- non-portable...
	    then case c of
		  '\'' -> "\\'"
		  '\\' -> "\\\\"
		  '"'  -> "\\\""
		  '\n' -> "\\n"
		  '\a' -> "\\a"
		  '\b' -> "\\b"
		  '\r' -> "\\r"
		  '\t' -> "\\t"
		  '\f' -> "\\f"
		  '\v' -> "\\v"
		  _    -> [c]
	    else '\\' : (octify (ord c))

-- really: charToSimpleHaskell

charToEasyHaskell c
  = if (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    then [c]
    else case c of
	  _    -> '\\' : 'o' : (octify (ord c))

octify :: Int -> String
octify n
  = if n < 8 then
	[chr (n + ord '0')]
    else 
	octify (n `quot` 8) ++ [chr (n `rem` 8 + ord '0')]

identToC ps
  = let
	str = _UNPK_ ps
    in
    ppBeside
	(case str of
	   's':'t':'d':_ -> -- avoid "stdin", "stdout", and "stderr"...
			    ppChar 'Z'
	   _	         -> ppNil)

	(if (all isAlphanum str) -- we gamble that this test will succeed...
	 then ppPStr ps
	 else ppIntersperse ppNil (map char_to_c str))
  where
    char_to_c 'Z'  = ppPStr SLIT("ZZ")
    char_to_c '&'  = ppPStr SLIT("Za")
    char_to_c '|'  = ppPStr SLIT("Zb")
    char_to_c ':'  = ppPStr SLIT("Zc")
    char_to_c '/'  = ppPStr SLIT("Zd")
    char_to_c '='  = ppPStr SLIT("Ze")
    char_to_c '>'  = ppPStr SLIT("Zg")
    char_to_c '#'  = ppPStr SLIT("Zh")
    char_to_c '<'  = ppPStr SLIT("Zl")
    char_to_c '-'  = ppPStr SLIT("Zm")
    char_to_c '!'  = ppPStr SLIT("Zn")
    char_to_c '.'  = ppPStr SLIT("Zo")
    char_to_c '+'  = ppPStr SLIT("Zp")
    char_to_c '\'' = ppPStr SLIT("Zq")
    char_to_c '*'  = ppPStr SLIT("Zt")
    char_to_c '_'  = ppPStr SLIT("Zu")

    char_to_c c    = if isAlphanum c
		     then ppChar c
		     else ppBeside (ppChar 'Z') (ppInt (ord c))
\end{code}

For \tr{modnameToC}, we really only have to worry about \tr{'}s (quote
chars) in the name.  Rare.
\begin{code}
modnameToC ps
  = let
	str = _UNPK_ ps
    in
    if not (any quote_here str) then
	ps
    else
	_PK_ (concat (map char_to_c str))
  where
    quote_here '\'' = True
    quote_here _    = False

    char_to_c c
      = if isAlphanum c then [c] else 'Z' : (show (ord c))
\end{code}
