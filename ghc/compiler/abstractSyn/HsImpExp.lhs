%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[HsImpExp]{Abstract syntax: imports, exports, interfaces}

\begin{code}
#include "HsVersions.h"

module HsImpExp where

import FiniteMap
import HsDecls		( FixityDecl, TyDecl, ClassDecl, InstDecl )
import HsBinds		( Sig )
import HsPat		( ProtoNamePat(..), RenamedPat(..), InPat )
import Id		( Id )
import Name             ( Name )
import Outputable
import Pretty
import ProtoName	( ProtoName(..) ) -- .. for pragmas only
import SrcLoc		( SrcLoc )
import Unique           ( Unique )
import Util		-- pragmas only
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyn-ImpExpDecls]{Import and export declaration lists}
%*									*
%************************************************************************

One per \tr{import} declaration in a module.
\begin{code}
data ImportedInterface name pat
  = ImportAll	  (Interface name pat)	-- the contents of the interface
				    	-- (incl module name)
		  [Renaming]

  | ImportSome	  (Interface name pat)
		  [IE]		-- the only things being imported
		  [Renaming]

  | ImportButHide (Interface name pat)
		  [IE]		-- import everything "but hide" these entities
		  [Renaming]
\end{code}

Synonyms:
\begin{code}
type ProtoNameImportedInterface = ImportedInterface ProtoName ProtoNamePat
type RenamedImportedInterface   = ImportedInterface Name      RenamedPat
\end{code}

\begin{code}
instance (NamedThing name, Outputable name,
          NamedThing pat, Outputable pat)
           => Outputable (ImportedInterface name pat) where

    ppr sty (ImportAll iface renamings)
      = ppAbove (ppCat [ppStr "import", ppr sty iface])
		(pprRenamings sty renamings)

    ppr sty (ImportSome iface imports renamings)
      = ppAboves [ppCat [ppStr "import", ppr sty iface],
		  ppNest 8 (ppBesides [ppStr " (", interpp'SP sty imports, ppStr ") "]),
		  pprRenamings sty renamings]

    ppr sty (ImportButHide iface imports renamings)
      = ppAboves [ppCat [ppStr "import", ppr sty iface],
		  ppNest 8 (ppBesides [ppStr "hiding (", interpp'SP sty imports, ppStr ") "]),
		  pprRenamings sty renamings]
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyn-entities]{Imported and exported entities}
%*									*
%************************************************************************
\begin{code}
data IE
  = IEVar               FAST_STRING
  | IEThingAbs          FAST_STRING	-- Constructor/Type/Class (can't tell)
  | IEThingAll          FAST_STRING	-- Class/Type plus all methods/constructors
  | IEConWithCons       FAST_STRING	-- import tycon w/ some cons
			[FAST_STRING]
  | IEClsWithOps        FAST_STRING	-- import tycls w/ some methods
			[FAST_STRING]
  | IEModuleContents    FAST_STRING	-- (Export Only)
\end{code}

\begin{code}
instance Outputable IE where
    ppr sty (IEVar	var)	= ppPStr var
    ppr sty (IEThingAbs	thing)	= ppPStr thing
    ppr sty (IEThingAll	thing)	= ppBesides [ppPStr thing, ppStr "(..)"]
    ppr sty (IEConWithCons tycon datacons)
      = ppBesides [ppPStr tycon, ppLparen, ppInterleave ppComma (map ppPStr datacons), ppRparen]
    ppr sty (IEClsWithOps cls methods)
      = ppBesides [ppPStr cls, ppLparen, ppInterleave ppComma (map ppPStr methods), ppRparen]
    ppr sty (IEModuleContents mod) = ppBesides [ppPStr mod, ppStr ".."]
\end{code}

We want to know what names are exported (the first list of the result)
and what modules are exported (the second list of the result).
\begin{code}
type ImExportListInfo
  = ( FiniteMap FAST_STRING ExportFlag,
			-- Assoc list of im/exported things &
			-- their "export" flags (im/exported
			-- abstractly, concretely, etc.)
			-- Hmm... slight misnomer there (WDP 95/02)
      FiniteSet FAST_STRING )
			-- List of modules to be exported
			-- entirely; NB: *not* everything with
			-- original names in these modules;
			-- but: everything that these modules'
			-- interfaces told us about.
			-- Note: This latter component can
			-- only arise on export lists.

getIEStrings    :: [IE] -> ImExportListInfo
getRawIEStrings :: [IE] -> ([(FAST_STRING, ExportFlag)], [FAST_STRING])
  -- "Raw" gives the raw lists of things; we need this for
  -- checking for duplicates.

getIEStrings exps
  = case (getRawIEStrings exps) of { (pairs, mods) ->
    (listToFM pairs, mkSet mods) }

getRawIEStrings exps
  = foldr do_one ([],[]) exps
  where
    do_one (IEVar n) (prs, mods) 
     = ((n, ExportAll):prs, mods)
    do_one (IEThingAbs n) (prs, mods) 
     = ((n, ExportAbs):prs, mods)
    do_one (IEThingAll n) (prs, mods) 
     = ((n, ExportAll):prs, mods)
    do_one (IEConWithCons n ns) (prs, mods) -- needn't do anything
     = ((n, ExportAll):prs, mods)	    -- with the indiv cons/ops
    do_one (IEClsWithOps n ns) (prs, mods) 
     = ((n, ExportAll):prs, mods)
    do_one (IEModuleContents n) (prs, mods)  
     = (prs, n : mods)
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyn-Renaming]{Renamings}
%*									*
%************************************************************************

\begin{code}
data Renaming = MkRenaming FAST_STRING FAST_STRING
\end{code}

\begin{code}
pprRenamings :: PprStyle -> [Renaming] -> Pretty
pprRenamings sty [] = ppNil
pprRenamings sty rs = ppBesides [ppStr "renaming (", interpp'SP sty rs, ppStr ")"]
\end{code}

\begin{code}
instance Outputable Renaming where
    ppr sty (MkRenaming from too) = ppCat [ppPStr from, ppStr "to", ppPStr too]
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyn-Interface]{Interfaces}
%*									*
%************************************************************************

\begin{code}
data Interface name pat
  = MkInterface FAST_STRING			-- module name
		[IfaceImportDecl]
		[FixityDecl name]	-- none yet (ToDo)
		[TyDecl name]		-- data decls may have no constructors
		[ClassDecl name pat]	-- Without default methods
		[InstDecl  name pat]	-- Without method defns
		[Sig name]
		SrcLoc
\end{code}

\begin{code}
type ProtoNameInterface = Interface ProtoName ProtoNamePat
type RenamedInterface = Interface Name RenamedPat
\end{code}

\begin{code}
instance (NamedThing name, Outputable name,
           NamedThing pat, Outputable pat)
             => Outputable (Interface name pat) where

    ppr PprForUser (MkInterface name _ _ _ _ _ _ _) = ppPStr name

    ppr sty (MkInterface name iimpdecls fixities tydecls classdecls instdecls sigs anns)
      = ppHang (ppBeside (ppPStr name) (ppStr " {-"))
	     4 (ppAboves [
		  ifPprShowAll sty (ppr sty anns),
		  ppCat [ppStr "interface", ppPStr name, ppStr "where"],
		  ppNest 4 (ppAboves [
		      ppr sty iimpdecls,	ppr sty fixities,
		      ppr sty tydecls,	ppr sty classdecls,
		      ppr sty instdecls,  ppr sty sigs]),
		  ppStr "-}"])
\end{code}

\begin{code}
data IfaceImportDecl
  = IfaceImportDecl FAST_STRING	    -- module we're being told about
		    [IE]	    -- things we're being told about
		    [Renaming]	    -- AAYYYYEEEEEEEEEE!!! (help)
		    SrcLoc
\end{code}

\begin{code}
instance Outputable IfaceImportDecl where

    ppr sty (IfaceImportDecl mod names renamings src_loc)
      = ppHang (ppCat [ppStr "import", ppPStr mod, ppLparen])
	     4 (ppSep [ppCat [interpp'SP sty names, ppRparen],
		       pprRenamings sty renamings])
\end{code}


