%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsImpExp]{Abstract syntax: imports, exports, interfaces}

\begin{code}
#include "HsVersions.h"

module HsImpExp where

import Ubiq{-uitous-}

-- friends:
import HsDecls		( FixityDecl, TyDecl, ClassDecl, InstDecl )
import HsBinds		( Sig )

-- others:
import Outputable
import PprStyle		( PprStyle(..) )
import Pretty
import SrcLoc		( SrcLoc{-instances-} )
\end{code}

%************************************************************************
%*									*
\subsection{Import and export declaration lists}
%*									*
%************************************************************************

One per \tr{import} declaration in a module.
\begin{code}
data ImportedInterface tyvar uvar name pat
  = ImportMod	  (Interface tyvar uvar name pat)
		  Bool				-- qualified?
		  (Maybe FAST_STRING)		-- as Modid
		  (Maybe (Bool, [IE name]))	-- (hiding?, names)
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	   => Outputable (ImportedInterface tyvar uvar name pat) where

    ppr sty (ImportMod iface qual as spec)
      = ppAbove (ppHang (ppCat [ppStr "import", pp_qual qual, ppr PprForUser iface, pp_as as])
	              4 (pp_spec spec))
		(case sty of {PprForUser -> ppNil; _ -> ppr sty iface})
      where
	pp_qual False   = ppNil
	pp_qual True	= ppStr "qualified"

	pp_as Nothing   = ppNil
	pp_as (Just a)  = ppCat [ppStr "as", ppPStr a]

	pp_spec Nothing = ppNil
	pp_spec (Just (False, spec))
			= ppBesides [ppStr "(", interpp'SP sty spec, ppStr ")"]
	pp_spec (Just (True, spec))
			= ppBesides [ppStr "hiding (", interpp'SP sty spec, ppStr ")"]

\end{code}

%************************************************************************
%*									*
\subsection{Imported and exported entities}
%*									*
%************************************************************************
\begin{code}
data IE name
  = IEVar		name
  | IEThingAbs          name		-- Constructor/Type/Class (can't tell)
  | IEThingAll          name		-- Class/Type plus all methods/constructors
  | IEThingWith		name [name]	-- Class/Type plus some methods/constructors
  | IEModuleContents    FAST_STRING	-- (Export Only)
\end{code}

\begin{code}
instance (Outputable name) => Outputable (IE name) where
    ppr sty (IEVar	var)	= ppr sty var
    ppr sty (IEThingAbs	thing)	= ppr sty thing
    ppr sty (IEThingAll	thing)
	= ppBesides [ppr sty thing, ppStr "(..)"]
    ppr sty (IEThingWith thing withs)
	= ppBesides [ppr sty thing, ppLparen, ppInterleave ppComma (map (ppr sty) withs), ppRparen]
    ppr sty (IEModuleContents mod)
	= ppBeside (ppPStr SLIT("module ")) (ppPStr mod)
\end{code}

%************************************************************************
%*									*
\subsection{Interfaces}
%*									*
%************************************************************************

\begin{code}
data Interface tyvar uvar name pat
  = Interface	FAST_STRING			-- module name
		[IfaceImportDecl name]
		[FixityDecl name]
		[TyDecl name]			-- data decls may have no constructors
		[ClassDecl tyvar uvar name pat]	-- without default methods
		[InstDecl  tyvar uvar name pat]	-- without method defns
		[Sig name]
		SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	     => Outputable (Interface tyvar uvar name pat) where

    ppr PprForUser (Interface name _ _ _ _ _ _ _) = ppPStr name

    ppr sty (Interface name iimpdecls fixities tydecls classdecls instdecls sigs anns)
      = ppAboves [ppStr "{-",
		  ifPprShowAll sty (ppr sty anns),
		  ppCat [ppStr "interface", ppPStr name, ppStr "where"],
		  ppNest 4 (ppAboves [
		      pp_nonnull iimpdecls,
		      pp_nonnull fixities,
		      pp_nonnull tydecls,
		      pp_nonnull classdecls,
		      pp_nonnull instdecls,
		      pp_nonnull sigs]),
		  ppStr "-}"]
      where
	pp_nonnull [] = ppNil
	pp_nonnull xs = ppAboves (map (ppr sty) xs)
\end{code}

\begin{code}
data IfaceImportDecl name
  = IfaceImportDecl FAST_STRING	    -- module we're being told about
		    [IE name]	    -- things we're being told about
		    SrcLoc
\end{code}

\begin{code}
instance Outputable name => Outputable (IfaceImportDecl name) where

    ppr sty (IfaceImportDecl mod names src_loc)
      = ppHang (ppCat [ppPStr SLIT("import"), ppPStr mod, ppLparen])
	     4 (ppSep [ppCat [interpp'SP sty names, ppRparen]])
\end{code}
