%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsImpExp]{Abstract syntax: imports, exports, interfaces}

\begin{code}
#include "HsVersions.h"

module HsImpExp where

IMP_Ubiq()

import Name		( pprNonSym )
import Outputable
import PprStyle		( PprStyle(..) )
import Pretty
import SrcLoc		( SrcLoc )
\end{code}

%************************************************************************
%*									*
\subsection{Import and export declaration lists}
%*									*
%************************************************************************

One per \tr{import} declaration in a module.
\begin{code}
data ImportDecl name
  = ImportDecl	  Module			-- module name
		  Bool				-- True => qualified
		  (Maybe Module)		-- as Module
		  (Maybe (Bool, [IE name]))	-- (True => hiding, names)
		  SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (ImportDecl name) where
    ppr sty (ImportDecl mod qual as spec _)
      = ppHang (ppCat [ppPStr SLIT("import"), pp_qual qual, ppPStr mod, pp_as as])
	     4 (pp_spec spec)
      where
	pp_qual False   = ppNil
	pp_qual True	= ppPStr SLIT("qualified")

	pp_as Nothing   = ppNil
	pp_as (Just a)  = ppBeside (ppPStr SLIT("as ")) (ppPStr a)

	pp_spec Nothing = ppNil
	pp_spec (Just (False, spec))
			= ppParens (interpp'SP sty spec)
	pp_spec (Just (True, spec))
			= ppBeside (ppPStr SLIT("hiding ")) (ppParens (interpp'SP sty spec))
\end{code}

%************************************************************************
%*									*
\subsection{Imported and exported entities}
%*									*
%************************************************************************

\begin{code}
data IE name
  = IEVar		name
  | IEThingAbs          name		-- Class/Type (can't tell)
  | IEThingAll          name		-- Class/Type plus all methods/constructors
  | IEThingWith		name [name]	-- Class/Type plus some methods/constructors
  | IEModuleContents    Module		-- (Export Only)
\end{code}

\begin{code}
ieName :: IE name -> name
ieName (IEVar n) 	 = n
ieName (IEThingAbs  n)   = n
ieName (IEThingWith n _) = n
ieName (IEThingAll  n)   = n
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (IE name) where
    ppr sty (IEVar	var)	= pprNonSym sty var
    ppr sty (IEThingAbs	thing)	= ppr sty thing
    ppr sty (IEThingAll	thing)
	= ppBesides [ppr sty thing, ppStr "(..)"]
    ppr sty (IEThingWith thing withs)
	= ppBeside (ppr sty thing)
	    (ppParens (ppInterleave ppComma (map (pprNonSym sty) withs)))
    ppr sty (IEModuleContents mod)
	= ppBeside (ppPStr SLIT("module ")) (ppPStr mod)
\end{code}

