%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsImpExp]{Abstract syntax: imports, exports, interfaces}

\begin{code}
#include "HsVersions.h"

module HsImpExp where

import Ubiq

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
instance (Outputable name) => Outputable (ImportDecl name) where
    ppr sty (ImportDecl mod qual as spec _)
      = ppHang (ppCat [ppStr "import", pp_qual qual, ppPStr mod, pp_as as])
	     4 (pp_spec spec)
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
  | IEThingAbs          name		-- Class/Type (can't tell)
  | IEThingAll          name		-- Class/Type plus all methods/constructors
  | IEThingWith		name [name]	-- Class/Type plus some methods/constructors
  | IEModuleContents    Module		-- (Export Only)
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
