%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsImpExp]{Abstract syntax: imports, exports, interfaces}

\begin{code}
module HsImpExp where

#include "HsVersions.h"

import Module		( ModuleName, WhereFrom, pprModuleName )
import Outputable
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
  = ImportDecl	  ModuleName			-- module name
		  WhereFrom
		  Bool				-- True => qualified
		  (Maybe ModuleName)		-- as Module
		  (Maybe (Bool, [IE name]))	-- (True => hiding, names)
		  SrcLoc
\end{code}

\begin{code}
instance (Outputable name) => Outputable (ImportDecl name) where
    ppr (ImportDecl mod from qual as spec _)
      = hang (hsep [ptext SLIT("import"), ppr from, 
                    pp_qual qual, pprModuleName mod, pp_as as])
	     4 (pp_spec spec)
      where
	pp_qual False   = empty
	pp_qual True	= ptext SLIT("qualified")

	pp_as Nothing   = empty
	pp_as (Just a)  = ptext SLIT("as ") <+> pprModuleName a

	pp_spec Nothing = empty
	pp_spec (Just (False, spec))
			= parens (interpp'SP spec)
	pp_spec (Just (True, spec))
			= ptext SLIT("hiding") <+> parens (interpp'SP spec)
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
  | IEModuleContents    ModuleName	-- (Export Only)
\end{code}

\begin{code}
ieName :: IE name -> name
ieName (IEVar n) 	 = n
ieName (IEThingAbs  n)   = n
ieName (IEThingWith n _) = n
ieName (IEThingAll  n)   = n

ieNames :: IE a -> [a]
ieNames (IEVar            n   ) = [n]
ieNames (IEThingAbs       n   ) = [n]
ieNames (IEThingAll       n   ) = [n]
ieNames (IEThingWith      n ns) = n:ns
ieNames (IEModuleContents _   ) = []
\end{code}

\begin{code}
instance (Outputable name) => Outputable (IE name) where
    ppr (IEVar	        var)	= ppr var
    ppr (IEThingAbs	thing)	= ppr thing
    ppr (IEThingAll	thing)	= hcat [ppr thing, text "(..)"]
    ppr (IEThingWith thing withs)
	= ppr thing <> parens (fsep (punctuate comma (map ppr withs)))
    ppr (IEModuleContents mod)
	= ptext SLIT("module") <+> pprModuleName mod
\end{code}

