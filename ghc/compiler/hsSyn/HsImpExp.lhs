%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsImpExp]{Abstract syntax: imports, exports, interfaces}

\begin{code}
module HsImpExp where

#include "HsVersions.h"

import BasicTypes	( Module, IfaceFlavour(..) )
import Name		( NamedThing )
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
  = ImportDecl	  Module			-- module name
		  Bool				-- True => qualified
		  IfaceFlavour			-- True => source imported module 
						--    (current interpretation: ignore ufolding info)
		  (Maybe Module)		-- as Module
		  (Maybe (Bool, [IE name]))	-- (True => hiding, names)
		  SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (ImportDecl name) where
    ppr (ImportDecl mod qual as_source as spec _)
      = hang (hsep [ptext SLIT("import"), pp_src as_source, 
                    pp_qual qual, ptext mod, pp_as as])
	     4 (pp_spec spec)
      where
	pp_src HiFile     = empty
	pp_src HiBootFile = ptext SLIT("{-# SOURCE #-}")

	pp_qual False   = empty
	pp_qual True	= ptext SLIT("qualified")

	pp_as Nothing   = empty
	pp_as (Just a)  = ptext SLIT("as ") <+> ptext a

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
    ppr (IEVar	        var)	= ppr var
    ppr (IEThingAbs	thing)	= ppr thing
    ppr (IEThingAll	thing)	= hcat [ppr thing, text "(..)"]
    ppr (IEThingWith thing withs)
	= ppr thing <> parens (fsep (punctuate comma (map ppr withs)))
    ppr (IEModuleContents mod)
	= ptext SLIT("module") <+> ptext mod
\end{code}

