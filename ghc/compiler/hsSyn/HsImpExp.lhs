%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsImpExp]{Abstract syntax: imports, exports, interfaces}

\begin{code}
#include "HsVersions.h"

module HsImpExp where

IMP_Ubiq()

import Outputable
import Pretty
import SrcLoc		( SrcLoc )
#if __GLASGOW_HASKELL__ >= 202
import Name
#endif
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
		  Bool				-- True => source imported module 
						--    (current interpretation: ignore ufolding info)
		  (Maybe Module)		-- as Module
		  (Maybe (Bool, [IE name]))	-- (True => hiding, names)
		  SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (ImportDecl name) where
    ppr sty (ImportDecl mod qual as_source as spec _)
      = hang (hsep [ptext SLIT("import"), pp_src as_source, 
                    pp_qual qual, ptext mod, pp_as as])
	     4 (pp_spec spec)
      where
	pp_src False   = empty
	pp_src True	= ptext SLIT("{-# SOURCE #-}")

	pp_qual False   = empty
	pp_qual True	= ptext SLIT("qualified")

	pp_as Nothing   = empty
	pp_as (Just a)  = (<>) (ptext SLIT("as ")) (ptext a)

	pp_spec Nothing = empty
	pp_spec (Just (False, spec))
			= parens (interpp'SP sty spec)
	pp_spec (Just (True, spec))
			= (<>) (ptext SLIT("hiding ")) (parens (interpp'SP sty spec))
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
    ppr sty (IEVar	var)	= ppr sty var
    ppr sty (IEThingAbs	thing)	= ppr sty thing
    ppr sty (IEThingAll	thing)
	= hcat [ppr sty thing, text "(..)"]
    ppr sty (IEThingWith thing withs)
	= (<>) (ppr sty thing)
	    (parens (fsep (punctuate comma (map (ppr sty) withs))))
    ppr sty (IEModuleContents mod)
	= (<>) (ptext SLIT("module ")) (ptext mod)
\end{code}

