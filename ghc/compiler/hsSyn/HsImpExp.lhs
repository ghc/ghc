%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsImpExp]{Abstract syntax: imports, exports, interfaces}

\begin{code}
module HsImpExp where

#include "HsVersions.h"

import Name 		( isLexSym )
import Module		( ModuleName, WhereFrom )
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
                    pp_qual qual, ppr mod, pp_as as])
	     4 (pp_spec spec)
      where
	pp_qual False   = empty
	pp_qual True	= ptext SLIT("qualified")

	pp_as Nothing   = empty
	pp_as (Just a)  = ptext SLIT("as ") <+> ppr a

	pp_spec Nothing = empty
	pp_spec (Just (False, spec))
			= parens (interpp'SP spec)
	pp_spec (Just (True, spec))
			= ptext SLIT("hiding") <+> parens (interpp'SP spec)

ideclName (ImportDecl mod_nm _ _ _ _ _) = mod_nm
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
    ppr (IEVar	        var)	= ppr_var var
    ppr (IEThingAbs	thing)	= ppr thing
    ppr (IEThingAll	thing)	= hcat [ppr thing, text "(..)"]
    ppr (IEThingWith thing withs)
	= ppr thing <> parens (fsep (punctuate comma (map ppr_var withs)))
    ppr (IEModuleContents mod)
	= ptext SLIT("module") <+> ppr mod

ppr_var v | isOperator v = parens (ppr v)
	  | otherwise	 = ppr v
\end{code}

\begin{code}
isOperator :: Outputable a => a -> Bool
isOperator v = isLexSym (_PK_ (showSDocUnqual (ppr v)))
	-- We use (showSDoc (ppr v)), rather than isSymOcc (getOccName v) simply so
	-- that we don't need NamedThing in the context of all these functions.
	-- Gruesome, but simple.
\end{code}

