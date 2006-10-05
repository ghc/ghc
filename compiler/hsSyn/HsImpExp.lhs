%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsImpExp]{Abstract syntax: imports, exports, interfaces}

\begin{code}
module HsImpExp where

#include "HsVersions.h"

import Module		( ModuleName )
import HsDoc		( HsDoc )

import Outputable
import FastString
import SrcLoc		( Located(..) )
import Char		( isAlpha )
\end{code}

%************************************************************************
%*									*
\subsection{Import and export declaration lists}
%*									*
%************************************************************************

One per \tr{import} declaration in a module.
\begin{code}
type LImportDecl name = Located (ImportDecl name)

data ImportDecl name
  = ImportDecl	  (Located ModuleName)		-- module name
		  Bool				-- True <=> {-# SOURCE #-} import
		  Bool				-- True => qualified
		  (Maybe ModuleName)		-- as Module
		  (Maybe (Bool, [LIE name]))	-- (True => hiding, names)
\end{code}

\begin{code}
instance (Outputable name) => Outputable (ImportDecl name) where
    ppr (ImportDecl mod from qual as spec)
      = hang (hsep [ptext SLIT("import"), ppr_imp from, 
                    pp_qual qual, ppr mod, pp_as as])
	     4 (pp_spec spec)
      where
	pp_qual False   = empty
	pp_qual True	= ptext SLIT("qualified")

	pp_as Nothing   = empty
	pp_as (Just a)  = ptext SLIT("as ") <+> ppr a

	ppr_imp True  = ptext SLIT("{-# SOURCE #-}")
	ppr_imp False = empty

	pp_spec Nothing = empty
	pp_spec (Just (False, spec))
			= parens (interpp'SP spec)
	pp_spec (Just (True, spec))
			= ptext SLIT("hiding") <+> parens (interpp'SP spec)

ideclName (ImportDecl mod_nm _ _ _ _) = mod_nm
\end{code}

%************************************************************************
%*									*
\subsection{Imported and exported entities}
%*									*
%************************************************************************

\begin{code}
type LIE name = Located (IE name)

data IE name
  = IEVar               name
  | IEThingAbs          name		 -- Class/Type (can't tell)
  | IEThingAll          name		 -- Class/Type plus all methods/constructors
  | IEThingWith         name [name]	 -- Class/Type plus some methods/constructors
  | IEModuleContents    ModuleName	 -- (Export Only)
  | IEGroup             Int (HsDoc name) -- Doc section heading
  | IEDoc               (HsDoc name)     -- Some documentation
  | IEDocNamed          String           -- Reference to named doc
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
ieNames (IEGroup          _ _ ) = []
ieNames (IEDoc            _   ) = []
ieNames (IEDocNamed       _   ) = []        
\end{code}

\begin{code}
instance (Outputable name) => Outputable (IE name) where
    ppr (IEVar	        var)	= pprHsVar var
    ppr (IEThingAbs	thing)	= ppr thing
    ppr (IEThingAll	thing)	= hcat [ppr thing, text "(..)"]
    ppr (IEThingWith thing withs)
	= ppr thing <> parens (fsep (punctuate comma (map pprHsVar withs)))
    ppr (IEModuleContents mod)
	= ptext SLIT("module") <+> ppr mod
    ppr (IEGroup n doc)         = text ("<IEGroup: " ++ (show n) ++ ">") 
    ppr (IEDoc doc)             = ppr doc
    ppr (IEDocNamed string)     = text ("<IEDocNamed: " ++ string ++ ">")
\end{code}

\begin{code}
pprHsVar :: Outputable name => name -> SDoc
pprHsVar v | isOperator ppr_v = parens ppr_v
	   | otherwise	      = ppr_v
	   where
	     ppr_v = ppr v

isOperator :: SDoc -> Bool
isOperator ppr_v 
  = case showSDocUnqual ppr_v of
	('(':s)   -> False		-- (), (,) etc
	('[':s)   -> False		-- []
	('$':c:s) -> not (isAlpha c)	-- Don't treat $d as an operator
	(':':c:s) -> not (isAlpha c)	-- Don't treat :T as an operator
	('_':s)   -> False		-- Not an operator
	(c:s)     -> not (isAlpha c)	-- Starts with non-alpha
	other     -> False
    -- We use (showSDoc (ppr v)), rather than isSymOcc (getOccName v) simply so
    -- that we don't need NamedThing in the context of all these functions.
    -- Gruesome, but simple.
\end{code}

