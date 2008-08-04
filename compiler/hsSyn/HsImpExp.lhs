%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

HsImpExp: Abstract syntax: imports, exports, interfaces

\begin{code}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module HsImpExp where

import Module		( ModuleName )
import HsDoc		( HsDoc )

import Outputable
import FastString
import SrcLoc		( Located(..) )
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
      = hang (hsep [ptext (sLit "import"), ppr_imp from, 
                    pp_qual qual, ppr mod, pp_as as])
	     4 (pp_spec spec)
      where
	pp_qual False   = empty
	pp_qual True	= ptext (sLit "qualified")

	pp_as Nothing   = empty
	pp_as (Just a)  = ptext (sLit "as ") <+> ppr a

	ppr_imp True  = ptext (sLit "{-# SOURCE #-}")
	ppr_imp False = empty

	pp_spec Nothing = empty
	pp_spec (Just (False, spec))
			= parens (interpp'SP spec)
	pp_spec (Just (True, spec))
			= ptext (sLit "hiding") <+> parens (interpp'SP spec)

ideclName :: ImportDecl name -> Located ModuleName
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
	= ptext (sLit "module") <+> ppr mod
    ppr (IEGroup n _)           = text ("<IEGroup: " ++ (show n) ++ ">")
    ppr (IEDoc doc)             = ppr doc
    ppr (IEDocNamed string)     = text ("<IEDocNamed: " ++ string ++ ">")
\end{code}


