%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Haskell abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.

\begin{code}
module HsSyn (
	module HsBinds,
	module HsDecls,
	module HsExpr,
	module HsImpExp,
	module HsLit,
	module HsPat,
	module HsTypes,
 	module HsUtils,
	module HsDoc,
	Fixity,

	HsModule(..), HsExtCore(..),

	HaddockModInfo(..),
	emptyHaddockModInfo,
) where

#include "HsVersions.h"

-- friends:
import HsDecls		
import HsBinds
import HsExpr
import HsImpExp
import HsLit
import HsPat
import HsTypes
import BasicTypes	( Fixity, DeprecTxt )
import HsUtils
import HsDoc

-- others:
import IfaceSyn		( IfaceBinding )
import Outputable
import SrcLoc		( Located(..) )
import Module		( Module, ModuleName )
\end{code}

All we actually declare here is the top-level structure for a module.
\begin{code}
data HsModule name
  = HsModule
	(Maybe (Located ModuleName))-- Nothing => "module X where" is omitted
				--	(in which case the next field is Nothing too)
	(Maybe [LIE name])	-- Export list; Nothing => export list omitted, so export everything
				-- Just [] => export *nothing*
				-- Just [...] => as you would expect...
	[LImportDecl name]	-- We snaffle interesting stuff out of the
				-- imported interfaces early on, adding that
				-- info to TyDecls/etc; so this list is
				-- often empty, downstream.
	[LHsDecl name]		-- Type, class, value, and interface signature decls
	(Maybe DeprecTxt)	-- reason/explanation for deprecation of this module
	(Maybe String)          -- Haddock options, declared with the {-# DOCOPTIONS ... #-} pragma
	(HaddockModInfo name)   -- Haddock module info
	(Maybe (HsDoc name))    -- Haddock module description

data HaddockModInfo name = HaddockModInfo { 
	hmi_description :: Maybe (HsDoc name),
	hmi_portability :: Maybe String,
	hmi_stability   :: Maybe String,
	hmi_maintainer  :: Maybe String
}

emptyHaddockModInfo :: HaddockModInfo a                                                  
emptyHaddockModInfo = HaddockModInfo {                                                  
	hmi_description = Nothing,
	hmi_portability = Nothing,
	hmi_stability   = Nothing,
	hmi_maintainer  = Nothing
}       

data HsExtCore name	-- Read from Foo.hcr
  = HsExtCore
	Module
	[TyClDecl name]	-- Type declarations only; just as in Haskell source,
			-- so that we can infer kinds etc
	[IfaceBinding]	-- And the bindings
\end{code}


\begin{code}
instance Outputable Char where
  ppr c = text [c]

instance (OutputableBndr name)
	=> Outputable (HsModule name) where

    ppr (HsModule Nothing _ imports decls _ _ _ mbDoc)
      = pp_mb mbDoc $$ pp_nonnull imports $$ pp_nonnull decls

    ppr (HsModule (Just name) exports imports decls deprec opts _ mbDoc)
      = vcat [
	    pp_mb mbDoc,
	    case exports of
	      Nothing -> pp_header (ptext SLIT("where"))
	      Just es -> vcat [
			   pp_header lparen,
			   nest 8 (fsep (punctuate comma (map ppr es))),
			   nest 4 (ptext SLIT(") where"))
			  ],
	    pp_nonnull imports,
	    pp_nonnull decls
          ]
      where
	pp_header rest = case deprec of
           Nothing -> pp_modname <+> rest
           Just d -> vcat [ pp_modname, ppr d, rest ]

	pp_modname = ptext SLIT("module") <+> ppr name

pp_mb (Just x) = ppr x 
pp_mb Nothing  = empty

pp_nonnull [] = empty
pp_nonnull xs = vcat (map ppr xs)
\end{code}
