%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Haskell abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.

\begin{code}
module HsSyn (

	-- NB: don't reexport HsCore
	-- this module tells about "real Haskell"

	module HsSyn,
	module HsBinds,
	module HsDecls,
	module HsExpr,
	module HsImpExp,
	module HsLit,
	module HsPat,
	module HsTypes,
	Fixity, NewOrData, 

	collectHsBinders, collectLocatedHsBinders, 
	collectMonoBinders, collectLocatedMonoBinders,
	hsModuleName, hsModuleImports
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
import BasicTypes	( Fixity, Version, NewOrData )

-- others:
import Name		( NamedThing )
import Outputable
import SrcLoc		( SrcLoc )
import Module		( ModuleName )
\end{code}

All we actually declare here is the top-level structure for a module.
\begin{code}
data HsModule name pat
  = HsModule
	ModuleName		-- module name
	(Maybe Version)		-- source interface version number
	(Maybe [IE name])	-- export list; Nothing => export everything
				-- Just [] => export *nothing* (???)
				-- Just [...] => as you would expect...
	[ImportDecl name]	-- We snaffle interesting stuff out of the
				-- imported interfaces early on, adding that
				-- info to TyDecls/etc; so this list is
				-- often empty, downstream.
	[HsDecl name pat]	-- Type, class, value, and interface signature decls
	(Maybe DeprecTxt)	-- reason/explanation for deprecation of this module
	SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat)
	=> Outputable (HsModule name pat) where

    ppr (HsModule name iface_version exports imports
		      decls deprec src_loc)
      = vcat [
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

	pp_nonnull [] = empty
	pp_nonnull xs = vcat (map ppr xs)

hsModuleName    (HsModule mod_name _ _ _ _ _ _) = mod_name
hsModuleImports (HsModule mod_name vers exports imports decls deprec src_loc) = imports
\end{code}


%************************************************************************
%*									*
\subsection{Collecting binders from @HsBinds@}
%*									*
%************************************************************************

Get all the binders in some @MonoBinds@, IN THE ORDER OF APPEARANCE.

These functions are here, rather than in HsBinds, to avoid a loop between HsPat and HsBinds.

\begin{verbatim}
...
where
  (x, y) = ...
  f i j  = ...
  [a, b] = ...
\end{verbatim}
it should return @[x, y, f, a, b]@ (remember, order important).

\begin{code}
collectLocatedHsBinders :: HsBinds name (InPat name) -> [(name,SrcLoc)]
collectLocatedHsBinders EmptyBinds = []
collectLocatedHsBinders (MonoBind b _ _) 
 = collectLocatedMonoBinders b
collectLocatedHsBinders (ThenBinds b1 b2)
 = collectLocatedHsBinders b1 ++ collectLocatedHsBinders b2

collectHsBinders :: HsBinds name (InPat name) -> [name]
collectHsBinders EmptyBinds = []
collectHsBinders (MonoBind b _ _) 
 = collectMonoBinders b
collectHsBinders (ThenBinds b1 b2)
 = collectHsBinders b1 ++ collectHsBinders b2

collectLocatedMonoBinders :: MonoBinds name (InPat name) -> [(name,SrcLoc)]
collectLocatedMonoBinders binds
  = go binds []
  where
    go EmptyMonoBinds	       acc = acc
    go (PatMonoBind pat _ loc) acc = map (\v->(v,loc)) (collectPatBinders pat) ++ acc
    go (FunMonoBind f _ _ loc) acc = (f,loc) : acc
    go (AndMonoBinds bs1 bs2)  acc = go bs1 (go bs2 acc)

collectMonoBinders :: MonoBinds name (InPat name) -> [name]
collectMonoBinders binds
  = go binds []
  where
    go EmptyMonoBinds	       acc = acc
    go (PatMonoBind pat _ loc) acc = collectPatBinders pat ++ acc
    go (FunMonoBind f _ _ loc) acc = f : acc
    go (AndMonoBinds bs1 bs2)  acc = go bs1 (go bs2 acc)
\end{code}
