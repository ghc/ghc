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

	module HsBinds,
	module HsDecls,
	module HsExpr,
	module HsImpExp,
	module HsLit,
	module HsPat,
	module HsTypes,
	Fixity, NewOrData, 

	HsModule(..), 
	collectStmtsBinders, collectStmtBinders,
	collectHsBinders,   collectLocatedHsBinders, 
	collectMonoBinders, collectLocatedMonoBinders,
	collectSigTysFromHsBinds, collectSigTysFromMonoBinds
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
import Module		( Module )
\end{code}

All we actually declare here is the top-level structure for a module.
\begin{code}
data HsModule name
  = HsModule
	(Maybe Module)		-- Nothing => "module X where" is omitted
				--	(in which case the next field is Nothing too)
	(Maybe [IE name])	-- Export list; Nothing => export list omitted, so export everything
				-- Just [] => export *nothing*
				-- Just [...] => as you would expect...
	[ImportDecl name]	-- We snaffle interesting stuff out of the
				-- imported interfaces early on, adding that
				-- info to TyDecls/etc; so this list is
				-- often empty, downstream.
	[HsDecl name]	-- Type, class, value, and interface signature decls
	(Maybe DeprecTxt)	-- reason/explanation for deprecation of this module
	SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, OutputableBndr name)
	=> Outputable (HsModule name) where

    ppr (HsModule Nothing _ imports decls _ src_loc)
      = pp_nonnull imports $$ pp_nonnull decls

    ppr (HsModule (Just name) exports imports decls deprec src_loc)
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
collectLocatedHsBinders :: HsBinds name -> [(name,SrcLoc)]
-- Used at top level only; so no need for an IPBinds case
collectLocatedHsBinders EmptyBinds = []
collectLocatedHsBinders (MonoBind b _ _) 
 = collectLocatedMonoBinders b
collectLocatedHsBinders (ThenBinds b1 b2)
 = collectLocatedHsBinders b1 ++ collectLocatedHsBinders b2

collectHsBinders :: HsBinds name -> [name]
collectHsBinders EmptyBinds 	   = []
collectHsBinders (IPBinds _)	   = []		-- Implicit parameters don't create
						-- ordinary bindings
collectHsBinders (MonoBind b _ _)  = collectMonoBinders b
collectHsBinders (ThenBinds b1 b2) = collectHsBinders b1 ++ collectHsBinders b2

collectLocatedMonoBinders :: MonoBinds name -> [(name,SrcLoc)]
collectLocatedMonoBinders binds
  = go binds []
  where
    go EmptyMonoBinds	       acc = acc
    go (PatMonoBind pat _ loc) acc = map (\v->(v,loc)) (collectPatBinders pat) ++ acc
    go (FunMonoBind f _ _ loc) acc = (f,loc) : acc
    go (AndMonoBinds bs1 bs2)  acc = go bs1 (go bs2 acc)

collectMonoBinders :: MonoBinds name -> [name]
collectMonoBinders binds
  = go binds []
  where
    go EmptyMonoBinds	       acc = acc
    go (PatMonoBind pat _ loc) acc = collectPatBinders pat ++ acc
    go (FunMonoBind f _ _ loc) acc = f : acc
    go (AndMonoBinds bs1 bs2)  acc = go bs1 (go bs2 acc)
    go (VarMonoBind v _)       acc = v : acc
    go (AbsBinds _ _ dbinds _ binds) acc
      = [dp | (_,dp,_) <- dbinds] ++ go binds acc
\end{code}


%************************************************************************
%*									*
\subsection{Getting patterns out of bindings}
%*									*
%************************************************************************

Get all the pattern type signatures out of a bunch of bindings

\begin{code}
collectSigTysFromHsBinds :: HsBinds name -> [HsType name]
collectSigTysFromHsBinds EmptyBinds        = [] 
collectSigTysFromHsBinds (IPBinds _)       = [] 
collectSigTysFromHsBinds (MonoBind b _ _)  = collectSigTysFromMonoBinds b
collectSigTysFromHsBinds (ThenBinds b1 b2) = collectSigTysFromHsBinds b1 ++
					     collectSigTysFromHsBinds b2
 

collectSigTysFromMonoBinds :: MonoBinds name -> [HsType name]
collectSigTysFromMonoBinds bind
  = go bind []
  where
    go EmptyMonoBinds           acc = acc
    go (PatMonoBind pat _ loc)  acc = collectSigTysFromPat pat ++ acc
    go (FunMonoBind f _ ms loc) acc = go_matches ms acc
    go (AndMonoBinds bs1 bs2)   acc = go bs1 (go bs2 acc)

	-- A binding like    x :: a = f y
	-- is parsed as FunMonoBind, but for this purpose we 	
	-- want to treat it as a pattern binding
    go_matches []				 acc = acc
    go_matches (Match [] (Just sig) _ : matches) acc = sig : go_matches matches acc
    go_matches (match		      : matches) acc = go_matches matches acc
\end{code}

\begin{code}
collectStmtsBinders :: [Stmt id] -> [id]
collectStmtsBinders = concatMap collectStmtBinders

collectStmtBinders :: Stmt id -> [id]
  -- Id Binders for a Stmt... [but what about pattern-sig type vars]?
collectStmtBinders (BindStmt pat _ _) = collectPatBinders pat
collectStmtBinders (LetStmt binds)    = collectHsBinders binds
collectStmtBinders (ExprStmt _ _ _)   = []
collectStmtBinders (ResultStmt _ _)   = []
collectStmtBinders (RecStmt ss _ _ _) = collectStmtsBinders ss
collectStmtBinders other              = panic "collectStmtBinders"
\end{code}

