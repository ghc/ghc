%
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
	Fixity, NewOrData, 

	HsModule(..), HsExtCore(..),
	collectStmtsBinders, collectStmtBinders, collectLStmtBinders,
	collectGroupBinders, collectHsBindLocatedBinders,
	collectHsBindBinders,
	collectSigTysFromHsBind, collectSigTysFromHsBinds
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
import HscTypes		( DeprecTxt )
import BasicTypes	( Fixity, NewOrData )
import HsUtils

-- others:
import IfaceSyn		( IfaceBinding )
import Outputable
import SrcLoc		( Located(..), unLoc, noLoc )
import Module		( Module )
import Bag		( Bag, foldrBag )
\end{code}

All we actually declare here is the top-level structure for a module.
\begin{code}
data HsModule name
  = HsModule
	(Maybe (Located Module))-- Nothing => "module X where" is omitted
				--	(in which case the next field is Nothing too)
	(Maybe [LIE name])	-- Export list; Nothing => export list omitted, so export everything
				-- Just [] => export *nothing*
				-- Just [...] => as you would expect...
	[LImportDecl name]	-- We snaffle interesting stuff out of the
				-- imported interfaces early on, adding that
				-- info to TyDecls/etc; so this list is
				-- often empty, downstream.
	[LHsDecl name]	-- Type, class, value, and interface signature decls
	(Maybe DeprecTxt)	-- reason/explanation for deprecation of this module

data HsExtCore name	-- Read from Foo.hcr
  = HsExtCore
	Module
	[TyClDecl name]	-- Type declarations only; just as in Haskell source,
			-- so that we can infer kinds etc
	[IfaceBinding]	-- And the bindings
\end{code}

\begin{code}
instance (OutputableBndr name)
	=> Outputable (HsModule name) where

    ppr (HsModule Nothing _ imports decls _)
      = pp_nonnull imports $$ pp_nonnull decls

    ppr (HsModule (Just name) exports imports decls deprec)
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
collectGroupBinders :: [HsBindGroup name] -> [Located name]
collectGroupBinders groups = foldr collect_group [] groups
	where
	  collect_group (HsBindGroup bag sigs is_rec) acc
	 	= foldrBag (collectAcc . unLoc) acc bag
	  collect_group (HsIPBinds _) acc = acc


collectAcc :: HsBind name -> [Located name] -> [Located name]
collectAcc (PatBind pat _) acc = collectLocatedPatBinders pat ++ acc
collectAcc (FunBind f _ _) acc = f : acc
collectAcc (VarBind f _) acc  = noLoc f : acc
collectAcc (AbsBinds _ _ dbinds _ binds) acc
  = [noLoc dp | (_,dp,_) <- dbinds] ++ acc
	-- ++ foldr collectAcc acc binds
	-- I don't think we want the binders from the nested binds
	-- The only time we collect binders from a typechecked 
	-- binding (hence see AbsBinds) is in zonking in TcHsSyn

collectHsBindBinders :: Bag (LHsBind name) -> [name]
collectHsBindBinders binds = map unLoc (collectHsBindLocatedBinders binds)

collectHsBindLocatedBinders :: Bag (LHsBind name) -> [Located name]
collectHsBindLocatedBinders binds = foldrBag (collectAcc . unLoc) [] binds
\end{code}


%************************************************************************
%*									*
\subsection{Getting patterns out of bindings}
%*									*
%************************************************************************

Get all the pattern type signatures out of a bunch of bindings

\begin{code}
collectSigTysFromHsBinds :: [LHsBind name] -> [LHsType name]
collectSigTysFromHsBinds binds = concat (map collectSigTysFromHsBind binds)

collectSigTysFromHsBind :: LHsBind name -> [LHsType name]
collectSigTysFromHsBind bind
  = go (unLoc bind)
  where
    go (PatBind pat _)  = collectSigTysFromPat pat
    go (FunBind f _ ms) = go_matches (map unLoc ms)

	-- A binding like    x :: a = f y
	-- is parsed as FunMonoBind, but for this purpose we 	
	-- want to treat it as a pattern binding
    go_matches []				 = []
    go_matches (Match [] (Just sig) _ : matches) = sig : go_matches matches
    go_matches (match		      : matches) = go_matches matches
\end{code}

\begin{code}
collectStmtsBinders :: [LStmt id] -> [Located id]
collectStmtsBinders = concatMap collectLStmtBinders

collectLStmtBinders = collectStmtBinders . unLoc

collectStmtBinders :: Stmt id -> [Located id]
  -- Id Binders for a Stmt... [but what about pattern-sig type vars]?
collectStmtBinders (BindStmt pat _)   = collectLocatedPatBinders pat
collectStmtBinders (LetStmt binds)    = collectGroupBinders binds
collectStmtBinders (ExprStmt _ _)     = []
collectStmtBinders (ResultStmt _)     = []
collectStmtBinders (RecStmt ss _ _ _) = collectStmtsBinders ss
collectStmtBinders other              = panic "collectStmtBinders"
\end{code}
