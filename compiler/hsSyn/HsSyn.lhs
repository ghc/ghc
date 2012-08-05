%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Haskell abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}

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
) where

-- friends:
import HsDecls
import HsBinds
import HsExpr
import HsImpExp
import HsLit
import HsPat
import HsTypes
import BasicTypes       ( Fixity, WarningTxt )
import HsUtils
import HsDoc

-- others:
import OccName          ( HasOccName )
import IfaceSyn         ( IfaceBinding )
import Outputable
import SrcLoc
import Module           ( Module, ModuleName )
import FastString

-- libraries:
import Data.Data hiding ( Fixity )
\end{code}

\begin{code}
-- | All we actually declare here is the top-level structure for a module.
data HsModule name
  = HsModule {
      hsmodName :: Maybe (Located ModuleName),
        -- ^ @Nothing@: \"module X where\" is omitted (in which case the next
        --     field is Nothing too)
      hsmodExports :: Maybe [LIE name],
        -- ^ Export list
        --
        --  - @Nothing@: export list omitted, so export everything
        --
        --  - @Just []@: export /nothing/
        --
        --  - @Just [...]@: as you would expect...
        --
      hsmodImports :: [LImportDecl name],
        -- ^ We snaffle interesting stuff out of the imported interfaces early
        -- on, adding that info to TyDecls/etc; so this list is often empty,
        -- downstream.
      hsmodDecls :: [LHsDecl name],
        -- ^ Type, class, value, and interface signature decls
      hsmodDeprecMessage :: Maybe WarningTxt,
        -- ^ reason\/explanation for warning/deprecation of this module
      hsmodHaddockModHeader :: Maybe LHsDocString
        -- ^ Haddock module info and description, unparsed
   } deriving (Data, Typeable)

data HsExtCore name     -- Read from Foo.hcr
  = HsExtCore
        Module
        [TyClDecl name] -- Type declarations only; just as in Haskell source,
                        -- so that we can infer kinds etc
        [IfaceBinding]  -- And the bindings
\end{code}


\begin{code}
instance (OutputableBndr name, HasOccName name)
        => Outputable (HsModule name) where

    ppr (HsModule Nothing _ imports decls _ mbDoc)
      = pp_mb mbDoc $$ pp_nonnull imports $$ pp_nonnull decls

    ppr (HsModule (Just name) exports imports decls deprec mbDoc)
      = vcat [
            pp_mb mbDoc,
            case exports of
              Nothing -> pp_header (ptext (sLit "where"))
              Just es -> vcat [
                           pp_header lparen,
                           nest 8 (fsep (punctuate comma (map ppr es))),
                           nest 4 (ptext (sLit ") where"))
                          ],
            pp_nonnull imports,
            pp_nonnull decls
          ]
      where
        pp_header rest = case deprec of
           Nothing -> pp_modname <+> rest
           Just d -> vcat [ pp_modname, ppr d, rest ]

        pp_modname = ptext (sLit "module") <+> ppr name

pp_mb :: Outputable t => Maybe t -> SDoc
pp_mb (Just x) = ppr x
pp_mb Nothing  = empty

pp_nonnull :: Outputable t => [t] -> SDoc
pp_nonnull [] = empty
pp_nonnull xs = vcat (map ppr xs)
\end{code}
