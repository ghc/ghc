{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{Haskell abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}

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
        module PlaceHolder,
        module HsExtension,
        Fixity,

        HsModule(..)
) where

-- friends:
import HsDecls
import HsBinds
import HsExpr
import HsImpExp
import HsLit
import PlaceHolder
import HsExtension
import HsPat
import HsTypes
import BasicTypes       ( Fixity, WarningTxt )
import HsUtils
import HsDoc

-- others:
import Outputable
import SrcLoc
import Module           ( ModuleName )

-- libraries:
import Data.Data hiding ( Fixity )

-- | Haskell Module
--
-- All we actually declare here is the top-level structure for a module.
data HsModule name
  = HsModule {
      hsmodName :: Maybe (Located ModuleName),
        -- ^ @Nothing@: \"module X where\" is omitted (in which case the next
        --     field is Nothing too)
      hsmodExports :: Maybe (Located [LIE name]),
        -- ^ Export list
        --
        --  - @Nothing@: export list omitted, so export everything
        --
        --  - @Just []@: export /nothing/
        --
        --  - @Just [...]@: as you would expect...
        --
        --
        --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen'
        --                                   ,'ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
      hsmodImports :: [LImportDecl name],
        -- ^ We snaffle interesting stuff out of the imported interfaces early
        -- on, adding that info to TyDecls/etc; so this list is often empty,
        -- downstream.
      hsmodDecls :: [LHsDecl name],
        -- ^ Type, class, value, and interface signature decls
      hsmodDeprecMessage :: Maybe (Located WarningTxt),
        -- ^ reason\/explanation for warning/deprecation of this module
        --
        --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen'
        --                                   ,'ApiAnnotation.AnnClose'
        --

        -- For details on above see note [Api annotations] in ApiAnnotation
      hsmodHaddockModHeader :: Maybe LHsDocString
        -- ^ Haddock module info and description, unparsed
        --
        --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen'
        --                                   ,'ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
   }
     -- ^ 'ApiAnnotation.AnnKeywordId's
     --
     --  - 'ApiAnnotation.AnnModule','ApiAnnotation.AnnWhere'
     --
     --  - 'ApiAnnotation.AnnOpen','ApiAnnotation.AnnSemi',
     --    'ApiAnnotation.AnnClose' for explicit braces and semi around
     --    hsmodImports,hsmodDecls if this style is used.

     -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (DataId name) => Data (HsModule name)

instance (SourceTextX pass, OutputableBndrId pass)
  => Outputable (HsModule pass) where

    ppr (HsModule Nothing _ imports decls _ mbDoc)
      = pp_mb mbDoc $$ pp_nonnull imports
                    $$ pp_nonnull decls

    ppr (HsModule (Just name) exports imports decls deprec mbDoc)
      = vcat [
            pp_mb mbDoc,
            case exports of
              Nothing -> pp_header (text "where")
              Just es -> vcat [
                           pp_header lparen,
                           nest 8 (fsep (punctuate comma (map ppr (unLoc es)))),
                           nest 4 (text ") where")
                          ],
            pp_nonnull imports,
            pp_nonnull decls
          ]
      where
        pp_header rest = case deprec of
           Nothing -> pp_modname <+> rest
           Just d -> vcat [ pp_modname, ppr d, rest ]

        pp_modname = text "module" <+> ppr name

pp_mb :: Outputable t => Maybe t -> SDoc
pp_mb (Just x) = ppr x
pp_mb Nothing  = empty

pp_nonnull :: Outputable t => [t] -> SDoc
pp_nonnull [] = empty
pp_nonnull xs = vcat (map ppr xs)
