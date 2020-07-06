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
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module GHC.Hs.Extension
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-} -- For deriving instance Data

module GHC.Hs (
        module GHC.Hs.Binds,
        module GHC.Hs.Decls,
        module GHC.Hs.Expr,
        module GHC.Hs.ImpExp,
        module GHC.Hs.Lit,
        module GHC.Hs.Pat,
        module GHC.Hs.Type,
        module GHC.Hs.Utils,
        module GHC.Hs.Doc,
        module GHC.Hs.Extension,
        module GHC.Parser.Annotation,
        Fixity,

        HsModule(..),
        HsParsedModule(..)
) where

-- friends:
import GHC.Prelude

import GHC.Hs.Decls
import GHC.Hs.Binds
import GHC.Hs.Expr
import GHC.Hs.ImpExp
import GHC.Hs.Lit
import GHC.Hs.Extension
import GHC.Parser.Annotation
import GHC.Hs.Pat
import GHC.Hs.Type
import GHC.Hs.Utils
import GHC.Hs.Doc
import GHC.Hs.Instances () -- For Data instances

-- others:
import GHC.Utils.Outputable
import GHC.Types.Fixity         ( Fixity )
import GHC.Types.SrcLoc
import GHC.Unit.Module          ( ModuleName )
import GHC.Unit.Module.Warnings ( WarningTxt )

-- libraries:
import Data.Data hiding ( Fixity )

-- | Haskell Module
--
-- All we actually declare here is the top-level structure for a module.
data HsModule
  = HsModule {
      hsmodAnn :: ApiAnn' AnnsModule,
      hsmodLayout :: LayoutInfo,
        -- ^ Layout info for the module.
        -- For incomplete modules (e.g. the output of parseHeader), it is NoLayoutInfo.
      hsmodName :: Maybe (Located ModuleName),
        -- ^ @Nothing@: \"module X where\" is omitted (in which case the next
        --     field is Nothing too)
      hsmodExports :: Maybe (LocatedL [LIE GhcPs]),
        -- ^ Export list
        --
        --  - @Nothing@: export list omitted, so export everything
        --
        --  - @Just []@: export /nothing/
        --
        --  - @Just [...]@: as you would expect...
        --
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnOpen'
        --                                   ,'GHC.Parser.Annotation.AnnClose'

        -- For details on above see note [Api annotations] in GHC.Parser.Annotation
      hsmodImports :: [LImportDecl GhcPs],
        -- ^ We snaffle interesting stuff out of the imported interfaces early
        -- on, adding that info to TyDecls/etc; so this list is often empty,
        -- downstream.
      hsmodDecls :: [LHsDecl GhcPs],
        -- ^ Type, class, value, and interface signature decls
      hsmodDeprecMessage :: Maybe (LocatedP WarningTxt),
        -- ^ reason\/explanation for warning/deprecation of this module
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnOpen'
        --                                   ,'GHC.Parser.Annotation.AnnClose'
        --

        -- For details on above see note [Api annotations] in GHC.Parser.Annotation
      hsmodHaddockModHeader :: Maybe LHsDocString
        -- ^ Haddock module info and description, unparsed
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnOpen'
        --                                   ,'GHC.Parser.Annotation.AnnClose'

        -- For details on above see note [Api annotations] in GHC.Parser.Annotation
   }
     -- ^ 'GHC.Parser.Annotation.AnnKeywordId's
     --
     --  - 'GHC.Parser.Annotation.AnnModule','GHC.Parser.Annotation.AnnWhere'
     --
     --  - 'GHC.Parser.Annotation.AnnOpen','GHC.Parser.Annotation.AnnSemi',
     --    'GHC.Parser.Annotation.AnnClose' for explicit braces and semi around
     --    hsmodImports,hsmodDecls if this style is used.

     -- For details on above see note [Api annotations] in GHC.Parser.Annotation

deriving instance Data HsModule

instance Outputable HsModule where

    ppr (HsModule _ _ Nothing _ imports decls _ mbDoc)
      = pp_mb mbDoc $$ pp_nonnull imports
                    $$ pp_nonnull decls

    ppr (HsModule _ _ (Just name) exports imports decls deprec mbDoc)
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

data HsParsedModule = HsParsedModule {
    hpm_module    :: Located HsModule,
    hpm_src_files :: [FilePath],
       -- ^ extra source files (e.g. from #includes).  The lexer collects
       -- these from '# <file> <line>' pragmas, which the C preprocessor
       -- leaves behind.  These files and their timestamps are stored in
       -- the .hi file, so that we can force recompilation if any of
       -- them change (#3589)
    hpm_annotations :: ApiAnns
    -- See note [Api annotations] in GHC.Parser.Annotation
  }

