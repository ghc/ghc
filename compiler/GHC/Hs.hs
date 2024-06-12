{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{Haskell abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.
-}

{-# OPTIONS_GHC -Wno-orphans    #-} -- Outputable
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-} -- For deriving instance Data

module GHC.Hs (
        module Language.Haskell.Syntax,
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

        HsModule(..), AnnsModule(..),
        HsParsedModule(..), XModulePs(..)
) where

-- friends:
import GHC.Prelude

import GHC.Hs.Decls
import GHC.Hs.Binds
import GHC.Hs.Expr
import GHC.Hs.ImpExp
import GHC.Hs.Lit
import Language.Haskell.Syntax
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
import GHC.Unit.Module.Warnings

-- libraries:
import Data.Data hiding ( Fixity )

-- | Haskell Module extension point: GHC specific
data XModulePs
  = XModulePs {
      hsmodAnn :: EpAnn AnnsModule,
      hsmodLayout :: EpLayout,
        -- ^ Layout info for the module.
        -- For incomplete modules (e.g. the output of parseHeader), it is EpNoLayout.
      hsmodDeprecMessage :: Maybe (LWarningTxt GhcPs),
        -- ^ reason\/explanation for warning/deprecation of this module
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnOpen'
        --                                   ,'GHC.Parser.Annotation.AnnClose'
        --

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
      hsmodHaddockModHeader :: Maybe (LHsDoc GhcPs)
        -- ^ Haddock module info and description, unparsed
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnOpen'
        --                                   ,'GHC.Parser.Annotation.AnnClose'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
   }
   deriving Data

type instance XCModule GhcPs = XModulePs
type instance XCModule GhcRn = DataConCantHappen
type instance XCModule GhcTc = DataConCantHappen
type instance XXModule p = DataConCantHappen

deriving instance Data (HsModule GhcPs)

data AnnsModule
  = AnnsModule {
    am_main :: [AddEpAnn],
    am_decls :: [TrailingAnn],                 -- ^ Semis before the start of top decls
    am_cs :: [LEpaComment],                    -- ^ Comments before start of top decl,
                                               --   used in exact printing only
    am_eof :: Maybe (RealSrcSpan, RealSrcSpan) -- ^ End of file and end of prior token
    } deriving (Data, Eq)

instance NoAnn AnnsModule where
  noAnn = AnnsModule [] [] [] Nothing

instance Outputable (HsModule GhcPs) where
    ppr (HsModule { hsmodExt = XModulePs { hsmodHaddockModHeader = mbDoc }
                  , hsmodName = Nothing
                  , hsmodImports = imports
                  , hsmodDecls = decls })
      = pprMaybeWithDoc mbDoc $ pp_nonnull imports
                             $$ pp_nonnull decls

    ppr (HsModule { hsmodExt = XModulePs { hsmodDeprecMessage = deprec
                                         , hsmodHaddockModHeader = mbDoc }
                  , hsmodName = (Just name)
                  , hsmodExports = exports
                  , hsmodImports = imports
                  , hsmodDecls = decls })
      = pprMaybeWithDoc mbDoc $
        vcat
          [ case exports of
              Nothing -> pp_header (text "where")
              Just es -> vcat [
                           pp_header lparen,
                           nest 8 (pprWithCommas ppr (unLoc es)),
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

pp_nonnull :: Outputable t => [t] -> SDoc
pp_nonnull [] = empty
pp_nonnull xs = vcat (map ppr xs)

data HsParsedModule = HsParsedModule {
    hpm_module    :: Located (HsModule GhcPs),
    hpm_src_files :: [FilePath]
       -- ^ extra source files (e.g. from #includes).  The lexer collects
       -- these from '# <file> <line>' pragmas, which the C preprocessor
       -- leaves behind.  These files and their timestamps are stored in
       -- the .hi file, so that we can force recompilation if any of
       -- them change (#3589)
  }
