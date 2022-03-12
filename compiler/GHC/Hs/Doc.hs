-- | Types and functions for raw and lexed docstrings.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module GHC.Hs.Doc
  ( HsDoc
  , WithHsDocIdentifiers(..)
  , hsDocIds
  , LHsDoc
  , pprHsDocDebug
  , pprWithDoc
  , pprMaybeWithDoc

  , module GHC.Hs.DocString

  , ExtractedTHDocs(..)

  , DocStructureItem(..)
  , DocStructure

  , Docs(..)
  , emptyDocs
  ) where

import GHC.Prelude

import GHC.Utils.Binary
import GHC.Types.Name
import GHC.Utils.Outputable as Outputable hiding ((<>))
import GHC.Types.SrcLoc
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.EnumSet (EnumSet)
import GHC.Types.Avail
import GHC.Types.Name.Set
import GHC.Unit.Module.Name
import GHC.Driver.Flags

import Control.Applicative (liftA2)
import Data.Data
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))
import GHC.LanguageExtensions.Type
import qualified GHC.Utils.Outputable as O
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Types.Unique.Map
import Data.List (sortBy)

import GHC.Hs.DocString

-- | A docstring with the (probable) identifiers found in it.
type HsDoc = WithHsDocIdentifiers HsDocString

-- | Annotate a value with the probable identifiers found in it
-- These will be used by haddock to generate links.
--
-- The identifiers are bundled along with their location in the source file.
-- This is useful for tooling to know exactly where they originate.
--
-- This type is currently used in two places - for regular documentation comments,
-- with 'a' set to 'HsDocString', and for adding identifier information to
-- warnings, where 'a' is 'StringLiteral'
data WithHsDocIdentifiers a pass = WithHsDocIdentifiers
  { hsDocString      :: !a
  , hsDocIdentifiers :: ![Located (IdP pass)]
  }

deriving instance (Data pass, Data (IdP pass), Data a) => Data (WithHsDocIdentifiers a pass)
deriving instance (Eq (IdP pass), Eq a) => Eq (WithHsDocIdentifiers a pass)

-- | For compatibility with the existing @-ddump-parsed' output, we only show
-- the docstring.
--
-- Use 'pprHsDoc' to show `HsDoc`'s internals.
instance Outputable a => Outputable (WithHsDocIdentifiers a pass) where
  ppr (WithHsDocIdentifiers s _ids) = ppr s

instance Binary a => Binary (WithHsDocIdentifiers a GhcRn) where
  put_ bh (WithHsDocIdentifiers s ids) = do
    put_ bh s
    put_ bh ids
  get bh =
    liftA2 WithHsDocIdentifiers (get bh) (get bh)

-- | Extract a mapping from the lexed identifiers to the names they may
-- correspond to.
hsDocIds :: WithHsDocIdentifiers a GhcRn -> NameSet
hsDocIds (WithHsDocIdentifiers _ ids) = mkNameSet $ map unLoc ids

-- | Pretty print a thing with its doc
-- The docstring will include the comment decorators '-- |', '{-|' etc
-- and will come either before or after depending on how it was written
-- i.e it will come after the thing if it is a '-- ^' or '{-^' and before
-- otherwise.
pprWithDoc :: LHsDoc name -> SDoc -> SDoc
pprWithDoc doc = pprWithDocString (hsDocString $ unLoc doc)

-- | See 'pprWithHsDoc'
pprMaybeWithDoc :: Maybe (LHsDoc name) -> SDoc -> SDoc
pprMaybeWithDoc Nothing    = id
pprMaybeWithDoc (Just doc) = pprWithDoc doc

-- | Print a doc with its identifiers, useful for debugging
pprHsDocDebug :: (Outputable (IdP name)) => HsDoc name -> SDoc
pprHsDocDebug (WithHsDocIdentifiers s ids) =
    vcat [ text "text:" $$ nest 2 (pprHsDocString s)
         , text "identifiers:" $$ nest 2 (vcat (map pprLocatedAlways ids))
         ]

type LHsDoc pass = Located (HsDoc pass)

-- | A simplified version of 'HsImpExp.IE'.
data DocStructureItem
  = DsiSectionHeading Int (HsDoc GhcRn)
  | DsiDocChunk (HsDoc GhcRn)
  | DsiNamedChunkRef String
  | DsiExports Avails
  | DsiModExport
      (NonEmpty ModuleName) -- ^ We might re-export avails from multiple
                            -- modules with a single export declaration. E.g.
                            -- when we have
                            --
                            -- > module M (module X) where
                            -- > import R0 as X
                            -- > import R1 as X
      Avails

instance Binary DocStructureItem where
  put_ bh = \case
    DsiSectionHeading level doc -> do
      putByte bh 0
      put_ bh level
      put_ bh doc
    DsiDocChunk doc -> do
      putByte bh 1
      put_ bh doc
    DsiNamedChunkRef name -> do
      putByte bh 2
      put_ bh name
    DsiExports avails -> do
      putByte bh 3
      put_ bh avails
    DsiModExport mod_names avails -> do
      putByte bh 4
      put_ bh mod_names
      put_ bh avails

  get bh = do
    tag <- getByte bh
    case tag of
      0 -> DsiSectionHeading <$> get bh <*> get bh
      1 -> DsiDocChunk <$> get bh
      2 -> DsiNamedChunkRef <$> get bh
      3 -> DsiExports <$> get bh
      4 -> DsiModExport <$> get bh <*> get bh
      _ -> fail "instance Binary DocStructureItem: Invalid tag"

instance Outputable DocStructureItem where
  ppr = \case
    DsiSectionHeading level doc -> vcat
      [ text "section heading, level" <+> ppr level O.<> colon
      , nest 2 (pprHsDocDebug doc)
      ]
    DsiDocChunk doc -> vcat
      [ text "documentation chunk:"
      , nest 2 (pprHsDocDebug doc)
      ]
    DsiNamedChunkRef name ->
      text "reference to named chunk:" <+> text name
    DsiExports avails ->
      text "avails:" $$ nest 2 (ppr avails)
    DsiModExport mod_names avails ->
      text "re-exported module(s):" <+> ppr mod_names $$ nest 2 (ppr avails)

type DocStructure = [DocStructureItem]

data Docs = Docs
  { docs_mod_hdr      :: Maybe (HsDoc GhcRn)
    -- ^ Module header.
  , docs_decls        :: UniqMap Name [HsDoc GhcRn]
    -- ^ Docs for declarations: functions, data types, instances, methods etc.
    -- A list because sometimes subsequent haddock comments can be combined into one
  , docs_args         :: UniqMap Name (IntMap (HsDoc GhcRn))
    -- ^ Docs for arguments. E.g. function arguments, method arguments.
  , docs_structure    :: DocStructure
  , docs_named_chunks :: Map String (HsDoc GhcRn)
    -- ^ Map from chunk name to content.
    --
    -- This map will be empty unless we have an explicit export list from which
    -- we can reference the chunks.
  , docs_haddock_opts :: Maybe String
    -- ^ Haddock options from @OPTIONS_HADDOCK@ or from @-haddock-opts@.
  , docs_language     :: Maybe Language
    -- ^ The 'Language' used in the module, for example 'Haskell2010'.
  , docs_extensions   :: EnumSet Extension
    -- ^ The full set of language extensions used in the module.
  }

instance Binary Docs where
  put_ bh docs = do
    put_ bh (docs_mod_hdr docs)
    put_ bh (sortBy (\a b -> (fst a) `stableNameCmp` fst b) $ nonDetEltsUniqMap $ docs_decls docs)
    put_ bh (sortBy (\a b -> (fst a) `stableNameCmp` fst b) $ nonDetEltsUniqMap $ docs_args docs)
    put_ bh (docs_structure docs)
    put_ bh (Map.toList $ docs_named_chunks docs)
    put_ bh (docs_haddock_opts docs)
    put_ bh (docs_language docs)
    put_ bh (docs_extensions docs)
  get bh = do
    mod_hdr <- get bh
    decls <- listToUniqMap <$> get bh
    args <- listToUniqMap <$> get bh
    structure <- get bh
    named_chunks <- Map.fromList <$> get bh
    haddock_opts <- get bh
    language <- get bh
    exts <- get bh
    pure Docs { docs_mod_hdr = mod_hdr
              , docs_decls =  decls
              , docs_args = args
              , docs_structure = structure
              , docs_named_chunks = named_chunks
              , docs_haddock_opts = haddock_opts
              , docs_language = language
              , docs_extensions = exts
              }

instance Outputable Docs where
  ppr docs =
      vcat
        [ pprField (pprMaybe pprHsDocDebug) "module header" docs_mod_hdr
        , pprField (ppr . fmap (ppr . map pprHsDocDebug)) "declaration docs" docs_decls
        , pprField (ppr . fmap (pprIntMap ppr pprHsDocDebug)) "arg docs" docs_args
        , pprField (vcat . map ppr) "documentation structure" docs_structure
        , pprField (pprMap (doubleQuotes . text) pprHsDocDebug) "named chunks"
                   docs_named_chunks
        , pprField pprMbString "haddock options" docs_haddock_opts
        , pprField ppr "language" docs_language
        , pprField (vcat . map ppr . EnumSet.toList) "language extensions"
                   docs_extensions
        ]
    where
      pprField :: (a -> SDoc) -> String -> (Docs -> a) -> SDoc
      pprField ppr' heading lbl =
        text heading O.<> colon $$ nest 2 (ppr' (lbl docs))
      pprMap pprKey pprVal m =
        vcat $ flip map (Map.toList m) $ \(k, v) ->
          pprKey k O.<> colon $$ nest 2 (pprVal v)
      pprIntMap pprKey pprVal m =
        vcat $ flip map (IntMap.toList m) $ \(k, v) ->
          pprKey k O.<> colon $$ nest 2 (pprVal v)
      pprMbString Nothing = empty
      pprMbString (Just s) = text s
      pprMaybe ppr' = \case
        Nothing -> text "Nothing"
        Just x -> text "Just" <+> ppr' x

emptyDocs :: Docs
emptyDocs = Docs
  { docs_mod_hdr = Nothing
  , docs_decls = emptyUniqMap
  , docs_args = emptyUniqMap
  , docs_structure = []
  , docs_named_chunks = Map.empty
  , docs_haddock_opts = Nothing
  , docs_language = Nothing
  , docs_extensions = EnumSet.empty
  }

-- | Maps of docs that were added via Template Haskell's @putDoc@.
data ExtractedTHDocs =
  ExtractedTHDocs
    { ethd_mod_header :: Maybe (HsDoc GhcRn)
      -- ^ The added module header documentation, if it exists.
    , ethd_decl_docs  :: UniqMap Name (HsDoc GhcRn)
      -- ^ The documentation added to declarations.
    , ethd_arg_docs   :: UniqMap Name (IntMap (HsDoc GhcRn))
      -- ^ The documentation added to function arguments.
    , ethd_inst_docs  :: UniqMap Name (HsDoc GhcRn)
      -- ^ The documentation added to class and family instances.
    }
