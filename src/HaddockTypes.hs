--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--
-- Ported to use the GHC API by David Waern 2006
-- 

module HaddockTypes (
  ExportItem2(..), 
  ModuleMap2, 
  DocMap,
  HaddockModule(..), 
  DocOption(..), 
  InstHead2,
  DocName(..),
  DocMarkup(..)
 ) where

import GHC
import Outputable

import Data.Map

data DocOption
  = OptHide           -- ^ This module should not appear in the docs
  | OptPrune
  | OptIgnoreExports  -- ^ Pretend everything is exported
  | OptNotHome        -- ^ Not the best place to get docs for things
                      -- exported by this module.
  deriving (Eq, Show)

data ExportItem2 name
  = ExportDecl2
      Name                 -- ^ The original name
      (LHsDecl name)       -- ^ A declaration
      (Maybe (HsDoc name)) -- ^ Maybe a doc comment
      [InstHead2 name]	   -- ^ Instances relevant to this declaration

  | ExportNoDecl2          -- ^ An exported entity for which we have no 
                           -- documentation (perhaps because it resides in
                           -- another package)
      Name                 -- ^ The original name
      name                 -- ^ Where to link to
      [name]               -- ^ Subordinate names

  | ExportGroup2           -- ^ A section heading
      Int                  -- ^ section level (1, 2, 3, ... )
      String               -- ^ Section "id" (for hyperlinks)
      (HsDoc name)         -- ^ Section heading text

  | ExportDoc2             -- ^ Some documentation
      (HsDoc name)

  | ExportModule2          -- ^ A cross-reference to another module
      Module

type InstHead2 name = ([HsPred name], name, [HsType name])
type ModuleMap2 = Map Module HaddockModule
type DocMap = Map Name (HsDoc DocName)
data DocName = Link Name | NoLink Name

instance Outputable DocName where
  ppr (Link   n) = ppr n
  ppr (NoLink n) = ppr n

data HaddockModule = HM {

-- | A value to identify the module

  hmod_mod                :: Module,

-- | The original filename for this module

  hmod_orig_filename      :: FilePath,

-- | Textual information about the module 

  hmod_info               :: HaddockModInfo Name,

-- | The documentation header for this module

  hmod_doc                :: Maybe (HsDoc Name),

-- | The renamed documentation header for this module

  hmod_rn_doc             :: Maybe (HsDoc DocName),

-- | The Haddock options for this module (prune, ignore-exports, etc)

  hmod_options            :: [DocOption],

  hmod_exported_decl_map  :: Map Name (LHsDecl Name),
  hmod_doc_map            :: Map Name (HsDoc Name),  
  hmod_rn_doc_map         :: Map Name (HsDoc DocName),

  hmod_export_items       :: [ExportItem2 Name],
  hmod_rn_export_items    :: [ExportItem2 DocName],

-- | All the names that are defined in this module

  hmod_locals             :: [Name],

-- | All the names that are exported by this module

  hmod_exports            :: [Name],

-- | All the visible names exported by this module
-- For a name to be visible, it has to:
-- - be exported normally, and not via a full module re-exportation.
-- - have a declaration in this module or any of it's imports, with the exception
--   that it can't be from another package.
-- Basically, a visible name is a name that will show up in the documentation
-- for this module.

  hmod_visible_exports    :: [Name],

  hmod_sub_map            :: Map Name [Name],

-- | The instances exported by this module

  hmod_instances          :: [Instance]
}

data DocMarkup id a = Markup {
  markupEmpty         :: a,
  markupString        :: String -> a,
  markupParagraph     :: a -> a,
  markupAppend        :: a -> a -> a,
  markupIdentifier    :: [id] -> a,
  markupModule        :: String -> a,
  markupEmphasis      :: a -> a,
  markupMonospaced    :: a -> a,
  markupUnorderedList :: [a] -> a,
  markupOrderedList   :: [a] -> a,
  markupDefList       :: [(a,a)] -> a,
  markupCodeBlock     :: a -> a,
  markupURL           :: String -> a,
  markupAName         :: String -> a
}

instance (Outputable a, OutputableBndr a) => Outputable (ExportItem2 a) where
  ppr (ExportDecl2 n decl doc instns) = text "ExportDecl" <+> ppr n <+> ppr decl <+> ppr doc <+> ppr instns
  ppr (ExportNoDecl2 n1 n2 ns) = text "ExportNoDecl (org name, link name, sub names)" <+> ppr n1 <+> ppr n2 <+> ppr ns
  ppr (ExportGroup2 lev id doc) = text "ExportGroup (lev, id, doc)" <+> ppr lev <+> ppr doc
  ppr (ExportDoc2 doc) = text "ExportDoc" <+> ppr doc
  ppr (ExportModule2 mod) = text "ExportModule" <+> ppr mod 	

instance OutputableBndr DocName where
  pprBndr _ d = ppr d
