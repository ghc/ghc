--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Types where


import GHC hiding (NoLink)
import Outputable
import Data.Map


data DocOption
  = OptHide           -- ^ This module should not appear in the docs
  | OptPrune
  | OptIgnoreExports  -- ^ Pretend everything is exported
  | OptNotHome        -- ^ Not the best place to get docs for things
                      -- exported by this module.
  deriving (Eq, Show)


data ExportItem name

  = ExportDecl {		  		
	
      -- | The original name
      expItemName :: Name, 

      -- | A declaration
      expItemDecl :: LHsDecl name, 
			       
      -- | Maybe a doc comment
      expItemMbDoc :: Maybe (HsDoc name),

      -- | Instances relevant to this declaration
      expItemInstances :: [InstHead name]
	
	  }	-- ^ An exported declaration 
		    
  | ExportNoDecl {
	  -- | The original name
      expItemName :: Name,

      -- | Where to link to
      expItemLinkTarget :: name,

      -- | Subordinate names
      expItemSubs :: [name]

		} -- ^ An exported entity for which we have no 
          -- documentation (perhaps because it resides in
          -- another package)

  | ExportGroup { 

      -- | Section level (1, 2, 3, ... )
      expItemSectionLevel :: Int,

      -- | Section id (for hyperlinks)
      expItemSectionId :: String,     
			
      -- | Section heading text
      expItemSectionText :: HsDoc name

    } -- ^ A section heading

  | ExportDoc (HsDoc name) -- ^ Some documentation

  | ExportModule Module    -- ^ A cross-reference to another module


type InstHead name = ([HsPred name], name, [HsType name])
type ModuleMap     = Map Module HaddockModule
type DocMap        = Map Name (HsDoc DocName)
type DocEnv        = Map Name Name


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

  hmod_export_items       :: [ExportItem Name],
  hmod_rn_export_items    :: [ExportItem DocName],

  -- | All the names that are defined in this module
  hmod_locals             :: [Name],

  -- | All the names that are exported by this module
  hmod_exports            :: [Name],

  -- | All the visible names exported by this module
  -- For a name to be visible, it has to:
  -- - be exported normally, and not via a full module re-exportation.
  -- - have a declaration in this module or any of it's imports, with the    
  --   exception that it can't be from another package.
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
