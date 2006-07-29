--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module HaddockTypes (
  -- * Module interfaces
  NameEnv, Interface(..), ExportItem(..), ExportItem2(..), ModuleMap, ModuleMap2,
  HaddockModule(..), 
  -- * Misc types
  DocOption(..), InstHead, InstHead2,
  DocName(..),
 ) where

import HsSyn2

import qualified GHC as GHC

import Data.Map

-- ---------------------------------------------------------------------------
-- Describing a module interface

type NameEnv   = Map HsName HsQName

data Interface 
  = Interface {
	iface_filename :: FilePath,
		-- ^ the filename that contains the source code for this module

	iface_orig_filename :: FilePath,
		-- ^ the original filename for this module, which may be
                -- different to the 'iface_filename' (for example the original
                -- file may have had a .lhs or .hs.pp extension).

	iface_module :: Module,

	iface_package :: Maybe String,

	iface_env :: NameEnv,
		-- ^ environment mapping exported names to *original* names

	iface_reexported :: [HsName],
		-- ^ For names exported by this module, but not
		-- actually documented in this module's documentation
		-- (perhaps because they are reexported via 'module M'
		-- in the export list), this mapping gives the
		-- location of documentation for the name in another
		-- module.

	iface_sub :: Map HsName [HsName],
		-- ^ maps names to "subordinate" names 
		-- (eg. tycon to constrs & fields, class to methods)

	iface_exports :: [ExportItem],
		-- ^ the exports used to construct the documentation 

	iface_orig_exports :: [ExportItem],
		-- ^ the exports used to construct the documentation
		-- (with orig names, not import names)

	iface_decls :: Map HsName HsDecl,
		-- ^ decls from this module (only)
		-- restricted to only those bits exported.
		-- the map key is the "main name" of the decl.

	iface_insts :: [HsDecl],
		-- ^ instances from this module

	iface_info :: ModuleInfo,
		-- ^ information from the module header

	iface_doc :: Maybe Doc,
		-- ^ documentation from the module header

	iface_options :: [DocOption]
		-- ^ module-wide doc options
  }

data DocOption
  = OptHide		-- this module should not appear in the docs
  | OptPrune
  | OptIgnoreExports	-- pretend everything is exported
  | OptNotHome		-- not the best place to get docs for things
		 	-- exported by this module.
  deriving (Eq, Show)

data ExportItem 
  = ExportDecl
	HsQName	      -- the original name
	HsDecl        -- a declaration (with doc annotations)
	[InstHead]    -- instances relevant to this declaration

  | ExportNoDecl	-- an exported entity for which we have no documentation
			-- (perhaps becuase it resides in another package)
	HsQName		-- the original name
	HsQName		-- where to link to
	[HsQName]	-- subordinate names

  | ExportGroup		-- a section heading
	Int		-- section level (1, 2, 3, ... )
	String		-- section "id" (for hyperlinks)
	Doc		-- section heading text

  | ExportDoc		-- some documentation
	Doc

  | ExportModule	-- a cross-reference to another module
	Module

data ExportItem2 name
  = ExportDecl2
        GHC.Name	      -- the original name
	(GHC.LHsDecl name) -- a declaration
        (Maybe (GHC.HsDoc name))       -- maybe a doc comment
	[InstHead2 name]	      -- instances relevant to this declaration

  | ExportNoDecl2	-- an exported entity for which we have no documentation
			-- (perhaps becuase it resides in another package)
	GHC.Name	-- the original name
	name		-- where to link to
	[name]	-- subordinate names

  | ExportGroup2		-- a section heading
	Int		-- section level (1, 2, 3, ... )
	String		-- section "id" (for hyperlinks)
	(GHC.HsDoc name)		-- section heading text

  | ExportDoc2		-- some documentation
	(GHC.HsDoc name)

  | ExportModule2	-- a cross-reference to another module
	GHC.Module

type InstHead = (HsContext,HsAsst)

type InstHead2 name = ([GHC.HsPred name], name, [GHC.HsType name])

type ModuleMap = Map Module Interface
type ModuleMap2 = Map GHC.Module HaddockModule

data DocName = Link GHC.Name | NoLink GHC.Name

data HaddockModule = HM {

-- | A value to identify the module
  hmod_mod                :: GHC.Module,

-- | The documentation header for this module
  hmod_doc                :: Maybe (GHC.HsDoc GHC.Name),

-- | The Haddock options for this module (prune, ignore-exports, etc)
  hmod_options            :: [DocOption],

  hmod_exported_decl_map  :: Map GHC.Name (GHC.LHsDecl GHC.Name),
  hmod_doc_map            :: Map GHC.Name (GHC.HsDoc GHC.Name),  
  hmod_export_items       :: [ExportItem2 GHC.Name],

-- | All the names that are defined in this module
  hmod_locals             :: [GHC.Name],

-- | All the names that are exported by this module
  hmod_exports            :: [GHC.Name],

-- | All the visible names exported by this module
-- For a name to be visible, it has to:
-- - be exported normally, and not via a full module re-exportation.
-- - have a declaration in this module or any of it's imports, with the exception
--   that it can't be from another package.
-- Basically, a visible name is a name that will show up in the documentation.
-- for this module.
  hmod_visible_exports    :: [GHC.Name],

  hmod_sub_map            :: Map GHC.Name [GHC.Name],

-- | The instances exported by this module
  hmod_instances          :: [GHC.Instance]
}
