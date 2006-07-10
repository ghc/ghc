--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module HaddockTypes (
  -- * Module interfaces
  NameEnv, Interface(..), ExportItem(..), ModuleMap,

  -- * Misc types
  DocOption(..), InstHead,
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
  deriving (Eq)

data ExportItem 
  = ExportDecl
	GHC.Name	-- the original name
	GHC.HsDecl	-- a declaration (with doc annotations)
	[InstHead]	-- instances relevant to this declaration

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

type InstHead = (HsContext,HsAsst)

type ModuleMap = Map Module Interface
type ModuleMap2 = Map GHC.Module HaddockModule

data HaddockModule = HM {
  hmod_options      :: [DocOption],
  hmod_decls        :: Map Name GHC.HsDecl,
  hmod_orig_exports :: [ExportItem]
}
