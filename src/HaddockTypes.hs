--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module HaddockTypes (
  -- * Module interfaces
  NameEnv, Interface(..), ExportItem(..), ModuleMap,
  DocOption(..),

 ) where

import FiniteMap
import HsSyn

-- ---------------------------------------------------------------------------
-- Describing a module interface

type NameEnv   = FiniteMap HsName HsQName

data Interface 
  = Interface {
	iface_filename :: FilePath,
		-- ^ the filename that contains the source code for this module

	iface_env :: NameEnv,
		-- ^ environment mapping names to *original* names

	iface_exports :: [ExportItem],
		-- ^ the exports used to construct the documentation 

	iface_orig_exports :: [ExportItem],
		-- ^ the exports used to construct the documentation
		-- (with orig names, not import names)

	iface_decls :: FiniteMap HsName HsDecl,
		-- ^ decls from this module (only)
		-- restricted to only those bits exported.
		-- the map key is the "main name" of the decl.

	iface_info :: Maybe ModuleInfo,
		-- ^ information from the module header

	iface_doc :: Maybe Doc,
		-- ^ documentation from the module header

	iface_options :: [DocOption]
		-- ^ module-wide doc options
  }

data DocOption = OptHide | OptPrune | OptIgnoreExports 
  deriving (Eq)

type DocString = String

data ExportItem 
  = ExportDecl
	HsDecl		-- a declaration (with doc annotations)

  | ExportGroup		-- a section heading
	Int		-- section level (1, 2, 3, ... )
	String		-- section "id" (for hyperlinks)
	Doc		-- section heading text

  | ExportDoc		-- some documentation
	Doc

  | ExportModule	-- a cross-reference to another module
	Module

type ModuleMap = FiniteMap Module Interface

