-----------------------------------------------------------------------------
-- $Id: Package.hs,v 1.6 2002/12/18 16:29:34 simonmar Exp $
--
-- Package configuration defn.
-----------------------------------------------------------------------------

#ifdef PKG_TOOL
module Package ( 
	PackageConfig(..), defaultPackageConfig
#ifdef WANT_PRETTY
	, listPkgs	 	-- :: [PackageConfig] -> String
	, dumpPackages		-- :: [PackageConfig] -> String
	, dumpPkgGuts		-- :: PackageConfig -> Doc
	, dumpFieldContents	-- :: [String] -> Doc
#endif
 ) where
#endif

#ifdef WANT_PRETTY
#if __GLASGOW_HASKELL__ >= 504 && !defined(INTERNAL_PRETTY)
import Text.PrettyPrint
#else
import Pretty
#endif
#endif

data PackageConfig
   = Package {
	name            :: String,
	auto		:: Bool,
	import_dirs     :: [String],
	source_dirs     :: [String],
	library_dirs    :: [String],
	hs_libraries    :: [String],
	extra_libraries :: [String],
	include_dirs    :: [String],
	c_includes      :: [String],
	package_deps    :: [String],
	extra_ghc_opts  :: [String],
	extra_cc_opts   :: [String],
	extra_ld_opts   :: [String],
	framework_dirs  :: [String], -- ignored everywhere but on Darwin/MacOS X
	extra_frameworks:: [String]  -- ignored everywhere but on Darwin/MacOS X
     }

defaultPackageConfig
   = Package {
	name = error "defaultPackage",
	auto = False,
	import_dirs     = [],
	source_dirs     = [],
	library_dirs    = [],
	hs_libraries    = [],
	extra_libraries = [],
	include_dirs    = [],
	c_includes      = [],
	package_deps    = [],
	extra_ghc_opts  = [],
	extra_cc_opts   = [],
	extra_ld_opts   = [],
	framework_dirs  = [],
	extra_frameworks= []
    }

-----------------------------------------------------------------------------
-- Pretty printing package info

#ifdef WANT_PRETTY
listPkgs :: [PackageConfig] -> String
listPkgs pkgs = render (fsep (punctuate comma (map (text . name) pkgs)))

dumpPackages :: [PackageConfig] -> String
dumpPackages pkgs = 
   render (brackets (vcat (punctuate comma (map dumpPkgGuts pkgs))))

dumpPkgGuts :: PackageConfig -> Doc
dumpPkgGuts pkg =
   text "Package" $$ nest 3 (braces (
      sep (punctuate comma [
         text "name = " <> text (show (name pkg)),
	 text "auto = " <> text (show (auto pkg)),
         dumpField "import_dirs"     (import_dirs     pkg),
         dumpField "source_dirs"     (source_dirs     pkg),
         dumpField "library_dirs"    (library_dirs    pkg),
         dumpField "hs_libraries"    (hs_libraries    pkg),
         dumpField "extra_libraries" (extra_libraries pkg),
         dumpField "include_dirs"    (include_dirs    pkg),
         dumpField "c_includes"      (c_includes      pkg),
         dumpField "package_deps"    (package_deps    pkg),
         dumpField "extra_ghc_opts"  (extra_ghc_opts  pkg),
         dumpField "extra_cc_opts"   (extra_cc_opts   pkg),
         dumpField "extra_ld_opts"   (extra_ld_opts   pkg),
         dumpField "framework_dirs"  (framework_dirs   pkg),
         dumpField "extra_frameworks"(extra_frameworks pkg)
      ])))

dumpField :: String -> [String] -> Doc
dumpField name val = hang (text name <+> equals) 2  (dumpFieldContents val)

dumpFieldContents :: [String] -> Doc
dumpFieldContents val = brackets (sep (punctuate comma (map (text . show) val)))
#endif

