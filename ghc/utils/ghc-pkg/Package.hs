-----------------------------------------------------------------------------
-- $Id: Package.hs,v 1.1 2001/03/15 15:51:38 simonmar Exp $
--
-- Package configuration defn.
-----------------------------------------------------------------------------

#ifdef PKG_TOOL
module Package ( 
	PackageConfig(..), defaultPackageConfig
#ifdef WANT_PRETTY
	,listPkgs 	-- :: [PackageConfig] -> String
	,dumpPackages	-- :: [PackageConfig] -> String
#endif
 ) where
#endif

#ifdef WANT_PRETTY
import Pretty
#endif

data PackageConfig
   = Package {
	name            :: String,
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
	extra_ld_opts   :: [String]
     }
#ifdef PKG_TOOL
	deriving (Read)
#endif

defaultPackageConfig
   = Package {
	name = error "defaultPackage",
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
	extra_ld_opts   = []
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
         dumpField "extra_ld_opts"   (extra_ld_opts   pkg)
      ])))

dumpField :: String -> [String] -> Doc
dumpField name val =
   hang (text name <+> equals) 2
        (brackets (sep (punctuate comma (map (text . show) val))))
#endif

