module Package where

import Pretty

data Package = Package {
		import_dirs     :: [String],
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
  deriving (Read, Show)

listPkgs :: [(String,Package)] -> String
listPkgs pkgs = render (fsep (punctuate comma (map (text . fst) pkgs)))

dumpPackages :: [(String,Package)] -> String
dumpPackages pkgs = 
   render (brackets (vcat (punctuate comma (map dumpPkg pkgs))))

dumpPkg :: (String,Package) -> Doc
dumpPkg (name, pkg) =
   parens (hang (text (show name) <> comma) 2 (dumpPkgGuts pkg))

dumpPkgGuts :: Package -> Doc
dumpPkgGuts pkg =
   text "Package" $$ nest 3 (braces (
      sep (punctuate comma [
         dumpField "import_dirs"     (import_dirs     pkg),
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
