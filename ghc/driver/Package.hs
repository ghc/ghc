module Package where

import Pretty

data Package = Package {
		import_dirs    :: [String],
      		library_dirs   :: [String],
      		libraries      :: [String],
      		include_dir    :: String,
		c_include      :: String,
      		package_deps   :: [String],
      		extra_ghc_opts :: String,
      		extra_cc_opts  :: String,
      		extra_ld_opts  :: String
     		}
  deriving (Read, Show)

listPkgs :: [(String,Package)] -> String
listPkgs pkgs = render (fsep (punctuate comma (map (text . fst) pkgs)))

dumpPackages :: [(String,Package)] -> String
dumpPackages pkgs = 
   render (brackets (vcat (punctuate comma (map dumpPkg pkgs))))

dumpPkg (name, pkg) = parens (hang (text (show name) <> comma) 
				2 (dumpPkgGuts pkg))

dumpPkgGuts (Package
	{ import_dirs    = import_dirs    
      	, library_dirs   = library_dirs   
      	, libraries      = libraries      
      	, include_dir    = include_dir    
	, c_include      = c_include      
      	, package_deps   = package_deps   
      	, extra_ghc_opts = extra_ghc_opts 
      	, extra_cc_opts  = extra_cc_opts  
      	, extra_ld_opts  = extra_ld_opts   })
   = text "Package" $$ nest 3 (braces (
   	sep (punctuate comma [
   	   hang (text "import_dirs ="     ) 2 (pprStrs import_dirs),
   	   hang (text "library_dirs = "   ) 2 (pprStrs library_dirs),
   	   hang (text "libraries = "      ) 2 (pprStrs libraries),
   	   hang (text "include_dir = "    ) 2 (text (show include_dir)),
   	   hang (text "c_include = "      ) 2 (text (show c_include)),
   	   hang (text "package_deps = "   ) 2 (pprStrs package_deps),
   	   hang (text "extra_ghc_opts = " ) 2 (text (show extra_ghc_opts)),
   	   hang (text "extra_cc_opts = "  ) 2 (text (show extra_cc_opts)),
   	   hang (text "extra_ld_opts = "  ) 2 (text (show extra_ld_opts))
   	])))

pprStrs strs = brackets (sep (punctuate comma (map (text . show) strs)))
