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

pprPackage :: [(String,Package)] -> String
pprPackage pkgs = render (brackets (vcat (punctuate comma (map pprPkg pkgs))))

pprPkg (name, (Package
	{ import_dirs    = import_dirs    
      	, library_dirs   = library_dirs   
      	, libraries      = libraries      
      	, include_dir    = include_dir    
	, c_include      = c_include      
      	, package_deps   = package_deps   
      	, extra_ghc_opts = extra_ghc_opts 
      	, extra_cc_opts  = extra_cc_opts  
      	, extra_ld_opts  = extra_ld_opts   }))
   = parens ( 
	text (show name) <> comma
    <+> text "Package" <+> braces (
   	vcat [
   	   text "import_dirs = "    <> text (show import_dirs) <> comma,
   	   text "library_dirs = "   <> text (show library_dirs) <> comma,
   	   text "libraries = "      <> text (show libraries) <> comma,
   	   text "include_dir = "    <> text (show include_dir) <> comma,
   	   text "c_include = "      <> text (show c_include) <> comma,
   	   text "package_deps = "   <> text (show package_deps) <> comma,
   	   text "extra_ghc_opts = " <> text (show extra_ghc_opts) <> comma,
   	   text "extra_cc_opts = "  <> text (show extra_cc_opts) <> comma,
   	   text "extra_ld_opts = "  <> text (show extra_ld_opts)
   	])
    )
