%
% (c) The University of Glasgow, 2000
%
\section[CmStaticInfo]{Session-static info for the Compilation Manager}

\begin{code}
module CmStaticInfo ( Path, ModName, PkgName,
		      Package(..), PCI(..), mkPCI )
where

#include "HsVersions.h"

import List		( nub )
import Char		( isUpper )
import Directory	( getDirectoryContents )
\end{code}

\begin{code}
type Path    = String
type ModName = String
type PkgName = String

data PCI 
   = PCI { 
        raw_package_info :: [Package],  -- contents of packages.conf
        module_table     :: [(ModName, PkgName, Path)]
                            -- maps each available module to pkg and path
     }

-- copied from the driver
data Package
   = Package {
        name            :: String,
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
  deriving Read

mkPCI :: [Package] -> IO PCI
mkPCI raw_package_info
   = do mtab <- mk_module_table raw_package_info
        return (PCI { raw_package_info = raw_package_info,
                      module_table = mtab })

mk_module_table :: [Package] -> IO [(ModName,PkgName,Path)]
mk_module_table raw_info
   = do 
        -- the list of directories where package interfaces are
        let p_i_dirs :: [(PkgName,Path)]
            p_i_dirs = concatMap nm_and_paths raw_info

        -- interface names in each directory
        ifacess <- mapM ifaces_in_dir p_i_dirs
        let iface_table :: [(ModName,PkgName,Path)] 
            iface_table = concat ifacess

        -- ToDo: allow a range of home package directories
        return iface_table
     where
        nm_and_paths :: Package -> [(PkgName,Path)]
        nm_and_paths package 
           = [(name package, path) | path <- nub (import_dirs package)]

        ifaces_in_dir :: (PkgName,Path) -> IO [(ModName,PkgName,Path)]
        ifaces_in_dir (pkgname,path)
           = getDirectoryContents path >>= \ entries ->
             return [(zap_hi if_nm, pkgname, path) 
                    | if_nm <- entries, looks_like_iface_name if_nm]
        looks_like_iface_name e
           = not (null e) && isUpper (head e) 
                          && take 3 (reverse e) == "ih."
        zap_hi 
           = reverse . drop 3 . reverse

\end{code}
