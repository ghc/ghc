#include "../includes/config.h"
#include "../includes/Derived.h"

module Main (main) where

import Utils

import IO
import System
import Package

main :: IO ()
main = do
  args <- getArgs
  case args of
     ("install":rest)  -> do { putStrLn (dumpPackages (package_details True rest)) }
     ("in-place":rest) -> do { putStrLn (dumpPackages (package_details False rest)) }
     _ -> do hPutStr stderr "usage: pkgconf (install | in-place) ...\n"
             exitWith (ExitFailure 1)

-- The compiler automatically replaces the string "$libdir" at the
-- beginning of a path with the directory passed to the compiler via
-- the -B<dir> flag.  Absolute path names will be unchanged.
--
-- This is how we make package.conf independent of GHC's installation
-- location.

package_details :: Bool -> [String] -> [PackageConfig]
package_details installing
 [ cTARGETPLATFORM
 , cCURRENT_DIR
 , cHaveLibGmp
 , cLibsReadline
 , cGHC_LIB_DIR
 , cGHC_RUNTIME_DIR
 , cGHC_UTILS_DIR
 , cGHC_INCLUDE_DIR
 ] =

 [
        Package {
	name           = "gmp",  -- GMP is at the bottom of the heap
        import_dirs    = [],
        source_dirs    = [],
        library_dirs   = if cHaveLibGmp == "YES"
                            then []
                            else if installing
                                    then [ "$libdir" ]
                                    else [ ghc_src_dir cGHC_RUNTIME_DIR ++ "/gmp" ],
  	hs_libraries   = [],
        extra_libraries = [ "gmp" ],
        include_dirs   = [],
        c_includes     = [],
        package_deps   = [],
        extra_ghc_opts = [],
        extra_cc_opts  = [],
        extra_ld_opts  = []
        },

        Package {
	name           = "rts",  -- The RTS is just another package!
        import_dirs    = [],
        source_dirs    = [],
        library_dirs   = if installing
                            then [ "$libdir" ]
                            else [ ghc_src_dir cGHC_RUNTIME_DIR ],
        hs_libraries      = [ "HSrts" ],
	extra_libraries   =
			      "m":		-- for ldexp()
#ifdef mingw32_TARGET_OS
			      "winmm":		-- for the threadDelay timer
			      "wsock32":	-- for the linker
#endif
#ifdef USING_LIBBFD
			      "bfd": "iberty":	-- for debugging
#endif
			    [],
        include_dirs   = if installing
                            then [ "$libdir/include"
#ifdef mingw32_TARGET_OS
				 , "$libdir/include/mingw"
#endif
				 ]
                            else [ ghc_src_dir cGHC_INCLUDE_DIR ],
        c_includes     = [ "Stg.h" ],           -- ha!
        package_deps   = [ "gmp" ],
        extra_ghc_opts = [],
        extra_cc_opts  = [],
                -- the RTS forward-references to a bunch of stuff in the prelude,
                -- so we force it to be included with special options to ld.
        extra_ld_opts  =
	   foldr (\ x xs -> "-u" : x : xs) []
                 (map (
#ifndef LEADING_UNDERSCORE
		          ""
#else
			  "_"
#endif
                          ++ ) [
           "PrelBase_Izh_static_info"
         , "PrelBase_Czh_static_info"
         , "PrelFloat_Fzh_static_info"
         , "PrelFloat_Dzh_static_info"
         , "PrelPtr_Ptr_static_info"
         , "PrelWord_Wzh_static_info"
         , "PrelInt_I8zh_static_info"
         , "PrelInt_I16zh_static_info"
         , "PrelInt_I32zh_static_info"
         , "PrelInt_I64zh_static_info"
         , "PrelWord_W8zh_static_info"
         , "PrelWord_W16zh_static_info"
         , "PrelWord_W32zh_static_info"
         , "PrelWord_W64zh_static_info"
         , "PrelStable_StablePtr_static_info"
         , "PrelBase_Izh_con_info"
         , "PrelBase_Czh_con_info"
         , "PrelFloat_Fzh_con_info"
         , "PrelFloat_Dzh_con_info"
         , "PrelPtr_Ptr_con_info"
         , "PrelStable_StablePtr_con_info"
         , "PrelBase_False_closure"
         , "PrelBase_True_closure"
         , "PrelPack_unpackCString_closure"
         , "PrelIOBase_stackOverflow_closure"
         , "PrelIOBase_heapOverflow_closure"
         , "PrelIOBase_NonTermination_closure"
         , "PrelIOBase_BlockedOnDeadMVar_closure"
         , "PrelWeak_runFinalizzerBatch_closure"
         , "__init_Prelude"
         ])
        },

        Package {
        name           = "std",  -- The Prelude & Standard Hs_libraries
	import_dirs    = if installing
                            then [ "$libdir/imports/std" ]
                            else [ ghc_src_dir cGHC_LIB_DIR ++ "/std" ],
        source_dirs    = [],
        library_dirs   = if installing
                            then [ "$libdir" ]
                            else [ ghc_src_dir cGHC_LIB_DIR ++ "/std"
                                 , ghc_src_dir cGHC_LIB_DIR ++ "/std/cbits" ],

        hs_libraries      = 
#                           ifndef mingw32_TARGET_OS
                            [ "HSstd" ]
#                           else
                            -- This splitting is the subject of a totally 
                            -- horrible hack, which glues HSstd1 and HSstd2 
                            -- back into HSstd for the purposes of static linking.
                            -- See DriverState.getPackageLibraries for details.
                            [ "HSstd1", "HSstd2" ]
#                           endif
                            ,
	extra_libraries   = [ "HSstd_cbits" ] ++
#                           ifdef mingw32_TARGET_OS
                            [ "wsock32", "msvcrt" ]
#                           else
                            [ ]
#                           endif
                            ,
        include_dirs   = if installing
                            then []
                            else [ ghc_src_dir cGHC_LIB_DIR ++ "/std/cbits" ],
        c_includes     = [ "HsStd.h" ],
        package_deps   = [ "rts" ],
        extra_ghc_opts = [],
        extra_cc_opts  = [],
        extra_ld_opts  = []
        },

         Package { 
         name           = "lang",
	 import_dirs    = if installing
                             then [ "$libdir/imports/lang" ]
                             else [ "$libdir/hslibs/lang"
                                  , "$libdir/hslibs/lang/monads" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hslibs/lang"
                                  , "$libdir/hslibs/lang/cbits" ],
         hs_libraries      = [ "HSlang" ],
	 extra_libraries   = [ "HSlang_cbits" ],
         include_dirs   = if installing
                             then []
                             else [ "$libdir/hslibs/lang/cbits" ],
         c_includes     = [ "HsLang.h" ],
         package_deps   = [],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = [
#ifndef LEADING_UNDERSCORE
		          "-u", "Addr_Azh_static_info"
#else
			  "-u", "_Addr_Azh_static_info"
#endif
			]
        },

         Package {
	 name           = "concurrent",
         import_dirs    = if installing
                             then [ "$libdir/imports/concurrent" ]
                             else [ "$libdir/hslibs/concurrent" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hslibs/concurrent" ],
         hs_libraries      = [ "HSconcurrent" ],
	 extra_libraries   = [],
         include_dirs   = if installing
                             then []
                             else [ "$libdir/hslibs/concurrent/cbits" ],
         c_includes     = [],
         package_deps   = [ "lang" ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        },

         Package {
         name           = "data",
         import_dirs    = if installing
                             then [ "$libdir/imports/data" ]
                             else [ "$libdir/hslibs/data"
                                  , "$libdir/hslibs/data/edison"
                                  , "$libdir/hslibs/data/edison/Assoc"
                                  , "$libdir/hslibs/data/edison/Coll"
                                  , "$libdir/hslibs/data/edison/Seq" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hslibs/data" ],
         hs_libraries      = [ "HSdata" ],
	 extra_libraries   = [],
         include_dirs   = if installing
                             then []
                             else [ "$libdir/hslibs/data/cbits" ],
         c_includes     = [],
         package_deps   = [ "lang", "util" ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        },

         Package {
         name           = "net",
         import_dirs    = if installing
                             then [ "$libdir/imports/net" ]
                             else [ "$libdir/hslibs/net" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hslibs/net"
                                  , "$libdir/hslibs/net/cbits" ],
         hs_libraries      = [ "HSnet" ],
	 extra_libraries   = if suffixMatch "solaris2" cTARGETPLATFORM
                                then [ "nsl",  "socket" ]
                                else []
                             ,
         include_dirs   = if installing
                             then []
                             else [ "$libdir/hslibs/net/cbits" ],
         c_includes     = [ "HsNet.h" ],
         package_deps   = [ "lang", "text", "concurrent" ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        },

         Package {
         name           = "posix",
         import_dirs    = if installing
                             then [ "$libdir/imports/posix" ]
                             else [ "$libdir/hslibs/posix" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hslibs/posix"
                                  , "$libdir/hslibs/posix/cbits" ],
         hs_libraries      = [ "HSposix" ],
	 extra_libraries   = [ "HSposix_cbits" ],
         include_dirs   = if installing
                             then []
                             else [ "$libdir/hslibs/posix/cbits" ],
         c_includes     = [ "HsPosix.h" ],
         package_deps   = [ "lang" ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        },

         Package {
         name           = "text",
         import_dirs    = if installing
                             then [ "$libdir/imports/text" ]
                             else [ "$libdir/hslibs/text" 
                                  , "$libdir/hslibs/text/html" 
                                  , "$libdir/hslibs/text/HaXml/lib" 
                                  , "$libdir/hslibs/text/parsec" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hslibs/text"
                                  , "$libdir/hslibs/text/cbits" ],
         hs_libraries      = [ "HStext" ],
	 extra_libraries   = [ "HStext_cbits" ],
         include_dirs   = if installing
                             then []
                             else [ "$libdir/hslibs/text/cbits" ],
         c_includes     = [ "HsText.h" ],
         package_deps   = [ "lang" ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        },

         Package {
         name           = "util",
         import_dirs    = if installing
                             then [ "$libdir/imports/util" ]
                             else [ "$libdir/hslibs/util"
                                  , "$libdir/hslibs/util/check" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hslibs/util"
                                  , "$libdir/hslibs/util/cbits" ],
         hs_libraries      = [ "HSutil" ],
	 extra_libraries   = [ "HSutil_cbits" ] 
#ifndef mingw32_TARGET_OS
                             ++ words cLibsReadline
#endif
                             ,
         include_dirs   = if installing
                             then []
                             else [ "$libdir/hslibs/util/cbits" ],
         c_includes     = [ "HsUtil.h" ],
         package_deps   = [ "lang", "concurrent"
#ifndef mingw32_TARGET_OS
			    , "posix"
#endif
			  ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        },

        -- no cbits at the moment, we'll need to add one if this library
        -- ever calls out to any C libs.
         Package {
         name           = "hssource",
         import_dirs    = if installing
                             then [ "$libdir/imports/hssource" ]
                             else [ "$libdir/hslibs/hssource" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hslibs/hssource" ],
         hs_libraries      = [ "HShssource" ],
	 extra_libraries   = [],
         include_dirs   = [],
         c_includes     = [],
         package_deps   = [ "text" ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        },

         Package {
	 name         = "greencard",
         import_dirs    = if installing
                             then [ "$libdir/imports/greencard" ]
                   	     else [ "$libdir/green-card/lib/ghc" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/green-card/lib/ghc" ],
         hs_libraries      = [ "HSgreencard" ],
         extra_libraries   = [],
         include_dirs   = [],
         c_includes     = [],
         package_deps   = [ "lang" ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = [],
        }

#if defined(mingw32_TARGET_OS) || defined(cygwin32_TARGET_OS)
         ,Package {
         name         = "win32",
	 import_dirs    = if installing
                             then [ "$libdir/imports/win32" ]
                             else [ "$libdir/hslibs/win32" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hslibs/win32" ],
         hs_libraries      = [ "HSwin32" ],
	 extra_libraries   = [ "user32",  "gdi32", "winmm" ],
         include_dirs   = [],
         c_includes     = [],           -- ???
         package_deps   = [ "lang" ], -- greencard now built in
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        },

         Package {
         name           = "com",
         import_dirs    = if installing
                             then [ "$libdir/imports/com" ]
                             else [ "$libdir/hdirect/lib" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hdirect/lib" ],
         hs_libraries      = [ "HScom" ],
	 extra_libraries   = [ "user32",  "ole32",  "oleaut32", "advapi32" ],
         include_dirs   = [],
         c_includes     = [],           -- ???
         package_deps   = [ "lang" ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        }
#endif

         ,Package {
         name           = "xlib",
         import_dirs    = if installing
                             then [ "$libdir/imports/xlib" ]
                             else [ "$libdir/hslibs/xlib" ],
         source_dirs    = [],
         library_dirs   = if installing
                             then [ "$libdir" ]
                             else [ "$libdir/hslibs/xlib"
                                  , "$libdir/hslibs/xlib/cbits" ],
         hs_libraries      = [ "HSxlib" ],
	 extra_libraries   = [ "HSxlib_cbits", "X11" ],
         include_dirs   = if installing
                             then []
                             else [ "$libdir/hslibs/xlib/cbits" ],
         c_includes     = [ "HsXlib.h" ],
         package_deps   = [ "greencard" ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        }

         ,Package {
         name           = "HGL",
         import_dirs    = if installing
                             then [ "$libdir/imports/HGL" ]
                             else [ "$libdir/hslibs/graphics/lib/x11" ],
         source_dirs    = [],
         library_dirs   = [],
         hs_libraries   = [ "HSHGL" ],
	 extra_libraries= [],
         include_dirs   = [],
         c_includes     = [],
         package_deps   = [ "xlib", "concurrent" ],
         extra_ghc_opts = [],
         extra_cc_opts  = [],
         extra_ld_opts  = []
        }

   ]
  where
	ghc_src_dir :: String -> String
	ghc_src_dir path = "$libdir/" ++ cCURRENT_DIR ++ '/':path
