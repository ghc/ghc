module Main (main) where

import IOExts
import IO
import System
import Config
import Package

main = do
  args <- getArgs
  case args of
	[ "install"  ] -> do { putStr (pprPackage (package_details True)) }
	[ "in-place" ] -> do { putStr (pprPackage (package_details False)) }
	_ -> do hPutStr stderr "usage: pkgconf (install | in-place)\n"
	        exitWith (ExitFailure 1)

package_details :: Bool -> [(String,Package)]
package_details installing =
 [
      ( "gmp",	-- GMP is at the bottom of the heap
	Package {
	import_dirs    = [],
	library_dirs   = if _HaveLibGmp == "YES"
			     then []
			     else if installing 
				   then [ _libdir ]
				   else [ ghc_src_dir _GHC_RUNTIME_DIR ++ "/gmp" ],
	libraries      = [ "gmp" ],
	include_dir    = "",
	c_include      = "",
	package_deps   = [],
	extra_ghc_opts = "",
	extra_cc_opts  = "",
	extra_ld_opts  = ""
	}
       ),

      ( "rts",	-- The RTS is just another package!
	Package {
	import_dirs    = [],
	library_dirs   = [ if installing 
	       		      then _libdir
	       		      else ghc_src_dir _GHC_RUNTIME_DIR ],
	libraries      = [ "HSrts" ],
	include_dir    = if installing 
			    then "" 
			    else ghc_src_dir _GHC_INCLUDE_DIR,
	c_include      = "Stg.h",		-- ha!
	package_deps   = [ "gmp" ],
	extra_ghc_opts = "",
	extra_cc_opts  = "",
		-- the RTS forward-references to a bunch of stuff in the prelude,
		-- so we force it to be included with special options to ld.
	extra_ld_opts  = unwords [
         "-u PrelMain_mainIO_closure",
         "-u PrelBase_Izh_static_info",
         "-u PrelBase_Czh_static_info",
         "-u PrelFloat_Fzh_static_info",
         "-u PrelFloat_Dzh_static_info",
         "-u PrelAddr_Azh_static_info",
         "-u PrelAddr_Wzh_static_info",
         "-u PrelAddr_I64zh_static_info",
         "-u PrelAddr_W64zh_static_info",
         "-u PrelStable_StablePtr_static_info",
	 "-u PrelBase_Izh_con_info",
         "-u PrelBase_Czh_con_info",
         "-u PrelFloat_Fzh_con_info",
         "-u PrelFloat_Dzh_con_info",
         "-u PrelAddr_Azh_con_info",
         "-u PrelAddr_Wzh_con_info",
         "-u PrelAddr_I64zh_con_info",
         "-u PrelAddr_W64zh_con_info",
         "-u PrelStable_StablePtr_con_info",
         "-u PrelBase_False_closure",
         "-u PrelBase_True_closure",
         "-u PrelPack_unpackCString_closure",
         "-u PrelException_stackOverflow_closure",
         "-u PrelException_heapOverflow_closure",
         "-u PrelException_NonTermination_closure",
         "-u PrelException_PutFullMVar_closure",
         "-u PrelException_BlockedOnDeadMVar_closure",
	 "-u PrelWeak_runFinalizzerBatch_closure",
         "-u __init_Prelude",
         "-u __init_PrelMain"
         ]
	}
      ),

      ( "std",	-- The Prelude & Standard Libraries
	Package {
	import_dirs    = [ if installing 
	     	       	     then _libdir ++ "/imports/std"
	     	       	     else ghc_src_dir _GHC_LIB_DIR ++ "/std" ],
	library_dirs   = if installing 
	       	       	   then [ _libdir ]
	       	       	   else [ ghc_src_dir _GHC_LIB_DIR ++ "/std"
				, ghc_src_dir _GHC_LIB_DIR ++ "/std/cbits" ],
	libraries      = [ "HSstd", "HSstd_cbits" ],
	include_dir    = if installing 
		       	   then "" 
		       	   else ghc_src_dir _GHC_LIB_DIR ++ "/std/cbits",
	c_include      = "HsStd.h",
	package_deps   = [ "rts" ],
	extra_ghc_opts = "",
	extra_cc_opts  = "",
	extra_ld_opts  = "-lm"
	}
       ),

       ( "lang",
	 Package { 
	 import_dirs    = if installing 
	       		   then [ _libdir ++ "/imports/lang" ]
	       		   else [ _FPTOOLS_TOP_ABS ++ "/hslibs/lang"
		    	        , _FPTOOLS_TOP_ABS ++ "/hslibs/lang/monads"],
	 library_dirs   = if installing 
	       		     then [ _libdir ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/lang"
				  , _FPTOOLS_TOP_ABS ++ "/hslibs/lang/cbits" ],
	 libraries      = [ "HSlang", "HSlang_cbits" ],
	 include_dir    = if installing 
			     then "" 
			     else _FPTOOLS_TOP_ABS ++ "/hslibs/lang/cbits",
	 c_include      = "HsLang.h",
	 package_deps   = [],
	 extra_ghc_opts = "",
	 extra_cc_opts  = "",
	 extra_ld_opts  = ""
	}
       ),

       ( "concurrent",
	 Package {
	 import_dirs    = [ if installing 
	       		        then _libdir ++ "/imports/concurrent"
	       		        else _FPTOOLS_TOP_ABS ++ "/hslibs/concurrent" ],
	 library_dirs   = [ if installing 
	       		        then _libdir
	       		        else _FPTOOLS_TOP_ABS ++ "/hslibs/concurrent" ],
	 libraries      = [ "HSconcurrent" ],
	 include_dir    = if installing 
			    then "" 
			    else _FPTOOLS_TOP_ABS ++ "/hslibs/concurrent/cbits",
	 c_include      = "HsConcurrent.h",
	 package_deps   = [ "lang" ],
	 extra_ghc_opts = "",
	 extra_cc_opts  = "",
	 extra_ld_opts  = ""
	}
       ),

       ( "data",
	 Package {
	 import_dirs    = if installing 
	       		     then [ _libdir ++ "/imports/data" ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/data"
		    	          , _FPTOOLS_TOP_ABS ++ "/hslibs/data/edison"
		    	          , _FPTOOLS_TOP_ABS ++ "/hslibs/data/edison/Assoc"
		    	          , _FPTOOLS_TOP_ABS ++ "/hslibs/data/edison/Coll"
		                  , _FPTOOLS_TOP_ABS ++ "/hslibs/data/edison/Seq" ],
	 library_dirs   = if installing 
	       		     then [_libdir ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/data" ],
	 libraries      = [ "HSdata" ],
	 include_dir    = if installing 
			     then "" 
			     else _FPTOOLS_TOP_ABS ++ "/hslibs/data/cbits",
	 c_include      = "HsData.h",
	 package_deps   = [ "lang" ],
	 extra_ghc_opts = "",
	 extra_cc_opts  = "",
	 extra_ld_opts  = ""
	}
       ),

       ( "net",
	 Package {
	 import_dirs    = if installing 
	       		     then [ _libdir ++ "/imports/net" ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/net" ],
	 library_dirs   = if installing 
	       		     then [ _libdir ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/net"
				  , _FPTOOLS_TOP_ABS ++ "/hslibs/net/cbits" ],
	 libraries      = [ "HSnet", "HSnet_cbits" ],
	 include_dir    = if installing 
			     then "" 
			     else _FPTOOLS_TOP_ABS ++ "/hslibs/net/cbits",
	 c_include      = "HsNet.h",
	 package_deps   = [ "lang", "text" ],
	 extra_ghc_opts = "",
	 extra_cc_opts  = "",
	 extra_ld_opts  = if postfixMatch "solaris2" _TARGETPLATFORM
				then "-lnsl -lsocket"
				else ""
	}
       ),

       ( "posix",
	 Package {
	 import_dirs    = if installing 
	       		     then [ _libdir ++ "/imports/posix" ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/posix" ],
	 library_dirs   = if installing 
	       		     then [ _libdir ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/posix"
				  , _FPTOOLS_TOP_ABS ++ "/hslibs/posix/cbits" ],
	 libraries      = [ "HSposix", "HSposix_cbits" ],
	 include_dir    = if installing 
			     then "" 
			     else _FPTOOLS_TOP_ABS ++ "/hslibs/posix/cbits",
	 c_include      = "HsPosix.h",
	 package_deps   = [ "lang" ],
	 extra_ghc_opts = "",
	 extra_cc_opts  = "",
	 extra_ld_opts  = ""
	}
       ),

       ( "text",
	 Package {
	 import_dirs    = if installing 
	       		     then [ _libdir ++ "/imports/text" ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/text" 
			 	  , _FPTOOLS_TOP_ABS ++ "/hslibs/text/html" 
			 	  , _FPTOOLS_TOP_ABS ++ "/hslibs/text/haxml/lib" 
			 	  , _FPTOOLS_TOP_ABS ++ "/hslibs/text/parsec" ],
	 library_dirs   = if installing 
	       		     then [ _libdir ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/text"
				  , _FPTOOLS_TOP_ABS ++ "/hslibs/text/cbits" ],
	 libraries      = [ "HStext", "HStext_cbits" ],
	 include_dir    = if installing 
			     then "" 
			     else _FPTOOLS_TOP_ABS ++ "/hslibs/text/cbits",
	 c_include      = "HsText.h",
	 package_deps   = [ "lang", "data" ],
	 extra_ghc_opts = "",
	 extra_cc_opts  = "",
	 extra_ld_opts  = ""
	}
       ),

       ( "util",
	 Package {
	 import_dirs    = if installing 
	       		     then [ _libdir ++ "/imports/util" ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/util"
		    	          , _FPTOOLS_TOP_ABS ++ "/hslibs/util/check" ],
	 library_dirs   = if installing 
	       		     then [ _libdir ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/util"
				  , _FPTOOLS_TOP_ABS ++ "/hslibs/util/cbits" ],
	 libraries      = [ "HSutil", "HSutil_cbits" ],
	 include_dir    = if installing 
			     then "" 
			     else _FPTOOLS_TOP_ABS ++ "/hslibs/util/cbits",
	 c_include      = "HsUtil.h",
	 package_deps   = ["lang", "concurrent", "posix"],
	 extra_ghc_opts = "",
	 extra_cc_opts  = "",
	 extra_ld_opts  = ""
	}
       ),

       ( "win32",
	 Package {
	 import_dirs    = if installing 
	       		     then [ _libdir ++ "/imports/win32" ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/win32/src" ],
	 library_dirs   = if installing 
	       		     then [ _libdir ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hslibs/win32/src" ],
	 libraries      = [ "HSwin32" ],
	 include_dir    = "",
	 c_include      = "",		-- ???
	 package_deps   = ["lang"],
	 extra_ghc_opts = "",
	 extra_cc_opts  = "",
	 extra_ld_opts  = "-luser32 -lgdi32"
	}
       ),

       ( "com",
	 Package {
	 import_dirs    = if installing 
	       		     then [ _libdir ++ "/imports/com" ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hdirect/lib" ],
	 library_dirs   = if installing 
	       		     then [ _libdir ]
	       		     else [ _FPTOOLS_TOP_ABS ++ "/hdirect/lib" ],
	 libraries      = [ "HScom" ],
	 include_dir    = "",
	 c_include      = "",		-- ???
	 package_deps   = [ "lang" ],
	 extra_ghc_opts = "",
	 extra_cc_opts  = "",
	 extra_ld_opts  = "-luser32 -lole32 -loleaut32 -ladvapi32"
	}
       )
   ]

ghc_src_dir path = _FPTOOLS_TOP_ABS ++ '/':_CURRENT_DIR ++ '/':path

prefixMatch :: Eq a => [a] -> [a] -> Bool
prefixMatch [] str = True
prefixMatch pat [] = False
prefixMatch (p:ps) (s:ss) | p == s    = prefixMatch ps ss
			  | otherwise = False

postfixMatch :: String -> String -> Bool
postfixMatch pat str = prefixMatch (reverse pat) (reverse str)
