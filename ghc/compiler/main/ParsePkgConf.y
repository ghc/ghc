{
module ParsePkgConf( loadPackageConfig ) where

#include "HsVersions.h"

import Packages  ( PackageConfig(..), defaultPackageConfig )
import Lexer
import CmdLineOpts
import FastString
import StringBuffer
import ErrUtils  ( mkLocMessage )
import SrcLoc
import Outputable
import Panic     ( GhcException(..) )
import EXCEPTION ( throwDyn )

}

%token
 '{'		{ L _ ITocurly }
 '}'		{ L _ ITccurly }
 '['		{ L _ ITobrack }
 ']'		{ L _ ITcbrack }
 ','		{ L _ ITcomma }
 '='		{ L _ ITequal }
 VARID   	{ L _ (ITvarid    $$) }
 CONID   	{ L _ (ITconid    $$) }
 STRING		{ L _ (ITstring   $$) }

%monad { P } { >>= } { return }
%lexer { lexer } { L _ ITeof }
%name parse
%tokentype { Located Token }
%%

pkgconf :: { [ PackageConfig ] }
	: '[' ']'			{ [] }
	| '[' pkgs ']'			{ reverse $2 }

pkgs 	:: { [ PackageConfig ] }
	: pkg 				{ [ $1 ] }
	| pkgs ',' pkg			{ $3 : $1 }

pkg 	:: { PackageConfig }
	: CONID '{' fields '}'		{ $3 defaultPackageConfig }

fields  :: { PackageConfig -> PackageConfig }
	: field				{ \p -> $1 p }
	| fields ',' field		{ \p -> $1 ($3 p) }

field	:: { PackageConfig -> PackageConfig }
	: VARID '=' STRING		
                 {% case unpackFS $1 of { 
		   "name" -> return (\ p -> p{name = unpackFS $3});
		   _      -> happyError } }
			
        | VARID '=' bool
		{\p -> case unpackFS $1 of {
		   	"auto" -> p{auto = $3};
		   	_      -> p } }

	| VARID '=' strlist		
		{\p -> case unpackFS $1 of
		        "import_dirs"     -> p{import_dirs     = $3}
		        "library_dirs"    -> p{library_dirs    = $3}
		        "hs_libraries"    -> p{hs_libraries    = $3}
		        "extra_libraries" -> p{extra_libraries = $3}
		        "include_dirs"    -> p{include_dirs    = $3}
		        "c_includes"      -> p{c_includes      = $3}
		        "package_deps"    -> p{package_deps    = $3}
		        "extra_ghc_opts"  -> p{extra_ghc_opts  = $3}
		        "extra_cc_opts"   -> p{extra_cc_opts   = $3}
		        "extra_ld_opts"   -> p{extra_ld_opts   = $3}
		        "framework_dirs"  -> p{framework_dirs  = $3}
		        "extra_frameworks"-> p{extra_frameworks= $3}
			_other            -> p
		}

strlist :: { [String] }
        : '[' ']'			{ [] }
	| '[' strs ']'			{ reverse $2 }

strs	:: { [String] }
	: STRING			{ [ unpackFS $1 ] }
	| strs ',' STRING		{ unpackFS $3 : $1 }

bool    :: { Bool }
	: CONID				{% case unpackFS $1 of {
					    "True"  -> return True;
					    "False" -> return False;
					    _       -> happyError } }

{
happyError :: P a
happyError = srcParseFail

loadPackageConfig :: FilePath -> IO [PackageConfig]
loadPackageConfig conf_filename = do
   buf <- hGetStringBuffer conf_filename
   let loc  = mkSrcLoc (mkFastString conf_filename) 1 0
   case unP parse (mkPState buf loc defaultDynFlags) of
	PFailed span err -> 
           throwDyn (InstallationError (showSDoc (mkLocMessage span err)))

	POk _ pkg_details -> do
	    return pkg_details
}
