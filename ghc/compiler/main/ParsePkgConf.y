{
module ParsePkgConf( loadPackageConfig ) where

import Packages  ( PackageConfig(..), defaultPackageConfig )
import Lex
import FastString
import StringBuffer
import SrcLoc
import Outputable
import Panic     ( GhcException(..) )
import Exception ( throwDyn )

#include "HsVersions.h"

}

%token
 '{'		{ ITocurly }
 '}'		{ ITccurly }
 '['		{ ITobrack }
 ']'		{ ITcbrack }
 ','		{ ITcomma }
 '='		{ ITequal }
 VARID   	{ ITvarid    $$ }
 CONID   	{ ITconid    $$ }
 STRING		{ ITstring   $$ }

%monad { P } { thenP } { returnP }
%lexer { lexer } { ITeof }
%name parse
%tokentype { Token }
%%

pkgconf :: { [ PackageConfig ] }
	: '[' pkgs ']'			{ reverse $2 }

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
		{\p -> case unpackFS $1 of
		        "name" -> p{name = unpackFS $3} }
			
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
			_other            -> p
		}

strlist :: { [String] }
        : '[' ']'			{ [] }
	| '[' strs ']'			{ reverse $2 }

strs	:: { [String] }
	: STRING			{ [ unpackFS $1 ] }
	| strs ',' STRING		{ unpackFS $3 : $1 }

{
happyError :: P a
happyError buf PState{ loc = loc } = PFailed (srcParseErr buf loc)

loadPackageConfig :: FilePath -> IO [PackageConfig]
loadPackageConfig conf_filename = do
   buf <- hGetStringBuffer False conf_filename
   case parse buf PState{ bol = 0#, atbol = 1#,
	 	          context = [], glasgow_exts = 0#,
  			  loc = mkSrcLoc (_PK_ conf_filename) 1 } of
	PFailed err -> do
	    freeStringBuffer buf
            throwDyn (InstallationError (showSDoc err))

	POk _ pkg_details -> do
	    freeStringBuffer buf
	    return pkg_details
}
