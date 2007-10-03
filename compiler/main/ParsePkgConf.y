{
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module ParsePkgConf( loadPackageConfig ) where

#include "HsVersions.h"

import PackageConfig
import Lexer
import Module
import DynFlags
import FastString
import StringBuffer
import ErrUtils  ( mkLocMessage )
import SrcLoc
import Outputable
import Panic     ( GhcException(..) )
import Control.Exception ( throwDyn )

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
 INT		{ L _ (ITinteger  $$) }

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
	: VARID '=' pkgid
		{% case unpackFS $1 of
		        "package"     -> return (\p -> p{package = $3})
			_other        -> happyError
		}

	| VARID '=' STRING		{ id }
		-- we aren't interested in the string fields, they're all
		-- boring (copyright, maintainer etc.)
			
        | VARID '=' CONID
		{% case unpackFS $1 of {
		   	"exposed" -> 
			   case unpackFS $3 of {
				"True"  -> return (\p -> p{exposed=True});
				"False" -> return (\p -> p{exposed=False});
				_       -> happyError };
		   	"license" -> return id; -- not interested
		   	_         -> happyError }
		}

	| VARID '=' CONID STRING	{ id }
		-- another case of license

	| VARID '=' strlist		
	{\p -> case unpackFS $1 of
	        "exposedModules"    -> p{exposedModules    = map mkModuleNameFS $3}
	        "hiddenModules"     -> p{hiddenModules     = map mkModuleNameFS $3}
	        "importDirs"        -> p{importDirs        = map unpackFS $3}
	        "libraryDirs"       -> p{libraryDirs       = map unpackFS $3}
	        "hsLibraries"       -> p{hsLibraries       = map unpackFS $3}
	        "extraLibraries"    -> p{extraLibraries    = map unpackFS $3}
	        "extraGHCiLibraries"-> p{extraGHCiLibraries= map unpackFS $3}
	        "includeDirs"       -> p{includeDirs       = map unpackFS $3}
	        "includes"          -> p{includes          = map unpackFS $3}
	        "hugsOptions"       -> p{hugsOptions       = map unpackFS $3}
	        "ccOptions"         -> p{ccOptions         = map unpackFS $3}
	        "ldOptions"         -> p{ldOptions         = map unpackFS $3}
	        "frameworkDirs"     -> p{frameworkDirs     = map unpackFS $3}
	        "frameworks"        -> p{frameworks        = map unpackFS $3}
	        "haddockInterfaces" -> p{haddockInterfaces = map unpackFS $3}
	        "haddockHTMLs"      -> p{haddockHTMLs      = map unpackFS $3}
	        "depends"     	    -> p{depends = []}
			-- empty list only, non-empty handled below
		other -> p
	}

	| VARID '=' pkgidlist
		{% case unpackFS $1 of
		        "depends"     -> return (\p -> p{depends = $3})
			_other        -> happyError
		}

pkgid	:: { PackageIdentifier }
	: CONID '{' VARID '=' STRING ',' VARID '=' version '}'
			{ PackageIdentifier{ pkgName = unpackFS $5, 
					     pkgVersion = $9 } }

version :: { Version }
	: CONID '{' VARID '=' intlist ',' VARID '=' strlist '}'
			{ Version{ versionBranch=$5, 
                                   versionTags=map unpackFS $9 } }

pkgidlist :: { [PackageIdentifier] }
	: '[' pkgids ']'		{ $2 }
	-- empty list case is covered by strlist, to avoid conflicts

pkgids	:: { [PackageIdentifier] }
	: pkgid				{ [ $1 ] }
	| pkgid ',' pkgids		{ $1 : $3 }

intlist :: { [Int] }
        : '[' ']'			{ [] }
	| '[' ints ']'			{ $2 }

ints	:: { [Int] }
	: INT				{ [ fromIntegral $1 ] }
	| INT ',' ints			{ fromIntegral $1 : $3 }

strlist :: { [FastString] }
        : '[' ']'			{ [] }
	| '[' strs ']'			{ $2 }

strs	:: { [FastString] }
	: STRING			{ [ $1 ] }
	| STRING ',' strs 		{ $1 : $3 }

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
