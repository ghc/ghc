-----------------------------------------------------------------------------
--
-- Parsing the top of a Haskell source file to get its module name
-- and imports.
--
-- (c) Simon Marlow 2005
--
-----------------------------------------------------------------------------

module GetImports ( getImportsFromFile, getImports ) where

#include "HsVersions.h"

import Parser		( parseHeader )
import Lexer		( P(..), ParseResult(..), mkPState )
import HsSyn		( ImportDecl(..), HsModule(..) )
import Module		( Module, mkModule )
import PrelNames        ( gHC_PRIM )
import StringBuffer	( StringBuffer, hGetStringBuffer )
import SrcLoc		( Located(..), mkSrcLoc, unLoc )
import FastString	( mkFastString )
import DynFlags	( DynFlags )
import ErrUtils
import Pretty
import Panic
import Bag		( unitBag )

import EXCEPTION	( throwDyn )
import IO
import List

-- getImportsFromFile is careful to close the file afterwards, otherwise
-- we can end up with a large number of open handles before the garbage
-- collector gets around to closing them.
getImportsFromFile :: DynFlags -> FilePath -> IO ([Module], [Module], Module)
getImportsFromFile dflags filename = do
  buf <- hGetStringBuffer filename
  getImports dflags buf filename

getImports :: DynFlags -> StringBuffer -> FilePath -> IO ([Module], [Module], Module)
getImports dflags buf filename = do
  let loc  = mkSrcLoc (mkFastString filename) 1 0
  case unP parseHeader (mkPState buf loc dflags) of
	PFailed span err -> parseError span err
	POk _ rdr_module -> 
	  case rdr_module of
	    L _ (HsModule mod _ imps _ _) ->
	      let
		mod_name | Just (L _ m) <- mod = m
			 | otherwise           = mkModule "Main"
	        (src_idecls, ord_idecls) = partition isSourceIdecl (map unLoc imps)
		source_imps   = map getImpMod src_idecls	
		ordinary_imps = filter (/= gHC_PRIM) (map getImpMod ord_idecls)
		     -- GHC.Prim doesn't exist physically, so don't go looking for it.
	      in
	      return (source_imps, ordinary_imps, mod_name)
  
parseError span err = throwDyn (ProgramError err_doc)
  where err_doc = render (pprBagOfErrors (unitBag (mkPlainErrMsg span err)))

isSourceIdecl (ImportDecl _ s _ _ _) = s

getImpMod (ImportDecl (L _ mod) _ _ _ _) = mod
