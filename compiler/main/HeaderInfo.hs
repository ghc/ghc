-----------------------------------------------------------------------------
--
-- Parsing the top of a Haskell source file to get its module name,
-- imports and options.
--
-- (c) Simon Marlow 2005
-- (c) Lemmih 2006
--
-----------------------------------------------------------------------------

module HeaderInfo ( getImportsFromFile, getImports
                  , getOptionsFromFile, getOptions
                  , optionsErrorMsgs ) where

#include "HsVersions.h"

import Parser		( parseHeader )
import Lexer		( P(..), ParseResult(..), mkPState, pragState
                        , lexer, Token(..), PState(..) )
import FastString
import HsSyn		( ImportDecl(..), HsModule(..) )
import Module		( ModuleName, moduleName )
import PrelNames        ( gHC_PRIM, mAIN_NAME )
import StringBuffer	( StringBuffer(..), hGetStringBuffer, hGetStringBufferBlock
                        , appendStringBuffers )
import SrcLoc		( Located(..), mkSrcLoc, unLoc, noSrcSpan )
import FastString	( mkFastString )
import DynFlags	( DynFlags )
import ErrUtils
import Util
import Outputable
import Pretty           ()
import Panic
import Bag		( emptyBag, listToBag )

import Distribution.Compiler

import EXCEPTION	( throwDyn )
import IO
import List

#if __GLASGOW_HASKELL__ >= 601
import System.IO		( openBinaryFile )
#else
import IOExts                   ( openFileEx, IOModeEx(..) )
#endif

#if __GLASGOW_HASKELL__ < 601
openBinaryFile fp mode = openFileEx fp (BinaryMode mode)
#endif

-- getImportsFromFile is careful to close the file afterwards, otherwise
-- we can end up with a large number of open handles before the garbage
-- collector gets around to closing them.
getImportsFromFile :: DynFlags -> FilePath
   -> IO ([Located ModuleName], [Located ModuleName], Located ModuleName)
getImportsFromFile dflags filename = do
  buf <- hGetStringBuffer filename
  getImports dflags buf filename

getImports :: DynFlags -> StringBuffer -> FilePath
    -> IO ([Located ModuleName], [Located ModuleName], Located ModuleName)
getImports dflags buf filename = do
  let loc  = mkSrcLoc (mkFastString filename) 1 0
  case unP parseHeader (mkPState buf loc dflags) of
	PFailed span err -> parseError span err
	POk _ rdr_module -> 
	  case rdr_module of
	    L _ (HsModule mod _ imps _ _) ->
	      let
		mod_name | Just located_mod <- mod = located_mod
			 | otherwise               = L noSrcSpan mAIN_NAME
	        (src_idecls, ord_idecls) = partition isSourceIdecl (map unLoc imps)
		source_imps   = map getImpMod src_idecls	
		ordinary_imps = filter ((/= moduleName gHC_PRIM) . unLoc) 
					(map getImpMod ord_idecls)
		     -- GHC.Prim doesn't exist physically, so don't go looking for it.
	      in
	      return (source_imps, ordinary_imps, mod_name)
  
parseError span err = throwDyn $ mkPlainErrMsg span err

isSourceIdecl (ImportDecl _ s _ _ _) = s

getImpMod (ImportDecl located_mod _ _ _ _) = located_mod

--------------------------------------------------------------
-- Get options
--------------------------------------------------------------


getOptionsFromFile :: FilePath            -- input file
                   -> IO [Located String] -- options, if any
getOptionsFromFile filename
    = bracket (openBinaryFile filename ReadMode)
              (hClose)
              (\handle ->
                   do buf <- hGetStringBufferBlock handle blockSize
                      loop handle buf)
    where blockSize = 1024
          loop handle buf
              | len buf == 0 = return []
              | otherwise
              = case getOptions' buf filename of
                  (Nothing, opts) -> return opts
                  (Just buf', opts) -> do nextBlock <- hGetStringBufferBlock handle blockSize
                                          newBuf <- appendStringBuffers buf' nextBlock
                                          if len newBuf == len buf
                                             then return opts
                                             else do opts' <- loop handle newBuf
                                                     return (opts++opts')

getOptions :: StringBuffer -> FilePath -> [Located String]
getOptions buf filename
    = case getOptions' buf filename of
        (_,opts) -> opts

-- The token parser is written manually because Happy can't
-- return a partial result when it encounters a lexer error.
-- We want to extract options before the buffer is passed through
-- CPP, so we can't use the same trick as 'getImports'.
getOptions' :: StringBuffer         -- Input buffer
            -> FilePath             -- Source file. Used for msgs only.
            -> ( Maybe StringBuffer -- Just => we can use more input
               , [Located String]   -- Options.
               )
getOptions' buf filename
    = parseToks (lexAll (pragState buf loc))
    where loc  = mkSrcLoc (mkFastString filename) 1 0

          getToken (buf,L _loc tok) = tok
          getLoc (buf,L loc _tok) = loc
          getBuf (buf,_tok) = buf
          combine opts (flag, opts') = (flag, opts++opts')
          add opt (flag, opts) = (flag, opt:opts)

          parseToks (open:close:xs)
              | IToptions_prag str <- getToken open
              , ITclose_prag       <- getToken close
              = map (L (getLoc open)) (words str) `combine`
                parseToks xs
          parseToks (open:close:xs)
              | ITinclude_prag str <- getToken open
              , ITclose_prag       <- getToken close
              = map (L (getLoc open)) ["-#include",removeSpaces str] `combine`
                parseToks xs
          parseToks (open:xs)
              | ITlanguage_prag <- getToken open
              = parseLanguage xs
          -- The last token before EOF could have been truncated.
          -- We ignore it to be on the safe side.
          parseToks [tok,eof]
              | ITeof <- getToken eof
              = (Just (getBuf tok),[])
          parseToks (eof:_)
              | ITeof <- getToken eof
              = (Just (getBuf eof),[])
          parseToks _ = (Nothing,[])
          parseLanguage ((_buf,L loc (ITconid fs)):rest)
              = checkExtension (L loc fs) `add`
                case rest of
                  (_,L loc ITcomma):more -> parseLanguage more
                  (_,L loc ITclose_prag):more -> parseToks more
                  (_,L loc _):_ -> languagePragParseError loc
          parseLanguage (tok:_)
              = languagePragParseError (getLoc tok)
          lexToken t = return t
          lexAll state = case unP (lexer lexToken) state of
                           POk state' t@(L _ ITeof) -> [(buffer state,t)]
                           POk state' t -> (buffer state,t):lexAll state'
                           _ -> [(buffer state,L (last_loc state) ITeof)]

checkExtension :: Located FastString -> Located String
checkExtension (L l ext)
    = case reads (unpackFS ext) of
        [] -> languagePragParseError l
        (okExt,""):_ -> case extensionsToGHCFlag [okExt] of
                          ([],[opt]) -> L l opt
                          _ -> unsupportedExtnError l okExt

languagePragParseError loc =
  pgmError (showSDoc (mkLocMessage loc (
                text "cannot parse LANGUAGE pragma")))

unsupportedExtnError loc unsup =
  pgmError (showSDoc (mkLocMessage loc (
                text "unsupported extension: " <>
                (text.show) unsup)))


optionsErrorMsgs :: [String] -> [Located String] -> FilePath -> Messages
optionsErrorMsgs unhandled_flags flags_lines filename
  = (emptyBag, listToBag (map mkMsg unhandled_flags_lines))
  where	unhandled_flags_lines = [ L l f | f <- unhandled_flags, 
					  L l f' <- flags_lines, f == f' ]
        mkMsg (L flagSpan flag) = 
            ErrUtils.mkPlainErrMsg flagSpan $
                    text "unknown flag in  {-# OPTIONS #-} pragma:" <+> text flag

