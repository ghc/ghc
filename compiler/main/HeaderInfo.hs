{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- | Parsing the top of a Haskell source file to get its module name,
-- imports and options.
--
-- (c) Simon Marlow 2005
-- (c) Lemmih 2006
--
-----------------------------------------------------------------------------

module HeaderInfo ( getImports
                  , mkPrelImports -- used by the renamer too
                  , getOptionsFromFile, getOptions
                  , optionsErrorMsgs,
                    checkProcessArgsResult ) where

#include "HsVersions.h"

import GhcPrelude

import HscTypes
import Parser           ( parseHeader )
import Lexer
import FastString
import HsSyn
import Module
import PrelNames
import StringBuffer
import SrcLoc
import DynFlags
import ErrUtils
import Util
import Outputable
import Pretty           ()
import Maybes
import Bag              ( emptyBag, listToBag, unitBag )
import MonadUtils
import Exception
import BasicTypes
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import System.IO
import System.IO.Unsafe
import Data.List

------------------------------------------------------------------------------

-- | Parse the imports of a source file.
--
-- Throws a 'SourceError' if parsing fails.
getImports :: DynFlags
           -> StringBuffer -- ^ Parse this.
           -> FilePath     -- ^ Filename the buffer came from.  Used for
                           --   reporting parse error locations.
           -> FilePath     -- ^ The original source filename (used for locations
                           --   in the function result)
           -> IO (Either
               ErrorMessages
               ([(Maybe FastString, Located ModuleName)],
                [(Maybe FastString, Located ModuleName)],
                Located ModuleName))
              -- ^ The source imports, normal imports, and the module name.
getImports dflags buf filename source_filename = do
  let loc  = mkRealSrcLoc (mkFastString filename) 1 1
  case unP parseHeader (mkPState dflags buf loc) of
    PFailed _ span err -> do
        -- assuming we're not logging warnings here as per below
        return $ Left $ unitBag $ mkPlainErrMsg dflags span err
    POk pst rdr_module -> fmap Right $ do
      let _ms@(_warns, errs) = getMessages pst dflags
      -- don't log warnings: they'll be reported when we parse the file
      -- for real.  See #2500.
          ms = (emptyBag, errs)
      -- logWarnings warns
      if errorsFound dflags ms
        then throwIO $ mkSrcErr errs
        else
          let   hsmod = unLoc rdr_module
                mb_mod = hsmodName hsmod
                imps = hsmodImports hsmod
                main_loc = srcLocSpan (mkSrcLoc (mkFastString source_filename)
                                       1 1)
                mod = mb_mod `orElse` cL main_loc mAIN_NAME
                (src_idecls, ord_idecls) = partition (ideclSource.unLoc) imps

               -- GHC.Prim doesn't exist physically, so don't go looking for it.
                ordinary_imps = filter ((/= moduleName gHC_PRIM) . unLoc
                                        . ideclName . unLoc)
                                       ord_idecls

                implicit_prelude = xopt LangExt.ImplicitPrelude dflags
                implicit_imports = mkPrelImports (unLoc mod) main_loc
                                                 implicit_prelude imps
                convImport (dL->L _ i) = (fmap sl_fs (ideclPkgQual i)
                                         , ideclName i)
              in
              return (map convImport src_idecls,
                      map convImport (implicit_imports ++ ordinary_imps),
                      mod)

mkPrelImports :: ModuleName
              -> SrcSpan    -- Attribute the "import Prelude" to this location
              -> Bool -> [LImportDecl GhcPs]
              -> [LImportDecl GhcPs]
-- Construct the implicit declaration "import Prelude" (or not)
--
-- NB: opt_NoImplicitPrelude is slightly different to import Prelude ();
-- because the former doesn't even look at Prelude.hi for instance
-- declarations, whereas the latter does.
mkPrelImports this_mod loc implicit_prelude import_decls
  | this_mod == pRELUDE_NAME
   || explicit_prelude_import
   || not implicit_prelude
  = []
  | otherwise = [preludeImportDecl]
  where
      explicit_prelude_import
       = notNull [ () | (dL->L _ (ImportDecl { ideclName = mod
                                        , ideclPkgQual = Nothing }))
                          <- import_decls
                      , unLoc mod == pRELUDE_NAME ]

      preludeImportDecl :: LImportDecl GhcPs
      preludeImportDecl
        = cL loc $ ImportDecl { ideclExt       = noExt,
                                ideclSourceSrc = NoSourceText,
                                ideclName      = cL loc pRELUDE_NAME,
                                ideclPkgQual   = Nothing,
                                ideclSource    = False,
                                ideclSafe      = False,  -- Not a safe import
                                ideclQualified = False,
                                ideclImplicit  = True,   -- Implicit!
                                ideclAs        = Nothing,
                                ideclHiding    = Nothing  }

--------------------------------------------------------------
-- Get options
--------------------------------------------------------------

-- | Parse OPTIONS and LANGUAGE pragmas of the source file.
--
-- Throws a 'SourceError' if flag parsing fails (including unsupported flags.)
getOptionsFromFile :: DynFlags
                   -> FilePath            -- ^ Input file
                   -> IO [Located String] -- ^ Parsed options, if any.
getOptionsFromFile dflags filename
    = Exception.bracket
              (openBinaryFile filename ReadMode)
              (hClose)
              (\handle -> do
                  opts <- fmap (getOptions' dflags)
                               (lazyGetToks dflags' filename handle)
                  seqList opts $ return opts)
    where -- We don't need to get haddock doc tokens when we're just
          -- getting the options from pragmas, and lazily lexing them
          -- correctly is a little tricky: If there is "\n" or "\n-"
          -- left at the end of a buffer then the haddock doc may
          -- continue past the end of the buffer, despite the fact that
          -- we already have an apparently-complete token.
          -- We therefore just turn Opt_Haddock off when doing the lazy
          -- lex.
          dflags' = gopt_unset dflags Opt_Haddock

blockSize :: Int
-- blockSize = 17 -- for testing :-)
blockSize = 1024

lazyGetToks :: DynFlags -> FilePath -> Handle -> IO [Located Token]
lazyGetToks dflags filename handle = do
  buf <- hGetStringBufferBlock handle blockSize
  unsafeInterleaveIO $ lazyLexBuf handle (pragState dflags buf loc) False blockSize
 where
  loc  = mkRealSrcLoc (mkFastString filename) 1 1

  lazyLexBuf :: Handle -> PState -> Bool -> Int -> IO [Located Token]
  lazyLexBuf handle state eof size = do
    case unP (lexer False return) state of
      POk state' t -> do
        -- pprTrace "lazyLexBuf" (text (show (buffer state'))) (return ())
        if atEnd (buffer state') && not eof
           -- if this token reached the end of the buffer, and we haven't
           -- necessarily read up to the end of the file, then the token might
           -- be truncated, so read some more of the file and lex it again.
           then getMore handle state size
           else case unLoc t of
                  ITeof  -> return [t]
                  _other -> do rest <- lazyLexBuf handle state' eof size
                               return (t : rest)
      _ | not eof   -> getMore handle state size
        | otherwise -> return [cL (RealSrcSpan (last_loc state)) ITeof]
                         -- parser assumes an ITeof sentinel at the end

  getMore :: Handle -> PState -> Int -> IO [Located Token]
  getMore handle state size = do
     -- pprTrace "getMore" (text (show (buffer state))) (return ())
     let new_size = size * 2
       -- double the buffer size each time we read a new block.  This
       -- counteracts the quadratic slowdown we otherwise get for very
       -- large module names (#5981)
     nextbuf <- hGetStringBufferBlock handle new_size
     if (len nextbuf == 0) then lazyLexBuf handle state True new_size else do
       newbuf <- appendStringBuffers (buffer state) nextbuf
       unsafeInterleaveIO $ lazyLexBuf handle state{buffer=newbuf} False new_size


getToks :: DynFlags -> FilePath -> StringBuffer -> [Located Token]
getToks dflags filename buf = lexAll (pragState dflags buf loc)
 where
  loc  = mkRealSrcLoc (mkFastString filename) 1 1

  lexAll state = case unP (lexer False return) state of
                   POk _      t@(dL->L _ ITeof) -> [t]
                   POk state' t -> t : lexAll state'
                   _ -> [cL (RealSrcSpan (last_loc state)) ITeof]


-- | Parse OPTIONS and LANGUAGE pragmas of the source file.
--
-- Throws a 'SourceError' if flag parsing fails (including unsupported flags.)
getOptions :: DynFlags
           -> StringBuffer -- ^ Input Buffer
           -> FilePath     -- ^ Source filename.  Used for location info.
           -> [Located String] -- ^ Parsed options.
getOptions dflags buf filename
    = getOptions' dflags (getToks dflags filename buf)

-- The token parser is written manually because Happy can't
-- return a partial result when it encounters a lexer error.
-- We want to extract options before the buffer is passed through
-- CPP, so we can't use the same trick as 'getImports'.
getOptions' :: DynFlags
            -> [Located Token]      -- Input buffer
            -> [Located String]     -- Options.
getOptions' dflags toks
    = parseToks toks
    where
          parseToks (open:close:xs)
              | IToptions_prag str <- unLoc open
              , ITclose_prag       <- unLoc close
              = case toArgs str of
                  Left _err -> optionsParseError str dflags $   -- #15053
                                 combineSrcSpans (getLoc open) (getLoc close)
                  Right args -> map (cL (getLoc open)) args ++ parseToks xs
          parseToks (open:close:xs)
              | ITinclude_prag str <- unLoc open
              , ITclose_prag       <- unLoc close
              = map (cL (getLoc open)) ["-#include",removeSpaces str] ++
                parseToks xs
          parseToks (open:close:xs)
              | ITdocOptions str <- unLoc open
              , ITclose_prag     <- unLoc close
              = map (cL (getLoc open)) ["-haddock-opts", removeSpaces str]
                ++ parseToks xs
          parseToks (open:xs)
              | ITlanguage_prag <- unLoc open
              = parseLanguage xs
          parseToks (comment:xs) -- Skip over comments
              | isComment (unLoc comment)
              = parseToks xs
          parseToks _ = []
          parseLanguage ((dL->L loc (ITconid fs)):rest)
              = checkExtension dflags (cL loc fs) :
                case rest of
                  (dL->L _loc ITcomma):more -> parseLanguage more
                  (dL->L _loc ITclose_prag):more -> parseToks more
                  (dL->L loc _):_ -> languagePragParseError dflags loc
                  [] -> panic "getOptions'.parseLanguage(1) went past eof token"
          parseLanguage (tok:_)
              = languagePragParseError dflags (getLoc tok)
          parseLanguage []
              = panic "getOptions'.parseLanguage(2) went past eof token"

          isComment :: Token -> Bool
          isComment c =
            case c of
              (ITlineComment {})     -> True
              (ITblockComment {})    -> True
              (ITdocCommentNext {})  -> True
              (ITdocCommentPrev {})  -> True
              (ITdocCommentNamed {}) -> True
              (ITdocSection {})      -> True
              _                      -> False

-----------------------------------------------------------------------------

-- | Complain about non-dynamic flags in OPTIONS pragmas.
--
-- Throws a 'SourceError' if the input list is non-empty claiming that the
-- input flags are unknown.
checkProcessArgsResult :: MonadIO m => DynFlags -> [Located String] -> m ()
checkProcessArgsResult dflags flags
  = when (notNull flags) $
      liftIO $ throwIO $ mkSrcErr $ listToBag $ map mkMsg flags
    where mkMsg (dL->L loc flag)
              = mkPlainErrMsg dflags loc $
                  (text "unknown flag in  {-# OPTIONS_GHC #-} pragma:" <+>
                   text flag)

-----------------------------------------------------------------------------

checkExtension :: DynFlags -> Located FastString -> Located String
checkExtension dflags (dL->L l ext)
-- Checks if a given extension is valid, and if so returns
-- its corresponding flag. Otherwise it throws an exception.
 =  let ext' = unpackFS ext in
    if ext' `elem` supportedLanguagesAndExtensions
    then cL l ("-X"++ext')
    else unsupportedExtnError dflags l ext'

languagePragParseError :: DynFlags -> SrcSpan -> a
languagePragParseError dflags loc =
    throwErr dflags loc $
       vcat [ text "Cannot parse LANGUAGE pragma"
            , text "Expecting comma-separated list of language options,"
            , text "each starting with a capital letter"
            , nest 2 (text "E.g. {-# LANGUAGE TemplateHaskell, GADTs #-}") ]

unsupportedExtnError :: DynFlags -> SrcSpan -> String -> a
unsupportedExtnError dflags loc unsup =
    throwErr dflags loc $
        text "Unsupported extension: " <> text unsup $$
        if null suggestions then Outputable.empty else text "Perhaps you meant" <+> quotedListWithOr (map text suggestions)
  where
     suggestions = fuzzyMatch unsup supportedLanguagesAndExtensions


optionsErrorMsgs :: DynFlags -> [String] -> [Located String] -> FilePath -> Messages
optionsErrorMsgs dflags unhandled_flags flags_lines _filename
  = (emptyBag, listToBag (map mkMsg unhandled_flags_lines))
  where unhandled_flags_lines :: [Located String]
        unhandled_flags_lines = [ cL l f
                                | f <- unhandled_flags
                                , (dL->L l f') <- flags_lines
                                , f == f' ]
        mkMsg (dL->L flagSpan flag) =
            ErrUtils.mkPlainErrMsg dflags flagSpan $
                    text "unknown flag in  {-# OPTIONS_GHC #-} pragma:" <+> text flag

optionsParseError :: String -> DynFlags -> SrcSpan -> a     -- #15053
optionsParseError str dflags loc =
  throwErr dflags loc $
      vcat [ text "Error while parsing OPTIONS_GHC pragma."
           , text "Expecting whitespace-separated list of GHC options."
           , text "  E.g. {-# OPTIONS_GHC -Wall -O2 #-}"
           , text ("Input was: " ++ show str) ]

throwErr :: DynFlags -> SrcSpan -> SDoc -> a                -- #15053
throwErr dflags loc doc =
  throw $ mkSrcErr $ unitBag $ mkPlainErrMsg dflags loc doc
