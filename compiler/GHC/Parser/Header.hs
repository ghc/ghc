{-# LANGUAGE CPP          #-}
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

module GHC.Parser.Header
   ( getImports
   , mkPrelImports -- used by the renamer too
   , getOptionsFromFile
   , getOptions
   , optionsErrorMsgs
   , checkProcessArgsResult
   )
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Platform

import GHC.Driver.Session
import GHC.Driver.Config

import GHC.Parser.Errors.Ppr
import GHC.Parser.Errors
import GHC.Parser           ( parseHeader )
import GHC.Parser.Lexer

import GHC.Hs
import GHC.Unit.Module
import GHC.Builtin.Names

import GHC.Types.SrcLoc
import GHC.Types.SourceError
import GHC.Types.SourceText

import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Monad
import GHC.Utils.Exception as Exception

import GHC.Data.StringBuffer
import GHC.Data.Maybe
import GHC.Data.Bag         ( Bag, emptyBag, listToBag, unitBag, isEmptyBag )
import GHC.Data.FastString

import Control.Monad
import System.IO
import System.IO.Unsafe
import Data.List

------------------------------------------------------------------------------

-- | Parse the imports of a source file.
--
-- Throws a 'SourceError' if parsing fails.
getImports :: ParserOpts   -- ^ Parser options
           -> Bool         -- ^ Implicit Prelude?
           -> StringBuffer -- ^ Parse this.
           -> FilePath     -- ^ Filename the buffer came from.  Used for
                           --   reporting parse error locations.
           -> FilePath     -- ^ The original source filename (used for locations
                           --   in the function result)
           -> IO (Either
               (Bag Error)
               ([(Maybe FastString, Located ModuleName)],
                [(Maybe FastString, Located ModuleName)],
                Located ModuleName))
              -- ^ The source imports and normal imports (with optional package
              -- names from -XPackageImports), and the module name.
getImports popts implicit_prelude buf filename source_filename = do
  let loc  = mkRealSrcLoc (mkFastString filename) 1 1
  case unP parseHeader (initParserState popts buf loc) of
    PFailed pst ->
        -- assuming we're not logging warnings here as per below
      return $ Left $ getErrorMessages pst
    POk pst rdr_module -> fmap Right $ do
      let (_warns, errs) = getMessages pst
      -- don't log warnings: they'll be reported when we parse the file
      -- for real.  See #2500.
      if not (isEmptyBag errs)
        then throwIO $ mkSrcErr (fmap pprError errs)
        else
          let   hsmod = unLoc rdr_module
                mb_mod = hsmodName hsmod
                imps = hsmodImports hsmod
                main_loc = srcLocSpan (mkSrcLoc (mkFastString source_filename)
                                       1 1)
                mod = mb_mod `orElse` L main_loc mAIN_NAME
                (src_idecls, ord_idecls) = partition ((== IsBoot) . ideclSource . unLoc) imps

               -- GHC.Prim doesn't exist physically, so don't go looking for it.
                ordinary_imps = filter ((/= moduleName gHC_PRIM) . unLoc
                                        . ideclName . unLoc)
                                       ord_idecls

                implicit_imports = mkPrelImports (unLoc mod) main_loc
                                                 implicit_prelude imps
                convImport (L _ i) = (fmap sl_fs (ideclPkgQual i), ideclName i)
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
       = notNull [ () | L _ (ImportDecl { ideclName = mod
                                        , ideclPkgQual = Nothing })
                          <- import_decls
                      , unLoc mod == pRELUDE_NAME ]

      loc' = noAnnSrcSpan loc
      preludeImportDecl :: LImportDecl GhcPs
      preludeImportDecl
        = L loc' $ ImportDecl { ideclExt       = noAnn,
                                ideclSourceSrc = NoSourceText,
                                ideclName      = L loc pRELUDE_NAME,
                                ideclPkgQual   = Nothing,
                                ideclSource    = NotBoot,
                                ideclSafe      = False,  -- Not a safe import
                                ideclQualified = NotQualified,
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
                               (lazyGetToks (initParserOpts dflags') filename handle)
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

lazyGetToks :: ParserOpts -> FilePath -> Handle -> IO [Located Token]
lazyGetToks popts filename handle = do
  buf <- hGetStringBufferBlock handle blockSize
  let prag_state = initPragState popts buf loc
  unsafeInterleaveIO $ lazyLexBuf handle prag_state False blockSize
 where
  loc  = mkRealSrcLoc (mkFastString filename) 1 1

  lazyLexBuf :: Handle -> PState -> Bool -> Int -> IO [Located Token]
  lazyLexBuf handle state eof size =
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
        | otherwise -> return [L (mkSrcSpanPs (last_loc state)) ITeof]
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


getToks :: ParserOpts -> FilePath -> StringBuffer -> [Located Token]
getToks popts filename buf = lexAll pstate
 where
  pstate = initPragState popts buf loc
  loc  = mkRealSrcLoc (mkFastString filename) 1 1

  lexAll state = case unP (lexer False return) state of
                   POk _      t@(L _ ITeof) -> [t]
                   POk state' t -> t : lexAll state'
                   _ -> [L (mkSrcSpanPs (last_loc state)) ITeof]


-- | Parse OPTIONS and LANGUAGE pragmas of the source file.
--
-- Throws a 'SourceError' if flag parsing fails (including unsupported flags.)
getOptions :: DynFlags
           -> StringBuffer -- ^ Input Buffer
           -> FilePath     -- ^ Source filename.  Used for location info.
           -> [Located String] -- ^ Parsed options.
getOptions dflags buf filename
    = getOptions' dflags (getToks (initParserOpts dflags) filename buf)

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
                  Right args -> map (L (getLoc open)) args ++ parseToks xs
          parseToks (open:close:xs)
              | ITinclude_prag str <- unLoc open
              , ITclose_prag       <- unLoc close
              = map (L (getLoc open)) ["-#include",removeSpaces str] ++
                parseToks xs
          parseToks (open:close:xs)
              | ITdocOptions str <- unLoc open
              , ITclose_prag     <- unLoc close
              = map (L (getLoc open)) ["-haddock-opts", removeSpaces str]
                ++ parseToks xs
          parseToks (open:xs)
              | ITlanguage_prag <- unLoc open
              = parseLanguage xs
          parseToks (comment:xs) -- Skip over comments
              | isComment (unLoc comment)
              = parseToks xs
          parseToks _ = []
          parseLanguage ((L loc (ITconid fs)):rest)
              = checkExtension dflags (L loc fs) :
                case rest of
                  (L _loc ITcomma):more -> parseLanguage more
                  (L _loc ITclose_prag):more -> parseToks more
                  (L loc _):_ -> languagePragParseError dflags loc
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
    where mkMsg (L loc flag)
              = mkPlainErrMsg dflags loc $
                  (text "unknown flag in  {-# OPTIONS_GHC #-} pragma:" <+>
                   text flag)

-----------------------------------------------------------------------------

checkExtension :: DynFlags -> Located FastString -> Located String
checkExtension dflags (L l ext)
-- Checks if a given extension is valid, and if so returns
-- its corresponding flag. Otherwise it throws an exception.
  = if ext' `elem` supported
    then L l ("-X"++ext')
    else unsupportedExtnError dflags l ext'
  where
    ext' = unpackFS ext
    supported = supportedLanguagesAndExtensions $ platformArchOS $ targetPlatform dflags

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
     supported = supportedLanguagesAndExtensions $ platformArchOS $ targetPlatform dflags
     suggestions = fuzzyMatch unsup supported


optionsErrorMsgs :: DynFlags -> [String] -> [Located String] -> FilePath -> Messages
optionsErrorMsgs dflags unhandled_flags flags_lines _filename
  = (emptyBag, listToBag (map mkMsg unhandled_flags_lines))
  where unhandled_flags_lines :: [Located String]
        unhandled_flags_lines = [ L l f
                                | f <- unhandled_flags
                                , L l f' <- flags_lines
                                , f == f' ]
        mkMsg (L flagSpan flag) =
            GHC.Utils.Error.mkPlainErrMsg dflags flagSpan $
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
