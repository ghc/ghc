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
   , toArgs
   , checkProcessArgsResult
   )
where

import GHC.Prelude

import GHC.Data.Bag

import GHC.Driver.Errors.Types -- Unfortunate, needed due to the fact we throw exceptions!

import GHC.Parser.Errors.Types
import GHC.Parser           ( parseHeader )
import GHC.Parser.Lexer

import GHC.Hs
import GHC.Unit.Module
import GHC.Builtin.Names

import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Types.SourceError
import GHC.Types.SourceText
import GHC.Types.PkgQual

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Monad
import GHC.Utils.Error
import GHC.Utils.Exception as Exception

import GHC.Data.StringBuffer
import GHC.Data.Maybe
import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict

import Control.Monad
import System.IO
import System.IO.Unsafe
import Data.List (partition)
import Data.Char (isSpace)
import Text.ParserCombinators.ReadP (readP_to_S, gather)
import Text.ParserCombinators.ReadPrec (readPrec_to_P)
import Text.Read (readPrec)

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
               (Messages PsMessage)
               ([(RawPkgQual, Located ModuleName)],
                [(RawPkgQual, Located ModuleName)],
                Bool, -- Is GHC.Prim imported or not
                Located ModuleName))
              -- ^ The source imports and normal imports (with optional package
              -- names from -XPackageImports), and the module name.
getImports popts implicit_prelude buf filename source_filename = do
  let loc  = mkRealSrcLoc (mkFastString filename) 1 1
  case unP parseHeader (initParserState popts buf loc) of
    PFailed pst ->
        -- assuming we're not logging warnings here as per below
      return $ Left $ getPsErrorMessages pst
    POk pst rdr_module -> fmap Right $ do
      let (_warns, errs) = getPsMessages pst
      -- don't log warnings: they'll be reported when we parse the file
      -- for real.  See #2500.
      if not (isEmptyMessages errs)
        then throwErrors (GhcPsMessage <$> errs)
        else
          let   hsmod = unLoc rdr_module
                mb_mod = hsmodName hsmod
                imps = hsmodImports hsmod
                main_loc = srcLocSpan (mkSrcLoc (mkFastString source_filename)
                                       1 1)
                mod = mb_mod `orElse` L (noAnnSrcSpan main_loc) mAIN_NAME
                (src_idecls, ord_idecls) = partition ((== IsBoot) . ideclSource . unLoc) imps

               -- GHC.Prim doesn't exist physically, so don't go looking for it.
                (ordinary_imps, ghc_prim_import)
                  = partition ((/= moduleName gHC_PRIM) . unLoc
                                  . ideclName . unLoc)
                                 ord_idecls

                implicit_imports = mkPrelImports (unLoc mod) main_loc
                                                 implicit_prelude imps
                convImport (L _ i) = (ideclPkgQual i, reLoc $ ideclName i)
              in
              return (map convImport src_idecls
                     , map convImport (implicit_imports ++ ordinary_imps)
                     , not (null ghc_prim_import)
                     , reLoc mod)

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
      explicit_prelude_import = any is_prelude_import import_decls

      is_prelude_import (L _ decl) =
        unLoc (ideclName decl) == pRELUDE_NAME
        -- allow explicit "base" package qualifier (#19082, #17045)
        && case ideclPkgQual decl of
            NoRawPkgQual -> True
            RawPkgQual b -> sl_fs b == unitIdFS baseUnitId


      loc' = noAnnSrcSpan loc
      preludeImportDecl :: LImportDecl GhcPs
      preludeImportDecl
        = L loc' $ ImportDecl { ideclExt       = noAnn,
                                ideclSourceSrc = NoSourceText,
                                ideclName      = L loc' pRELUDE_NAME,
                                ideclPkgQual   = NoRawPkgQual,
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
getOptionsFromFile :: ParserOpts
                   -> FilePath            -- ^ Input file
                   -> IO (Messages PsMessage, [Located String]) -- ^ Parsed options, if any.
getOptionsFromFile opts filename
    = Exception.bracket
              (openBinaryFile filename ReadMode)
              (hClose)
              (\handle -> do
                  (warns, opts) <- fmap (getOptions' opts)
                               (lazyGetToks opts' filename handle)
                  seqList opts
                    $ seqList (bagToList $ getMessages warns)
                    $ return (warns, opts))
    where -- We don't need to get haddock doc tokens when we're just
          -- getting the options from pragmas, and lazily lexing them
          -- correctly is a little tricky: If there is "\n" or "\n-"
          -- left at the end of a buffer then the haddock doc may
          -- continue past the end of the buffer, despite the fact that
          -- we already have an apparently-complete token.
          -- We therefore just turn Opt_Haddock off when doing the lazy
          -- lex.
          opts' = disableHaddock opts

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
getOptions :: ParserOpts
           -> StringBuffer -- ^ Input Buffer
           -> FilePath     -- ^ Source filename.  Used for location info.
           -> (Messages PsMessage,[Located String]) -- ^ warnings and parsed options.
getOptions opts buf filename
    = getOptions' opts (getToks opts filename buf)

-- The token parser is written manually because Happy can't
-- return a partial result when it encounters a lexer error.
-- We want to extract options before the buffer is passed through
-- CPP, so we can't use the same trick as 'getImports'.
getOptions' :: ParserOpts
            -> [Located Token]      -- Input buffer
            -> (Messages PsMessage,[Located String])     -- Options.
getOptions' opts toks
    = parseToks toks
    where
          parseToks (open:close:xs)
              | IToptions_prag str <- unLoc open
              , ITclose_prag       <- unLoc close
              = case toArgs starting_loc str of
                  Left _err -> optionsParseError str $   -- #15053
                                 combineSrcSpans (getLoc open) (getLoc close)
                  Right args -> fmap (args ++) (parseToks xs)
            where
              src_span      = getLoc open
              real_src_span = expectJust "getOptions'" (srcSpanToRealSrcSpan src_span)
              starting_loc  = realSrcSpanStart real_src_span
          parseToks (open:close:xs)
              | ITinclude_prag str <- unLoc open
              , ITclose_prag       <- unLoc close
              = fmap (map (L (getLoc open)) ["-#include",removeSpaces str] ++)
                     (parseToks xs)
          parseToks (open:close:xs)
              | ITdocOptions str _ <- unLoc open
              , ITclose_prag       <- unLoc close
              = fmap (map (L (getLoc open)) ["-haddock-opts", removeSpaces str] ++)
                     (parseToks xs)
          parseToks (open:xs)
              | ITlanguage_prag <- unLoc open
              = parseLanguage xs
          parseToks (comment:xs) -- Skip over comments
              | isComment (unLoc comment)
              = parseToks xs
          -- At the end of the header, warn about all the misplaced pragmas
          parseToks xs = (unionManyMessages $ mapMaybe mkMessage xs ,[])

          parseLanguage ((L loc (ITconid fs)):rest)
              = fmap (checkExtension opts (L loc fs) :) $
                case rest of
                  (L _loc ITcomma):more -> parseLanguage more
                  (L _loc ITclose_prag):more -> parseToks more
                  (L loc _):_ -> languagePragParseError loc
                  [] -> panic "getOptions'.parseLanguage(1) went past eof token"
          parseLanguage (tok:_)
              = languagePragParseError (getLoc tok)
          parseLanguage []
              = panic "getOptions'.parseLanguage(2) went past eof token"

          -- Warn for all the misplaced pragmas
          mkMessage :: Located Token -> Maybe (Messages PsMessage)
          mkMessage (L loc token)
            | IToptions_prag _ <- token
            = Just (singleMessage $ mkPlainMsgEnvelope diag_opts loc (PsWarnMisplacedPragma OptionsPrag))
            | ITinclude_prag _ <- token
            = Just (singleMessage $ mkPlainMsgEnvelope diag_opts loc (PsWarnMisplacedPragma IncludePrag))
            | ITdocOptions _ _ <- token
            = Just (singleMessage $ mkPlainMsgEnvelope diag_opts loc (PsWarnMisplacedPragma DocOptionsPrag))
            | ITlanguage_prag <- token
            = Just (singleMessage $ mkPlainMsgEnvelope diag_opts loc (PsWarnMisplacedPragma LanguagePrag))
            | otherwise = Nothing
            where diag_opts = pDiagOpts opts

          isComment :: Token -> Bool
          isComment c =
            case c of
              (ITlineComment {})  -> True
              (ITblockComment {}) -> True
              (ITdocComment {})   -> True
              _                   -> False

toArgs :: RealSrcLoc
       -> String -> Either String   -- Error
                           [Located String] -- Args
toArgs starting_loc orig_str
    = let (after_spaces_loc, after_spaces_str) = consume_spaces starting_loc orig_str in
      case after_spaces_str of
      '[':after_bracket ->
        let after_bracket_loc = advanceSrcLoc after_spaces_loc '['
            (after_bracket_spaces_loc, after_bracket_spaces_str)
              = consume_spaces after_bracket_loc after_bracket in
        case after_bracket_spaces_str of
          ']':rest | all isSpace rest -> Right []
          _ -> readAsList after_bracket_spaces_loc after_bracket_spaces_str

      _ -> toArgs' after_spaces_loc after_spaces_str
 where
  consume_spaces :: RealSrcLoc -> String -> (RealSrcLoc, String)
  consume_spaces loc [] = (loc, [])
  consume_spaces loc (c:cs)
    | isSpace c = consume_spaces (advanceSrcLoc loc c) cs
    | otherwise = (loc, c:cs)

  break_with_loc :: (Char -> Bool) -> RealSrcLoc -> String
                 -> (String, RealSrcLoc, String)  -- location is start of second string
  break_with_loc p = go []
    where
      go reversed_acc loc [] = (reverse reversed_acc, loc, [])
      go reversed_acc loc (c:cs)
        | p c       = (reverse reversed_acc, loc, c:cs)
        | otherwise = go (c:reversed_acc) (advanceSrcLoc loc c) cs

  advance_src_loc_many :: RealSrcLoc -> String -> RealSrcLoc
  advance_src_loc_many = foldl' advanceSrcLoc

  locate :: RealSrcLoc -> RealSrcLoc -> a -> Located a
  locate begin end x = L (RealSrcSpan (mkRealSrcSpan begin end) Strict.Nothing) x

  toArgs' :: RealSrcLoc -> String -> Either String [Located String]
  -- Remove outer quotes:
  -- > toArgs' "\"foo\" \"bar baz\""
  -- Right ["foo", "bar baz"]
  --
  -- Keep inner quotes:
  -- > toArgs' "-DFOO=\"bar baz\""
  -- Right ["-DFOO=\"bar baz\""]
  toArgs' loc s =
    let (after_spaces_loc, after_spaces_str) = consume_spaces loc s in
    case after_spaces_str of
      [] -> Right []
      '"' : _ -> do
        -- readAsString removes outer quotes
        (arg, new_loc, rest) <- readAsString after_spaces_loc after_spaces_str
        check_for_space rest
        (locate after_spaces_loc new_loc arg:)
          `fmap` toArgs' new_loc rest
      _ -> case break_with_loc (isSpace <||> (== '"')) after_spaces_loc after_spaces_str of
            (argPart1, loc2, s''@('"':_)) -> do
                (argPart2, loc3, rest) <- readAsString loc2 s''
                check_for_space rest
                -- show argPart2 to keep inner quotes
                (locate after_spaces_loc loc3 (argPart1 ++ show argPart2):)
                  `fmap` toArgs' loc3 rest
            (arg, loc2, s'') -> (locate after_spaces_loc loc2 arg:)
                                  `fmap` toArgs' loc2 s''

  check_for_space :: String -> Either String ()
  check_for_space [] = Right ()
  check_for_space (c:_)
    | isSpace c = Right ()
    | otherwise = Left ("Whitespace expected after string in " ++ show orig_str)

  reads_with_consumed :: Read a => String
                      -> [((String, a), String)]
                        -- ((consumed string, parsed result), remainder of input)
  reads_with_consumed = readP_to_S (gather (readPrec_to_P readPrec 0))

  readAsString :: RealSrcLoc
               -> String
               -> Either String (String, RealSrcLoc, String)
  readAsString loc s = case reads_with_consumed s of
                [((consumed, arg), rest)] ->
                    Right (arg, advance_src_loc_many loc consumed, rest)
                _ ->
                    Left ("Couldn't read " ++ show s ++ " as String")

   -- input has had the '[' stripped off
  readAsList :: RealSrcLoc -> String -> Either String [Located String]
  readAsList loc s = do
    let (after_spaces_loc, after_spaces_str) = consume_spaces loc s
    (arg, after_arg_loc, after_arg_str) <- readAsString after_spaces_loc after_spaces_str
    let (after_arg_spaces_loc, after_arg_spaces_str)
          = consume_spaces after_arg_loc after_arg_str
    (locate after_spaces_loc after_arg_loc arg :) <$>
      case after_arg_spaces_str of
        ',':after_comma -> readAsList (advanceSrcLoc after_arg_spaces_loc ',') after_comma
        ']':after_bracket
          | all isSpace after_bracket
          -> Right []
        _ -> Left ("Couldn't read " ++ show ('[' : s) ++ " as [String]")
             -- reinsert missing '[' for clarity.

-----------------------------------------------------------------------------

-- | Complain about non-dynamic flags in OPTIONS pragmas.
--
-- Throws a 'SourceError' if the input list is non-empty claiming that the
-- input flags are unknown.
checkProcessArgsResult :: MonadIO m => [Located String] -> m ()
checkProcessArgsResult flags
  = when (notNull flags) $
      liftIO $ throwErrors $ foldMap (singleMessage . mkMsg) flags
    where mkMsg (L loc flag)
              = mkPlainErrorMsgEnvelope loc $
                GhcPsMessage $ PsHeaderMessage $ PsErrUnknownOptionsPragma flag

-----------------------------------------------------------------------------

checkExtension :: ParserOpts -> Located FastString -> Located String
checkExtension opts (L l ext)
-- Checks if a given extension is valid, and if so returns
-- its corresponding flag. Otherwise it throws an exception.
  = if ext' `elem` (pSupportedExts opts)
    then L l ("-X"++ext')
    else unsupportedExtnError opts l ext'
  where
    ext' = unpackFS ext

languagePragParseError :: SrcSpan -> a
languagePragParseError loc =
    throwErr loc $ PsErrParseLanguagePragma

unsupportedExtnError :: ParserOpts -> SrcSpan -> String -> a
unsupportedExtnError opts loc unsup =
    throwErr loc $ PsErrUnsupportedExt unsup (pSupportedExts opts)

optionsParseError :: String -> SrcSpan -> a     -- #15053
optionsParseError str loc =
  throwErr loc $ PsErrParseOptionsPragma str

throwErr :: SrcSpan -> PsHeaderMessage -> a                -- #15053
throwErr loc ps_msg =
  let msg = mkPlainErrorMsgEnvelope loc $ GhcPsMessage (PsHeaderMessage ps_msg)
  in throw $ mkSrcErr $ singleMessage msg
