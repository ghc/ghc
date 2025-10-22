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
   , lazyGetToks, getOptions'
   , getOptions
   , toArgs
   , checkProcessArgsResult
   -- TODO: This is here to break an import loop. Is this the right place?
   , initParserStateWithMacros
   , initPragStateWithMacros
   , initParserStateWithMacrosString
   )
where

import GHC.Prelude

import GHC.Data.Bag

import GHC.Driver.DynFlags (DynFlags)
import GHC.Driver.Errors.Types -- Unfortunate, needed due to the fact we throw exceptions!

import GHC.Parser.Errors.Types
import GHC.Parser           ( parseHeader )
import GHC.Parser.Lexer hiding (initPragState, initParserState, lexer)
import GHC.Parser.Lexer qualified as Lexer
import GHC.Parser.PreProcess   (initPragState, lexer)
import GHC.Parser.PreProcess.State (PpState (..), initPpState, PpScope (..), PpGroupState (PpNoGroup))

import GHC.Hs
import GHC.Builtin.Names

import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Types.SourceError
import GHC.Types.SourceText
import GHC.Types.PkgQual
import GHC.Types.Basic (ImportLevel(..), convImportLevel)

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Monad
import GHC.Utils.Error
import GHC.Utils.Exception as Exception

import qualified Data.List.NonEmpty as NE
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
import GHC.Driver.Session (DynFlags)
import GHC.Driver.Config.Parser (predefinedMacros)
import GHC.Unit.Env (UnitEnv)
import GHC.SysTools.Cpp (cppMacroDefines)
import qualified GHC.LanguageExtensions.Type as LangExt
import GHC.Driver.DynFlags (xopt)

------------------------------------------------------------------------------

-- | Parse the imports of a source file.
--
-- Throws a 'SourceError' if parsing fails.
getImports :: DynFlags
           -> Maybe UnitEnv
           -> ParserOpts   -- ^ Parser options
           -> SourceErrorContext
           -> Bool         -- ^ Implicit Prelude?
           -> StringBuffer -- ^ Parse this.
           -> FilePath     -- ^ Filename the buffer came from.  Used for
                           --   reporting parse error locations.
           -> FilePath     -- ^ The original source filename (used for locations
                           --   in the function result)
           -> IO (Either
               (Messages PsMessage)
               ([Located ModuleName],
                [(ImportLevel, RawPkgQual, Located ModuleName)],
                Located ModuleName))
              -- ^ The source imports and normal imports (with optional package
              -- names from -XPackageImports), and the module name.
getImports dflags unit_env popts sec implicit_prelude buf filename source_filename = do
  let loc  = mkRealSrcLoc (mkFastString filename) 1 1
  case unP parseHeader (initParserStateWithMacros dflags unit_env popts buf loc) of
    PFailed pst ->
        -- assuming we're not logging warnings here as per below
      return $ Left $ getPsErrorMessages pst
    POk pst rdr_module -> fmap Right $ do
      let (_warns, errs) = getPsMessages pst
      -- don't log warnings: they'll be reported when we parse the file
      -- for real.  See #2500.
      if not (isEmptyMessages errs)
        then throwErrors sec (GhcPsMessage <$> errs)
        else
          let   hsmod = unLoc rdr_module
                mb_mod = hsmodName hsmod
                imps = hsmodImports hsmod
                main_loc = srcLocSpan (mkSrcLoc (mkFastString source_filename)
                                       1 1)
                mod = mb_mod `orElse` L (noAnnSrcSpan main_loc) mAIN_NAME
                (src_idecls, ord_idecls) = partition ((== IsBoot) . ideclSource . unLoc) imps

                generated_imports = mkPrelImports (unLoc mod) implicit_prelude imps
                convImport (L _ (i :: ImportDecl GhcPs)) = (convImportLevel (ideclLevelSpec i), ideclPkgQual i, reLoc $ ideclName i)
                convImport_src (L _ (i :: ImportDecl GhcPs)) = (reLoc $ ideclName i)
              in
              return (map convImport_src src_idecls
                     , map convImport (generated_imports ++ ord_idecls)
                     , reLoc mod)

initParserStateWithMacros
  :: DynFlags -> Maybe UnitEnv -> ParserOpts -> StringBuffer -> RealSrcLoc -> PState PpState
initParserStateWithMacros df unit_env opts buf pos
  = initParserStateWithMacrosString df (fmap cppMacroDefines unit_env) opts buf pos

initParserStateWithMacrosString
  :: DynFlags -> Maybe String -> ParserOpts -> StringBuffer -> RealSrcLoc -> PState PpState
initParserStateWithMacrosString df Nothing opts buf pos
  = Lexer.initParserState (initPpState { pp_defines = predefinedMacros df
                                       , pp_scope = (PpScope True PpNoGroup) NE.:| [] })
                          opts buf pos
initParserStateWithMacrosString df (Just macro_defs) opts buf pos = p_state
  where
    p_state0 = Lexer.initParserState (initPpState { pp_defines = predefinedMacros df
                                                  , pp_scope = (PpScope True PpNoGroup) NE.:| [] })
                                     opts (stringToStringBuffer macro_defs) pos
    p_state =
      if xopt LangExt.GhcCpp df
        then case unP parseHeader p_state0 of
               PFailed _ -> p_state0 { buffer = buf }
               POk st _ -> p_state0 { buffer = buf
                                    , pp = (pp p_state0) { pp_defines = pp_defines (pp st)}}
        else p_state0 { buffer = buf }

initPragStateWithMacros
  :: DynFlags -> UnitEnv -> ParserOpts -> StringBuffer -> RealSrcLoc -> PState PpState
initPragStateWithMacros df unit_env opts buf pos = prag_state
  where
    p_state = initParserStateWithMacros df (Just unit_env) opts buf pos
    prag_state0 = initPragState opts buf pos
    prag_state = prag_state0 { pp = (pp p_state) { pp_defines = pp_defines (pp p_state) } }

mkPrelImports :: ModuleName
              -> Bool -> [LImportDecl GhcPs]
              -> [LImportDecl GhcPs]
-- Construct the implicit declaration "import Prelude" (or not)
--
-- NB: opt_NoImplicitPrelude is slightly different to import Prelude ();
-- because the former doesn't even look at Prelude.hi for instance
-- declarations, whereas the latter does.
mkPrelImports this_mod implicit_prelude import_decls
  | this_mod == pRELUDE_NAME
   || explicit_prelude_import
   || not implicit_prelude
  = []
  | otherwise = [preludeImportDecl]
  where
      explicit_prelude_import = any is_prelude_import import_decls

      is_prelude_import (L _ (decl::ImportDecl GhcPs)) =
        unLoc (ideclName decl) == pRELUDE_NAME
        -- See #17045, package qualified imports are never counted as
        -- explicit prelude imports
        && case ideclPkgQual decl of
            NoRawPkgQual -> True
            RawPkgQual {} -> False
        -- Only a "normal" level import will override the implicit prelude import.
        && case ideclLevelSpec decl of
              NotLevelled -> True
              _ -> False


      loc = noAnnSrcSpan generatedSrcSpan
      preludeImportDecl :: LImportDecl GhcPs
      preludeImportDecl
        = L loc $ ImportDecl { ideclExt       = XImportDeclPass
                                                    { ideclAnn = noAnn
                                                    , ideclSourceText = NoSourceText
                                                    , ideclGenerated  = True   -- Generated!
                                                    },
                                ideclName      = L loc pRELUDE_NAME,
                                ideclPkgQual   = NoRawPkgQual,
                                ideclSource    = NotBoot,
                                ideclSafe      = False,  -- Not a safe import
                                ideclQualified = NotQualified,
                                ideclAs        = Nothing,
                                ideclLevelSpec = NotLevelled,
                                ideclImportList = Nothing  }

--------------------------------------------------------------
-- Get options
--------------------------------------------------------------

-- | Parse OPTIONS and LANGUAGE pragmas of the source file.
--
-- Throws a 'SourceError' if flag parsing fails (including unsupported flags.)
getOptionsFromFile :: DynFlags
                   -> UnitEnv
                   -> ParserOpts
                   -> SourceErrorContext
                   -> [String] -- ^ Supported LANGUAGE pragmas
                   -> FilePath -- ^ Input file
                   -> IO (Messages PsMessage, [Located String]) -- ^ Parsed options, if any.
getOptionsFromFile df unit_env popts sec supported filename
    = Exception.bracket
              (openBinaryFile filename ReadMode)
              (hClose)
              (\handle -> do
                  (warns, opts) <- fmap (getOptions' popts sec supported)
                               (getPragState df unit_env popts' filename handle
                               >>= \prag_state -> lazyGetToks prag_state handle)
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
          popts' = disableHaddock popts

blockSize :: Int
-- blockSize = 17 -- for testing :-)
blockSize = 1024

getPragState :: DynFlags -> UnitEnv -> ParserOpts -> FilePath -> Handle -> IO (PState PpState)
getPragState df unit_env popts filename handle = do
  buf <- hGetStringBufferBlock handle blockSize
  let loc  = mkRealSrcLoc (mkFastString filename) 1 1
  let prag_state = if Lexer.ghcCppEnabled popts
        then initPragStateWithMacros df unit_env popts buf loc
        else initPragState popts buf loc
  return prag_state

lazyGetToks :: PState PpState -> Handle -> IO [Located Token]
lazyGetToks prag_state handle = do
  unsafeInterleaveIO $ lazyLexBuf handle prag_state False blockSize
 where
  lazyLexBuf :: Handle -> PState PpState -> Bool -> Int -> IO [Located Token]
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

  getMore :: Handle -> PState PpState -> Int -> IO [Located Token]
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
           -> SourceErrorContext
           -> [String] -- ^ Supported LANGUAGE pragmas
           -> StringBuffer -- ^ Input Buffer
           -> FilePath     -- ^ Source filename.  Used for location info.
           -> (Messages PsMessage,[Located String]) -- ^ warnings and parsed options.
getOptions opts sec supported buf filename
    = getOptions' opts sec supported (getToks opts filename buf)

-- The token parser is written manually because Happy can't
-- return a partial result when it encounters a lexer error.
-- We want to extract options before the buffer is passed through
-- CPP, so we can't use the same trick as 'getImports'.
getOptions' :: ParserOpts
            -> SourceErrorContext
            -> [String]
            -> [Located Token]      -- Input buffer
            -> (Messages PsMessage,[Located String])     -- Options.
getOptions' opts sec supported toks
    = parseToks False toks
    where
          ghcpp = ghcCppEnabled opts

          parseToks False (open:close:xs)
              | IToptions_prag str <- unLoc open
              , ITclose_prag       <- unLoc close
              = case toArgs starting_loc str of
                  Left _err -> optionsParseError sec str $   -- #15053
                                 combineSrcSpans (getLoc open) (getLoc close)
                  Right args -> fmap (args ++) (parseToks False xs)
            where
              src_span      = getLoc open
              real_src_span = expectJust (srcSpanToRealSrcSpan src_span)
              starting_loc  = realSrcSpanStart real_src_span
          parseToks False (open:close:xs)
              | ITinclude_prag str <- unLoc open
              , ITclose_prag       <- unLoc close
              = fmap (map (L (getLoc open)) ["-#include",removeSpaces str] ++)
                     (parseToks False xs)
          parseToks False (open:close:xs)
              | ITdocOptions str _ <- unLoc open
              , ITclose_prag       <- unLoc close
              = fmap (map (L (getLoc open)) ["-haddock-opts", removeSpaces str] ++)
                     (parseToks False xs)
          parseToks acpp (open:xs)
              | ITlanguage_prag <- unLoc open
              = parseLanguage acpp xs
          parseToks _acpp (tok:xs) -- Skip over comments
              | isCpp (unLoc tok)
              = parseToks True xs
          parseToks acpp (comment:xs) -- Skip over comments
              | isComment (unLoc comment)
              = parseToks acpp xs
          -- At the end of the header, warn about all the misplaced pragmas
          parseToks _acpp xs = (unionManyMessages $ mapMaybe mkMessage xs ,[])

          parseLanguage :: Bool -> [Located Token] -> (Messages PsMessage,[Located String])
          parseLanguage acpp@False ((L loc (ITconid fs)):rest)
              = fmap (checkExtension sec supported (L loc fs) :) $
                parseLanguageRest acpp rest

          parseLanguage True ((L loc (ITconid fs)):rest)
              | "GHC_CPP" == unpackFS fs = fmap ( L loc ("-XGHC_CPP"):) $
                parseLanguageRest True rest
              | otherwise =
                parseLanguageRest True rest

          -- parseLanguage acpp ((L loc (ITconid fs)):rest)
          --     = fmap (checkExtension sec supported (L loc fs) :) $
          --       case rest of
          --         (L _loc ITcomma):more -> parseLanguage acpp more
          --         (L _loc ITclose_prag):more -> parseToks acpp more
          --         (L loc _):_ -> languagePragParseError sec loc
          --         [] -> panic "getOptions'.parseLanguage(1) went past eof token"
          parseLanguage _acpp (tok:_)
              = languagePragParseError sec (getLoc tok)
          parseLanguage _acpp []
              = panic "getOptions'.parseLanguage(2) went past eof token"

          parseLanguageRest acpp rest
              = case rest of
                  (L _loc ITcomma):more -> parseLanguage acpp more
                  (L _loc ITclose_prag):more -> parseToks acpp more
                  (L loc _):_ -> languagePragParseError sec loc
                  [] -> panic "getOptions'.parseLanguage(1) went past eof token"

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

          isCpp :: Token -> Bool
          isCpp c =
            case c of
              (ITcpp {}) -> ghcpp == False
              _          -> False

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
checkProcessArgsResult :: MonadIO m => DynFlags -> [Located String] -> m ()
checkProcessArgsResult dflags flags
  = when (notNull flags) $
      liftIO $ throwErrors (initSourceErrorContext dflags) $ foldMap (singleMessage . mkMsg) flags
    where mkMsg (L loc flag)
              = mkPlainErrorMsgEnvelope loc $
                GhcPsMessage $ PsHeaderMessage $ PsErrUnknownOptionsPragma flag

-----------------------------------------------------------------------------

checkExtension :: SourceErrorContext -> [String] -> Located FastString -> Located String
checkExtension sec supported (L l ext)
-- Checks if a given extension is valid, and if so returns
-- its corresponding flag. Otherwise it throws an exception.
  = if ext' `elem` supported
    then L l ("-X"++ext')
    else unsupportedExtnError sec supported l ext'
  where
    ext' = unpackFS ext

languagePragParseError :: SourceErrorContext -> SrcSpan -> a
languagePragParseError sec loc =
    throwErr sec loc $ PsErrParseLanguagePragma

unsupportedExtnError :: SourceErrorContext -> [String] -> SrcSpan -> String -> a
unsupportedExtnError sec supported loc unsup =
    throwErr sec loc $ PsErrUnsupportedExt unsup supported

optionsParseError :: SourceErrorContext -> String -> SrcSpan -> a     -- #15053
optionsParseError sec str loc =
  throwErr sec loc $ PsErrParseOptionsPragma str

throwErr :: SourceErrorContext -> SrcSpan -> PsHeaderMessage -> a                -- #15053
throwErr sec loc ps_msg =
  let msg = mkPlainErrorMsgEnvelope loc $ GhcPsMessage (PsHeaderMessage ps_msg)
  in throw $ mkSrcErr sec $ singleMessage msg
