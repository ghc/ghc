{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- This module rexposes wrapped parsers from the GHC API. Along with
-- returning the parse result, the corresponding annotations are also
-- returned such that it is then easy to modify the annotations and print
-- the result.
--
----------------------------------------------------------------------------
module Parsers (
        -- * Utility
          Parser
        , ParseResult
        , withDynFlags
        , CppOptions(..)
        , defaultCppOptions
        , LibDir

        -- * Module Parsers
        , parseModule
        , parseModuleFromString
        , parseModuleWithOptions
        , parseModuleWithCpp

        -- * Basic Parsers
        , parseExpr
        , parseImport
        , parseType
        , parseDecl
        , parsePattern
        , parseStmt

        , parseWith

        -- * Internal

        , ghcWrapper

        , initDynFlags
        , initDynFlagsPure
        , parseModuleFromStringInternal
        , parseModuleEpAnnsWithCpp
        , parseModuleEpAnnsWithCppInternal
        , postParseTransform
        ) where

import Preprocess

import Control.Monad.RWS

import qualified GHC hiding (parseModule)
import qualified GHC.Run as GHC
import qualified Control.Monad.IO.Class as GHC
import qualified GHC.Data.FastString    as GHC
import qualified GHC.Data.StringBuffer  as GHC
import qualified GHC.Driver.Config.Parser as GHC
import qualified GHC.Driver.Errors.Types as GHC
import qualified GHC.Driver.Session     as GHC
import qualified GHC.Parser             as GHC
import qualified GHC.Parser.Header      as GHC
import qualified GHC.Parser.Lexer       as GHC
import qualified GHC.Parser.PostProcess as GHC
import qualified GHC.Types.SrcLoc       as GHC

import qualified GHC.LanguageExtensions as LangExt

-- ---------------------------------------------------------------------

-- | Wrapper function which returns Annotations along with the parsed
-- element.
parseWith :: GHC.DynFlags
          -> FilePath
          -> GHC.P w
          -> String
          -> ParseResult w
parseWith dflags fileName parser s =
  case runParser parser dflags fileName s of
    GHC.PFailed pst
      -> Left (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)
    GHC.POk _ pmod
      -> Right pmod


parseWithECP :: (GHC.DisambECP w)
          => GHC.DynFlags
          -> FilePath
          -> GHC.P GHC.ECP
          -> String
          -> ParseResult (GHC.LocatedA w)
parseWithECP dflags fileName parser s =
    case runParser (parser >>= \p -> GHC.runPV $ GHC.unECP p) dflags fileName s of
      GHC.PFailed pst
        -> Left (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)
      GHC.POk _ pmod
        -> Right pmod

-- ---------------------------------------------------------------------

runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.initParserState (GHC.initParserOpts flags) buffer location

-- ---------------------------------------------------------------------

-- | Provides a safe way to consume a properly initialised set of
-- 'DynFlags'.
--
-- @
-- myParser fname expr = withDynFlags (\\d -> parseExpr d fname expr)
-- @
withDynFlags :: FilePath -> (GHC.DynFlags -> a) -> IO a
withDynFlags libdir action = ghcWrapper libdir $ do
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags dflags
  return (action dflags)

-- ---------------------------------------------------------------------

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult (GHC.Located GHC.HsModule)
parseFile = runParser GHC.parseModule

-- ---------------------------------------------------------------------

type LibDir = FilePath

type ParseResult a = Either GHC.ErrorMessages a

type Parser a = GHC.DynFlags -> FilePath -> String
                -> ParseResult a

parseExpr :: Parser (GHC.LHsExpr GHC.GhcPs)
parseExpr df fp = parseWithECP df fp GHC.parseExpression

parseImport :: Parser (GHC.LImportDecl GHC.GhcPs)
parseImport df fp = parseWith df fp GHC.parseImport

parseType :: Parser (GHC.LHsType GHC.GhcPs)
parseType df fp = parseWith df fp GHC.parseType

-- safe, see D1007
parseDecl :: Parser (GHC.LHsDecl GHC.GhcPs)
parseDecl df fp = parseWith df fp GHC.parseDeclaration

parseStmt :: Parser (GHC.ExprLStmt GHC.GhcPs)
parseStmt df fp = parseWith df fp GHC.parseStatement

parsePattern :: Parser (GHC.LPat GHC.GhcPs)
parsePattern df fp = parseWith df fp GHC.parsePattern

-- ---------------------------------------------------------------------
--

-- | This entry point will also work out which language extensions are
-- required and perform CPP processing if necessary.
--
-- @
-- parseModule = parseModuleWithCpp defaultCppOptions
-- @
--
-- Note: 'GHC.ParsedSource' is a synonym for 'GHC.Located' ('GHC.HsModule' 'GhcPs')
parseModule :: LibDir -> FilePath -> IO (ParseResult GHC.ParsedSource)
parseModule libdir file = parseModuleWithCpp libdir defaultCppOptions file


-- | This entry point will work out which language extensions are
-- required but will _not_ perform CPP processing.
-- In contrast to `parseModoule` the input source is read from the provided
-- string; the `FilePath` parameter solely exists to provide a name
-- in source location annotations.
parseModuleFromString
  :: FilePath -- GHC libdir
  -> FilePath
  -> String
  -> IO (ParseResult GHC.ParsedSource)
parseModuleFromString libdir fp s = ghcWrapper libdir $ do
  dflags <- initDynFlagsPure fp s
  return $ parseModuleFromStringInternal dflags fp s

-- | Internal part of 'parseModuleFromString'.
parseModuleFromStringInternal :: Parser GHC.ParsedSource
parseModuleFromStringInternal dflags fileName str =
  let (str1, lp) = stripLinePragmas str
      res        = case runParser GHC.parseModule dflags fileName str1 of
        GHC.PFailed pst
          -> Left (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)
        GHC.POk     _  pmod
          -> Right (lp, dflags, pmod)
  in  postParseTransform res

parseModuleWithOptions :: FilePath -- ^ GHC libdir
                       -> FilePath
                       -> IO (ParseResult GHC.ParsedSource)
parseModuleWithOptions libdir fp =
  parseModuleWithCpp libdir defaultCppOptions fp


-- | Parse a module with specific instructions for the C pre-processor.
parseModuleWithCpp
  :: FilePath -- ^ GHC libdir
  -> CppOptions
  -> FilePath -- ^ File to be parsed
  -> IO (ParseResult GHC.ParsedSource)
parseModuleWithCpp libdir cpp fp = do
  res <- parseModuleEpAnnsWithCpp libdir cpp fp
  return $ postParseTransform res

-- ---------------------------------------------------------------------

-- | Low level function which is used in the internal tests.
-- It is advised to use 'parseModule' or 'parseModuleWithCpp' instead of
-- this function.
parseModuleEpAnnsWithCpp
  :: FilePath -- ^ GHC libdir
  -> CppOptions
  -> FilePath -- ^ File to be parsed
  -> IO
       ( Either
           GHC.ErrorMessages
           ([GHC.LEpaComment], GHC.DynFlags, GHC.ParsedSource)
       )
parseModuleEpAnnsWithCpp libdir cppOptions file = ghcWrapper libdir $ do
  dflags <- initDynFlags file
  parseModuleEpAnnsWithCppInternal cppOptions dflags file

-- | Internal function. Default runner of GHC.Ghc action in IO.
ghcWrapper :: FilePath -> GHC.Ghc a -> IO a
ghcWrapper libdir a =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
    $ GHC.runGhcWithAbiHashes (Just libdir) a

-- | Internal function. Exposed if you want to muck with DynFlags
-- before parsing.
parseModuleEpAnnsWithCppInternal
  :: GHC.GhcMonad m
  => CppOptions
  -> GHC.DynFlags
  -> FilePath
  -> m
       ( Either
           GHC.ErrorMessages
           ([GHC.LEpaComment], GHC.DynFlags, GHC.ParsedSource)
       )
parseModuleEpAnnsWithCppInternal cppOptions dflags file = do
  let useCpp = GHC.xopt LangExt.Cpp dflags
  (fileContents, injectedComments, dflags') <-
    if useCpp
      then do
        (contents,dflags1) <- getPreprocessedSrcDirect cppOptions file
        cppComments <- getCppTokensAsComments cppOptions file
        return (contents,cppComments,dflags1)
      else do
        txt <- GHC.liftIO $ readFileGhc file
        let (contents1,lp) = stripLinePragmas txt
        return (contents1,lp,dflags)
  return $
    case parseFile dflags' file fileContents of
      GHC.PFailed pst
        -> Left (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)
      GHC.POk _ pmod
        -> Right $ (injectedComments, dflags', fixModuleTrailingComments pmod)

-- | Internal function. Exposed if you want to muck with DynFlags
-- before parsing. Or after parsing.
postParseTransform
  :: Either a ([GHC.LEpaComment], GHC.DynFlags, GHC.ParsedSource)
  -> Either a (GHC.ParsedSource)
postParseTransform parseRes = fmap mkAnns parseRes
  where
    -- TODO:AZ perhaps inject the comments into the parsedsource here already
    mkAnns (_cs, _, m) = fixModuleTrailingComments m

fixModuleTrailingComments :: GHC.ParsedSource -> GHC.ParsedSource
fixModuleTrailingComments (GHC.L l p) = GHC.L l p'
  where
    an' = case GHC.hsmodAnn p of
      (GHC.EpAnn a an ocs) -> GHC.EpAnn a an (rebalance (GHC.am_decls an) ocs)
      unused -> unused
    p' = p { GHC.hsmodAnn = an' }
    -- p'  = error $ "fixModuleTrailingComments: an'=" ++ showAst an'

    rebalance :: GHC.AnnList -> GHC.EpAnnComments -> GHC.EpAnnComments
    rebalance al cs = cs'
      where
        cs' = case GHC.al_close al of
          Just (GHC.AddEpAnn _ (GHC.EpaSpan ss)) ->
            let
              pc = GHC.priorComments cs
              fc = GHC.getFollowingComments cs
              bf (GHC.L anc _) = GHC.anchor anc > ss
              (prior,f) = break bf fc
              cs'' = GHC.EpaCommentsBalanced (pc <> prior) f
            in cs''
          _ -> cs

-- | Internal function. Initializes DynFlags value for parsing.
--
-- Passes "-hide-all-packages" to the GHC API to prevent parsing of
-- package environment files. However this only works if there is no
-- invocation of `setSessionDynFlags` before calling `initDynFlags`.
-- See ghc tickets #15513, #15541.
initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
  dflags0         <- GHC.getSessionDynFlags
  let parser_opts0 = GHC.initParserOpts dflags0
  src_opts        <- GHC.liftIO $ GHC.getOptionsFromFile parser_opts0 file
  (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 src_opts
  -- Turn this on last to avoid T10942
  let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  -- Prevent parsing of .ghc.environment.* "package environment files"
  (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine
    dflags2
    [GHC.noLoc "-hide-all-packages"]
  _ <- GHC.setSessionDynFlags dflags3
  return dflags3

-- | Requires GhcMonad constraint because there is
-- no pure variant of `parseDynamicFilePragma`. Yet, in constrast to
-- `initDynFlags`, it does not (try to) read the file at filepath, but
-- solely depends on the module source in the input string.
--
-- Passes "-hide-all-packages" to the GHC API to prevent parsing of
-- package environment files. However this only works if there is no
-- invocation of `setSessionDynFlags` before calling `initDynFlagsPure`.
-- See ghc tickets #15513, #15541.
initDynFlagsPure :: GHC.GhcMonad m => FilePath -> String -> m GHC.DynFlags
initDynFlagsPure fp s = do
  -- I was told we could get away with using the unsafeGlobalDynFlags.
  -- as long as `parseDynamicFilePragma` is impure there seems to be
  -- no reason to use it.
  dflags0 <- GHC.getSessionDynFlags
  let parser_opts0 = GHC.initParserOpts dflags0
  let pragmaInfo = GHC.getOptions parser_opts0 (GHC.stringToStringBuffer $ s) fp
  (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 pragmaInfo
  -- Turn this on last to avoid T10942
  let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  -- Prevent parsing of .ghc.environment.* "package environment files"
  (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine
    dflags2
    [GHC.noLoc "-hide-all-packages"]
  _ <- GHC.setSessionDynFlags dflags3
  return dflags3

-- ---------------------------------------------------------------------
