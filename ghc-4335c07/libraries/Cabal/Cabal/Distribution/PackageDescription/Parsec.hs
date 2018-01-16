{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.Parsec
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defined parsers and partial pretty printers for the @.cabal@ format.

module Distribution.PackageDescription.Parsec (
    -- * Package descriptions
    readGenericPackageDescription,
    parseGenericPackageDescription,
    parseGenericPackageDescriptionMaybe,

    -- ** Parsing
    ParseResult,
    runParseResult,

    -- ** Supplementary build information
    readHookedBuildInfo,
    parseHookedBuildInfo,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import           Control.Monad.State.Strict                   (StateT, execStateT)
import           Control.Monad.Trans.Class                    (lift)
import qualified Data.ByteString                              as BS
import           Data.List                                    (partition)
import qualified Distribution.Compat.Map.Strict               as Map
import           Distribution.FieldGrammar
import           Distribution.PackageDescription
import           Distribution.PackageDescription.FieldGrammar
import           Distribution.PackageDescription.Quirks       (patchQuirks)
import           Distribution.Parsec.Class                    (parsec)
import           Distribution.Parsec.Common
import           Distribution.Parsec.ConfVar                  (parseConditionConfVar)
import           Distribution.Parsec.Field                    (FieldName, getName)
import           Distribution.Parsec.LexerMonad               (LexWarning, toPWarning)
import           Distribution.Parsec.Parser
import           Distribution.Parsec.ParseResult
import           Distribution.Simple.Utils                    (die', fromUTF8BS, warn)
import           Distribution.Text                            (display)
import           Distribution.Types.CondTree
import           Distribution.Types.ForeignLib
import           Distribution.Types.UnqualComponentName
                 (UnqualComponentName, mkUnqualComponentName)
import           Distribution.Utils.Generic                   (breakMaybe, unfoldrM)
import           Distribution.Verbosity                       (Verbosity)
import           Distribution.Version
                 (LowerBound (..), Version, asVersionIntervals, mkVersion, orLaterVersion)
import           System.Directory                             (doesFileExist)

import           Distribution.Compat.Lens
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.PackageDescription.Lens        as L

-- ---------------------------------------------------------------
-- Parsing

-- | Helper combinator to do parsing plumbing for files.
--
-- Given a parser and a filename, return the parse of the file,
-- after checking if the file exists.
--
-- Argument order is chosen to encourage partial application.
readAndParseFile
    :: (BS.ByteString -> ParseResult a)  -- ^ File contents to final value parser
    -> Verbosity                         -- ^ Verbosity level
    -> FilePath                          -- ^ File to read
    -> IO a
readAndParseFile parser verbosity fpath = do
    exists <- doesFileExist fpath
    unless exists $
      die' verbosity $
        "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue."
    bs <- BS.readFile fpath
    let (warnings, errors, result) = runParseResult (parser bs)
    traverse_ (warn verbosity . showPWarning fpath) warnings
    traverse_ (warn verbosity . showPError fpath) errors
    case result of
        Nothing -> die' verbosity $ "Failing parsing \"" ++ fpath ++ "\"."
        Just x  -> return x

-- | Parse the given package file.
readGenericPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription = readAndParseFile parseGenericPackageDescription

------------------------------------------------------------------------------
-- | Parses the given file into a 'GenericPackageDescription'.
--
-- In Cabal 1.2 the syntax for package descriptions was changed to a format
-- with sections and possibly indented property descriptions.
--
parseGenericPackageDescription :: BS.ByteString -> ParseResult GenericPackageDescription
parseGenericPackageDescription bs = case readFields' bs' of
    Right (fs, lexWarnings) -> do
        when patched $
            parseWarning zeroPos PWTQuirkyCabalFile "Legacy cabal file"
        parseGenericPackageDescription' lexWarnings fs
    -- TODO: better marshalling of errors
    Left perr -> parseFatalFailure zeroPos (show perr)
  where
    (patched, bs') = patchQuirks bs

-- | 'Maybe' variant of 'parseGenericPackageDescription'
parseGenericPackageDescriptionMaybe :: BS.ByteString -> Maybe GenericPackageDescription
parseGenericPackageDescriptionMaybe =
    trdOf3 . runParseResult . parseGenericPackageDescription
  where
    trdOf3 (_, _, x) = x

fieldlinesToBS :: [FieldLine ann] -> BS.ByteString
fieldlinesToBS = BS.intercalate "\n" . map (\(FieldLine _ bs) -> bs)

-- Monad in which sections are parsed
type SectionParser = StateT GenericPackageDescription ParseResult

-- Note [Accumulating parser]
--
-- This parser has two "states":
-- * first we parse fields of PackageDescription
-- * then we parse sections (libraries, executables, etc)
parseGenericPackageDescription'
    :: [LexWarning]
    -> [Field Position]
    -> ParseResult GenericPackageDescription
parseGenericPackageDescription' lexWarnings fs = do
    parseWarnings (fmap toPWarning lexWarnings)
    let (syntax, fs') = sectionizeFields fs

    -- PackageDescription
    let (fields, sectionFields) = takeFields fs'
    pd <- parseFieldGrammar fields packageDescriptionFieldGrammar
    maybeWarnCabalVersion syntax pd

    -- Sections
    let gpd = emptyGpd & L.packageDescription .~ pd

    -- elif conditional is accepted if spec version is >= 2.1
    let hasElif = if specVersion pd >= mkVersion [2,1] then HasElif else NoElif
    execStateT (goSections hasElif sectionFields) gpd
  where
    emptyGpd :: GenericPackageDescription
    emptyGpd = GenericPackageDescription emptyPackageDescription [] Nothing [] [] [] [] []

    newSyntaxVersion :: Version
    newSyntaxVersion = mkVersion [1, 2]

    maybeWarnCabalVersion :: Syntax -> PackageDescription -> ParseResult ()
    maybeWarnCabalVersion syntax pkg
      | syntax == NewSyntax && specVersion pkg < newSyntaxVersion
      = parseWarning (Position 0 0) PWTNewSyntax $
             "A package using section syntax must specify at least\n"
          ++ "'cabal-version: >= 1.2'."

    maybeWarnCabalVersion syntax pkg
      | syntax == OldSyntax && specVersion pkg >= newSyntaxVersion
      = parseWarning (Position 0 0) PWTOldSyntax $
             "A package using 'cabal-version: "
          ++ displaySpecVersion (specVersionRaw pkg)
          ++ "' must use section syntax. See the Cabal user guide for details."
      where
        displaySpecVersion (Left version)       = display version
        displaySpecVersion (Right versionRange) =
          case asVersionIntervals versionRange of
            [] {- impossible -}           -> display versionRange
            ((LowerBound version _, _):_) -> display (orLaterVersion version)

    maybeWarnCabalVersion _ _ = return ()

    -- Sections
goSections :: HasElif -> [Field Position] -> SectionParser ()
goSections hasElif = traverse_ process
  where
    process (Field (Name pos name) _) =
        lift $ parseWarning pos PWTTrailingFields $
            "Ignoring trailing fields after sections: " ++ show name
    process (Section name args secFields) =
        parseSection name args secFields

    snoc x xs = xs ++ [x]

    parseSection :: Name Position -> [SectionArg Position] -> [Field Position] -> SectionParser ()
    parseSection (Name pos name) args fields
        | name == "library" && null args = do
            lib <- lift $ parseCondTree hasElif (libraryFieldGrammar Nothing) (targetBuildDepends . libBuildInfo) fields
            -- TODO: check that library is defined once
            L.condLibrary ?= lib

        -- Sublibraries
        | name == "library" = do
            -- TODO: check cabal-version
            name' <- parseUnqualComponentName pos args
            lib   <- lift $ parseCondTree hasElif (libraryFieldGrammar $ Just name') (targetBuildDepends . libBuildInfo) fields
            -- TODO check duplicate name here?
            L.condSubLibraries %= snoc (name', lib)

        | name == "foreign-library" = do
            name' <- parseUnqualComponentName pos args
            flib  <- lift $ parseCondTree hasElif (foreignLibFieldGrammar name') (targetBuildDepends . foreignLibBuildInfo) fields
            -- TODO check duplicate name here?
            L.condForeignLibs %= snoc (name', flib)

        | name == "executable" = do
            name' <- parseUnqualComponentName pos args
            exe   <- lift $ parseCondTree hasElif (executableFieldGrammar name') (targetBuildDepends . buildInfo) fields
            -- TODO check duplicate name here?
            L.condExecutables %= snoc (name', exe)

        | name == "test-suite" = do
            name'      <- parseUnqualComponentName pos args
            testStanza <- lift $ parseCondTree hasElif testSuiteFieldGrammar (targetBuildDepends . _testStanzaBuildInfo) fields
            testSuite  <- lift $ traverse (validateTestSuite pos) testStanza
            -- TODO check duplicate name here?
            L.condTestSuites %= snoc (name', testSuite)

        | name == "benchmark" = do
            name'       <- parseUnqualComponentName pos args
            benchStanza <- lift $ parseCondTree hasElif benchmarkFieldGrammar (targetBuildDepends . _benchmarkStanzaBuildInfo) fields
            bench       <- lift $ traverse (validateBenchmark pos) benchStanza
            -- TODO check duplicate name here?
            L.condBenchmarks %= snoc (name', bench)

        | name == "flag" = do
            name'  <- parseName pos args
            name'' <- lift $ runFieldParser' pos parsec name' `recoverWith` mkFlagName ""
            flag   <- lift $ parseFields fields (flagFieldGrammar name'')
            -- Check default flag
            L.genPackageFlags %= snoc flag

        | name == "custom-setup" && null args = do
            sbi <- lift $ parseFields fields  (setupBInfoFieldGrammar False)
            L.packageDescription . L.setupBuildInfo ?= sbi

        | name == "source-repository" = do
            kind <- lift $ case args of
                [SecArgName spos secName] ->
                    runFieldParser' spos parsec (fromUTF8BS secName) `recoverWith` RepoHead
                [] -> do
                    parseFailure pos "'source-repository' requires exactly one argument"
                    pure RepoHead
                _ -> do
                    parseFailure pos $ "Invalid source-repository kind " ++ show args
                    pure RepoHead

            sr <- lift $ parseFields fields (sourceRepoFieldGrammar kind)
            L.packageDescription . L.sourceRepos %= snoc sr

        | otherwise = lift $
            parseWarning pos PWTUnknownSection $ "Ignoring section: " ++ show name

parseName :: Position -> [SectionArg Position] -> SectionParser String
parseName pos args = case args of
    [SecArgName _pos secName] ->
         pure $ fromUTF8BS secName
    [SecArgStr _pos secName] ->
         pure $ fromUTF8BS secName
    [] -> do
         lift $ parseFailure pos $ "name required"
         pure ""
    _ -> do
         -- TODO: pretty print args
         lift $ parseFailure pos $ "Invalid name " ++ show args
         pure ""

parseUnqualComponentName :: Position -> [SectionArg Position] -> SectionParser UnqualComponentName
parseUnqualComponentName pos args = mkUnqualComponentName <$> parseName pos args

-- | Parse a non-recursive list of fields.
parseFields
    :: [Field Position] -- ^ fields to be parsed
    -> ParsecFieldGrammar' a
    -> ParseResult a
parseFields fields grammar = do
    let (fs0, ss) = partitionFields fields
    traverse_ (traverse_ warnInvalidSubsection) ss
    parseFieldGrammar fs0 grammar

warnInvalidSubsection :: Section Position -> ParseResult ()
warnInvalidSubsection (MkSection (Name pos name) _ _) =
    void (parseFailure pos $ "invalid subsection " ++ show name)


data HasElif = HasElif | NoElif
  deriving (Eq, Show)

parseCondTree
    :: forall a c.
       HasElif                -- ^ accept @elif@
    -> ParsecFieldGrammar' a  -- ^ grammar
    -> (a -> c)               -- ^ condition extractor
    -> [Field Position]
    -> ParseResult (CondTree ConfVar c a)
parseCondTree hasElif grammar cond = go
  where
    go fields = do
        let (fs, ss) = partitionFields fields
        x <- parseFieldGrammar fs grammar
        branches <- concat <$> traverse parseIfs ss
        return (CondNode x (cond x) branches) -- TODO: branches

    parseIfs :: [Section Position] -> ParseResult [CondBranch ConfVar c a]
    parseIfs [] = return []
    parseIfs (MkSection (Name _ name) test fields : sections) | name == "if" = do
        test' <- parseConditionConfVar test
        fields' <- go fields
        -- TODO: else
        (elseFields, sections') <- parseElseIfs sections
        return (CondBranch test' fields' elseFields : sections')
    parseIfs (MkSection (Name pos name) _ _ : sections) = do
        parseWarning pos PWTInvalidSubsection $ "invalid subsection " ++ show name
        parseIfs sections

    parseElseIfs
        :: [Section Position]
        -> ParseResult (Maybe (CondTree ConfVar c a), [CondBranch ConfVar c a])
    parseElseIfs [] = return (Nothing, [])
    parseElseIfs (MkSection (Name pos name) args fields : sections) | name == "else" = do
        unless (null args) $
            parseFailure pos $ "`else` section has section arguments " ++ show args
        elseFields <- go fields
        sections' <- parseIfs sections
        return (Just elseFields, sections')

    parseElseIfs (MkSection (Name _ name) test fields : sections) | hasElif == HasElif, name == "elif" = do
        -- TODO: check cabal-version
        test' <- parseConditionConfVar test
        fields' <- go fields
        (elseFields, sections') <- parseElseIfs sections
        -- we parse an empty 'Fields', to get empty value for a node
        a <- parseFieldGrammar mempty grammar
        return (Just $ CondNode a (cond a) [CondBranch test' fields' elseFields], sections')

    parseElseIfs sections = (,) Nothing <$> parseIfs sections

{- Note [Accumulating parser]

Note: Outdated a bit

In there parser, @'FieldDescr' a@ is transformed into @Map FieldName (a ->
FieldParser a)@.  The weird value is used because we accumulate structure of
@a@ by folding over the fields.  There are various reasons for that:

* Almost all fields are optional

* This is simple approach so declarative bi-directional format (parsing and
printing) of structure could be specified (list of @'FieldDescr' a@)

* There are surface syntax fields corresponding to single field in the file:
  @license-file@ and @license-files@

* This is quite safe approach.

When/if we re-implement the parser to support formatting preservging roundtrip
with new AST, this all need to be rewritten.
-}

-------------------------------------------------------------------------------
-- Old syntax
-------------------------------------------------------------------------------

-- TODO: move to own module

-- | "Sectionize" an old-style Cabal file.  A sectionized file has:
--
--  * all global fields at the beginning, followed by
--
--  * all flag declarations, followed by
--
--  * an optional library section, and an arbitrary number of executable
--    sections (in any order).
--
-- The current implementation just gathers all library-specific fields
-- in a library section and wraps all executable stanzas in an executable
-- section.
sectionizeFields :: [Field ann] -> (Syntax, [Field ann])
sectionizeFields fs = case classifyFields fs of
    Just fields -> (OldSyntax, convert fields)
    Nothing     -> (NewSyntax, fs)
  where
    -- return 'Just' if all fields are simple fields
    classifyFields :: [Field ann] -> Maybe [(Name ann, [FieldLine ann])]
    classifyFields = traverse f
      where
        f (Field name fieldlines) = Just (name, fieldlines)
        f _                      = Nothing

    trim = BS.dropWhile isSpace' . BS.reverse . BS.dropWhile isSpace' . BS.reverse
    isSpace' = (== 32)

    convert :: [(Name ann, [FieldLine ann])] -> [Field ann]
    convert fields =
      let
        toField (name, ls) = Field name ls
        -- "build-depends" is a local field now.  To be backwards
        -- compatible, we still allow it as a global field in old-style
        -- package description files and translate it to a local field by
        -- adding it to every non-empty section
        (hdr0, exes0) = break ((=="executable") . getName . fst) fields
        (hdr, libfs0) = partition (not . (`elem` libFieldNames) . getName . fst) hdr0

        (deps, libfs) = partition ((== "build-depends") . getName . fst)
                                   libfs0

        exes = unfoldr toExe exes0
        toExe [] = Nothing
        toExe ((Name pos n, ls) : r)
          | n == "executable" =
              let (efs, r') = break ((== "executable") . getName . fst) r
              in Just (Section (Name pos "executable") [SecArgName pos $ trim $ fieldlinesToBS ls] (map toField $ deps ++ efs), r')
        toExe _ = error "unexpected input to 'toExe'"

        lib = case libfs of
            []                         -> []
            ((Name pos _,  _) : _) ->
                [Section (Name pos "library") [] (map toField $ deps ++ libfs)]

      in map toField hdr ++ lib ++ exes

-- | See 'sectionizeFields'.
data Syntax = OldSyntax | NewSyntax
    deriving (Eq, Show)

libFieldNames :: [FieldName]
libFieldNames = fieldGrammarKnownFieldList (libraryFieldGrammar Nothing)

-------------------------------------------------------------------------------
-- Suplementary build information
-------------------------------------------------------------------------------

readHookedBuildInfo :: Verbosity -> FilePath -> IO HookedBuildInfo
readHookedBuildInfo = readAndParseFile parseHookedBuildInfo

parseHookedBuildInfo :: BS.ByteString -> ParseResult HookedBuildInfo
parseHookedBuildInfo bs = case readFields' bs' of
    Right (fs, lexWarnings) -> do
        when patched $
            parseWarning zeroPos PWTQuirkyCabalFile "Legacy cabal file"
        parseHookedBuildInfo' lexWarnings fs
    -- TODO: better marshalling of errors
    Left perr -> parseFatalFailure zeroPos (show perr)
  where
    (patched, bs') = patchQuirks bs

parseHookedBuildInfo'
    :: [LexWarning]
    -> [Field Position]
    -> ParseResult HookedBuildInfo
parseHookedBuildInfo' lexWarnings fs = do
    parseWarnings (fmap toPWarning lexWarnings)
    (mLibFields, exes) <- stanzas fs
    mLib <- parseLib mLibFields
    biExes <- traverse parseExe exes
    return (mLib, biExes)
  where
    parseLib :: Fields Position -> ParseResult (Maybe BuildInfo)
    parseLib fields
        | Map.null fields = pure Nothing
        | otherwise       = Just <$> parseFieldGrammar fields buildInfoFieldGrammar

    parseExe :: (UnqualComponentName, Fields Position) -> ParseResult (UnqualComponentName, BuildInfo)
    parseExe (n, fields) = do
        bi <- parseFieldGrammar fields buildInfoFieldGrammar
        pure (n, bi)

    stanzas :: [Field Position] -> ParseResult (Fields Position, [(UnqualComponentName, Fields Position)])
    stanzas fields = do
        let (hdr0, exes0) = breakMaybe isExecutableField fields
        hdr <- toFields hdr0
        exes <- unfoldrM (traverse toExe) exes0
        pure (hdr, exes)

    toFields :: [Field Position] -> ParseResult (Fields Position)
    toFields fields = do
        let (fields', ss) = partitionFields fields
        traverse_ (traverse_ warnInvalidSubsection) ss
        pure fields'

    toExe
        :: ([FieldLine Position], [Field Position])
        -> ParseResult ((UnqualComponentName, Fields Position), Maybe ([FieldLine Position], [Field Position]))
    toExe (fss, fields) = do
        name <- runFieldParser zeroPos parsec fss
        let (hdr0, rest) = breakMaybe isExecutableField fields
        hdr <- toFields hdr0
        pure ((name, hdr), rest)

    isExecutableField (Field (Name _ name) fss)
        | name == "executable" = Just fss
        | otherwise            = Nothing
    isExecutableField _ = Nothing
