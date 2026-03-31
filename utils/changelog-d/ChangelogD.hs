{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wall #-}
-- |
-- Adapted from changelog-d (https://codeberg.org/fgaz/changelog-d)
-- License: GPL-3.0-or-later
--
-- Collects changelog fragments from changelog.d/ and generates RST
-- release notes for GHC's Sphinx documentation.
module Main (main) where

import Control.Exception       (Exception (..))
import Control.Monad           (unless, void, when)
import Data.Char               (isSpace)
import Data.Foldable           (for_, toList, traverse_)
import Data.Function           (on)
import Data.List               (intercalate, sort, sortBy)
import Data.Maybe              (isJust, isNothing, mapMaybe)
import Data.Set                (Set)
import Data.Traversable        (for)
import System.Directory        (listDirectory)
import System.Environment      (getArgs)
import System.Exit             (exitFailure)
import System.FilePath         ((</>), dropTrailingPathSeparator, takeDirectory)
import System.IO               (hPutStrLn, stderr)

import qualified Data.ByteString                 as BS
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields             as C
import qualified Distribution.Fields.LexerMonad  as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Utils.Generic      as C
import qualified Text.PrettyPrint                as PP

-------------------------------------------------------------------------------
-- CLI
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err   -> do
            hPutStrLn stderr $ "changelog-d: " ++ err
            hPutStrLn stderr usage
            exitFailure
        Right opts -> makeChangelog opts

usage :: String
usage = unlines
    [ "Usage: changelog-d [OPTIONS] [<changelog.d>]"
    , ""
    , "  Collect changelog entries and produce release notes."
    , ""
    , "Options:"
    , "  --version <version>   Version number for RST file header (e.g. 10.2.1)"
    , "  --validate            Validate entries only, no output"
    , "  --expect-mr <N>       Check that at least one entry references MR !N"
    , "  --help                Show this help"
    ]

parseArgs :: [String] -> Either String Opts
parseArgs = go defaultOpts
  where
    defaultOpts = Opts "changelog.d" Nothing False Nothing

    go opts [] = Right opts
    go _    ("--help" : _) = Left ""
    go opts ("--validate" : rest) = go opts { optValidate = True } rest
    go opts ("--version" : v : rest) = go opts { optVersion = Just v } rest
    go _    ("--version" : []) = Left "--version requires an argument"
    go opts ("--expect-mr" : n : rest) = case reads n of
        [(mr, "")] -> go opts { optExpectMR = Just mr } rest
        _          -> Left $ "--expect-mr requires a number, got: " ++ n
    go _    ("--expect-mr" : []) = Left "--expect-mr requires an argument"
    go _    (('-':'-':opt) : _) = Left $ "Unknown option: --" ++ opt
    go _    (('-':opt) : _) = Left $ "Unknown option: -" ++ opt
    go opts (dir : rest) = go opts { optDirectory = dir } rest

-------------------------------------------------------------------------------
-- Parse cabal style config
-------------------------------------------------------------------------------

parseWith
    :: ([C.Field C.Position] -> C.ParseResult a)
    -> FilePath
    -> BS.ByteString
    -> Either String a
parseWith parser fp bs = case C.readFields' bs of
    Left perr -> Left $ fp ++ ": " ++ show perr
    Right (fields, lexWarnings) ->
        let result = do
                C.parseWarnings (C.toPWarnings lexWarnings)
                for_ (C.validateUTF8 bs) $ \pos ->
                    C.parseWarning C.zeroPos C.PWTUTF $
                        "UTF8 encoding problem at byte offset " ++ show pos
                parser fields
        in case snd (C.runParseResult result) of
            Right x            -> Right x
            Left (_mver, errs) -> Left $ unlines
                [ fp ++ ":" ++ C.showPos pos ++ ": " ++ msg
                | C.PError pos msg <- toList errs
                ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

exitWithExc :: Exception e => e -> IO a
exitWithExc e = do
    hPutStrLn stderr $ displayException e
    exitFailure

makeChangelog :: Opts -> IO ()
makeChangelog Opts {..} = do
    cfg <- do
        let filename = optDirectory </> "config"
        contents <- BS.readFile filename
        either (exitWithExc . PlainError) return $
            parseWith parseConfig filename contents

    dirContents <- filter (not . isTmpFile) <$> listDirectory optDirectory
    allEntries <- fmap Map.fromList $
      for (filter (/= "config") $ sort dirContents) $ \name -> do
        let fp = optDirectory </> name
        contents <- BS.readFile fp
        entry <- parseEntryFile fp contents
        pure (fp, entry)

    let validationErrors = Map.filter (not . null) $ validateEntry cfg <$> allEntries
    unless (null validationErrors) $ do
      void $ flip Map.traverseWithKey validationErrors $ \fp errors -> do
        hPutStrLn stderr $ "Validation error(s) in entry `" ++ fp ++ "`:"
        for_ errors $ hPutStrLn stderr . ("  "++) . displayException
      exitWithExc $ PlainError "Validation failed."

    -- Check expected MR number if specified
    for_ optExpectMR $ \expectedMR -> do
      let expectedMRNum = MRNumber expectedMR
          entriesWithMR = Map.filter (\e -> expectedMRNum `Set.member` entryMrs e) allEntries
      when (Map.null entriesWithMR && not (Map.null allEntries)) $ do
        hPutStrLn stderr $ "Warning: No changelog entry references this MR (!" ++ show expectedMR ++ ")."
        hPutStrLn stderr $ "Add 'mrs: !" ++ show expectedMR ++ "' to your changelog entry."
        hPutStrLn stderr ""
        exitFailure

    unless optValidate $
      outputRST optDirectory optVersion cfg (Map.elems allEntries)

-------------------------------------------------------------------------------
-- RST output
-------------------------------------------------------------------------------

outputRST :: FilePath -> Maybe String -> Cfg -> [Entry] -> IO ()
outputRST dir mVersion Cfg{..} entries = do
    -- File header
    for_ mVersion $ \ver -> do
        let label = ".. _release-" ++ map dotToDash ver ++ ":"
            title = "Version " ++ ver
        putStrLn label
        putStrLn ""
        putStrLn title
        putStrLn $ replicate (length title) '='
        unless (null cfgPreamble) $ do
            putStrLn ""
            putStrLn $ substituteVersion ver (trim cfgPreamble)

    -- Grouped release notes
    let grouped = groupBySections cfgSections entries
    for_ grouped $ \(displayName, sectionEntries) -> do
        putStrLn ""
        putStrLn displayName
        putStrLn $ replicate (length displayName) '~'
        putStrLn ""
        for_ (sortBy (flip compare `on` hasDescription) sectionEntries) $ \entry ->
            putStr $ formatEntry entry

    -- Generate Included libraries section
    let baseDir = takeDirectory (dropTrailingPathSeparator dir)
    when (isJust mVersion && not (null cfgIncludedLibraries)) $
            generateIncludedLibraries baseDir cfgIncludedLibrariesPreamble cfgIncludedLibraries
  where
    dotToDash '.' = '-'
    dotToDash c   = c

-- | Replace $version$ and $major_version$ placeholders in a template string.
substituteVersion :: String -> String -> String
substituteVersion ver = go
  where
    majorVer = intercalate "." $ take 2 $ splitOn '.' ver

    go [] = []
    go ('$':rest) = case break (== '$') rest of
        ("version", '$':after)       -> ver ++ go after
        ("major_version", '$':after) -> majorVer ++ go after
        _                            -> '$' : go rest
    go (c:rest) = c : go rest

    splitOn :: Char -> String -> [String]
    splitOn _ [] = [""]
    splitOn d s  = case break (== d) s of
        (w, [])   -> [w]
        (w, _:r)  -> w : splitOn d r

formatEntry :: Entry -> String
formatEntry Entry {..} =
    indentBullet $ header ++ "\n" ++ description
  where
    header = unwords $
        [ entrySynopsis ] ++
        [ "(:ghc-ticket:`" ++ show n ++ "`)"
        | IssueNumber n <- Set.toList entryIssues
        ] ++
        [ "(:ghc-mr:`" ++ show n ++ "`)"
        | MRNumber n <- Set.toList entryMrs
        ]

    description = maybe "" (\d -> "\n" ++ trim d ++ "\n\n") entryDescription

-------------------------------------------------------------------------------
-- Included libraries section generation
-------------------------------------------------------------------------------

generateIncludedLibraries :: FilePath -> String -> [(FilePath, String)] -> IO ()
generateIncludedLibraries baseDir preamble libs = do
    putStrLn ""
    putStrLn "Included libraries"
    putStrLn "~~~~~~~~~~~~~~~~~~"
    unless (null preamble) $ do
        putStrLn ""
        putStrLn $ trim preamble
    putStrLn ""

    rows <- fmap (sortBy (compare `on` fst3)) $
        for libs $ \(cabalPath, reason) -> do
            contents <- readFile (baseDir </> cabalPath)
            let name = extractField "name" contents
                ver  = extractField "version" contents
            case (name, ver) of
                (Just n, Just v) -> return (n, v, reason)
                _ -> do
                    hPutStrLn stderr $
                        "Warning: could not parse name/version from " ++ cabalPath
                    return (cabalPath, "?.?.?", reason)

    let nameW = maximum $ length ("Package" :: String) : map (\(n,_,_) -> length n) rows
        verW  = maximum $ length ("Version" :: String) : map (\(_,v,_) -> length v) rows
        pad w s = s ++ replicate (w - length s) ' '
        sep = replicate nameW '=' ++ "  " ++ replicate verW '=' ++ "  " ++ replicate 40 '='

    putStrLn sep
    putStrLn $ pad nameW "Package" ++ "  " ++ pad verW "Version" ++ "  " ++ "Reason for inclusion"
    putStrLn sep
    for_ rows $ \(name, ver, reason) ->
        putStrLn $ pad nameW name ++ "  " ++ pad verW ver ++ "  " ++ reason
    putStrLn sep
  where
    fst3 (a, _, _) = a

    extractField :: String -> String -> Maybe String
    extractField fieldName contents =
        case mapMaybe (matchField fieldName) (lines contents) of
            (v:_) -> Just v
            []    -> Nothing

    matchField :: String -> String -> Maybe String
    matchField fieldName line =
        let stripped = dropWhile isSpace line
            (key, rest) = break (\c -> c == ':' || isSpace c) stripped
        in if map toLower' key == map toLower' fieldName
           then case dropWhile isSpace rest of
                    (':':val) -> Just (trim (dropWhile isSpace val))
                    _         -> Nothing
           else Nothing

    toLower' c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise             = c

-------------------------------------------------------------------------------
-- Section grouping
-------------------------------------------------------------------------------

groupBySections :: [(String, String)] -> [Entry] -> [(String, [Entry])]
groupBySections sectionDefs entries =
    [ (displayName, sectionEntries)
    | (key, displayName) <- sectionDefs
    , let sectionEntries = filter (hasSection key) entries
    , not (null sectionEntries)
    ]
  where
    hasSection key e = case entrySection e of
        Just (Section s) -> s == key
        Nothing          -> False

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Opts = Opts
    { optDirectory :: FilePath
    , optVersion   :: Maybe String
    , optValidate  :: Bool
    , optExpectMR  :: Maybe Int        -- ^ Expected MR number
    }
  deriving (Show)

newtype IssueNumber = IssueNumber Int
  deriving (Eq, Ord, Show)

instance C.Parsec IssueNumber where
    parsec = do
        _ <- P.char '#'
        IssueNumber <$> P.integral

instance C.Pretty IssueNumber where
    pretty (IssueNumber n) = PP.char '#' PP.<> PP.int n

newtype MRNumber = MRNumber Int
  deriving (Eq, Ord, Show)

instance C.Parsec MRNumber where
    parsec = do
        _ <- P.char '!'
        MRNumber <$> P.integral

instance C.Pretty MRNumber where
    pretty (MRNumber n) = PP.char '!' PP.<> PP.int n

newtype Section = Section String
  deriving (Eq, Ord, Show)

instance C.Parsec Section where
    parsec = Section <$> C.parsecToken

instance C.Pretty Section where
    pretty (Section s) = PP.text s

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

data Cfg = Cfg
    { cfgRequiredFields            :: Set String
    , cfgSections                  :: [(String, String)]  -- ^ (key, displayName)
    , cfgPreamble                  :: String
    , cfgIncludedLibraries         :: [(FilePath, String)] -- ^ (cabalPath, description)
    , cfgIncludedLibrariesPreamble :: String
    }
  deriving (Show)

parseConfig :: [C.Field C.Position] -> C.ParseResult Cfg
parseConfig fields0 = do
    traverse_ warnSection $ concat sections
    raw <- C.parseFieldGrammar C.cabalSpecLatest fields cfgRawGrammar
    return Cfg
        { cfgRequiredFields            = cfgRawRequiredFields raw
        , cfgSections                  = parseSections (cfgRawSections raw)
        , cfgPreamble                  = cfgRawPreamble raw
        , cfgIncludedLibraries         = parseIncludedLibraries (cfgRawIncludedLibraries raw)
        , cfgIncludedLibrariesPreamble = cfgRawIncludedLibrariesPreamble raw
        }
  where
    (fields, sections) = C.partitionFields fields0

    warnSection :: C.Section C.Position -> C.ParseResult ()
    warnSection (C.MkSection (C.Name pos name) _ _) =
        C.parseWarning pos C.PWTUnknownSection $ "Unknown section " ++ C.fromUTF8BS name

data CfgRaw = CfgRaw
    { cfgRawRequiredFields            :: Set String
    , cfgRawSections                  :: String
    , cfgRawPreamble                  :: String
    , cfgRawIncludedLibraries         :: String
    , cfgRawIncludedLibrariesPreamble :: String
    }

cfgRawRequiredFieldsL :: Functor f => (Set String -> f (Set String)) -> CfgRaw -> f CfgRaw
cfgRawRequiredFieldsL f s = (\x -> s { cfgRawRequiredFields = x }) <$> f (cfgRawRequiredFields s)

cfgRawSectionsL :: Functor f => (String -> f String) -> CfgRaw -> f CfgRaw
cfgRawSectionsL f s = (\x -> s { cfgRawSections = x }) <$> f (cfgRawSections s)

cfgRawPreambleL :: Functor f => (String -> f String) -> CfgRaw -> f CfgRaw
cfgRawPreambleL f s = (\x -> s { cfgRawPreamble = x }) <$> f (cfgRawPreamble s)

cfgRawIncludedLibrariesL :: Functor f => (String -> f String) -> CfgRaw -> f CfgRaw
cfgRawIncludedLibrariesL f s = (\x -> s { cfgRawIncludedLibraries = x }) <$> f (cfgRawIncludedLibraries s)

cfgRawIncludedLibrariesPreambleL :: Functor f => (String -> f String) -> CfgRaw -> f CfgRaw
cfgRawIncludedLibrariesPreambleL f s = (\x -> s { cfgRawIncludedLibrariesPreamble = x }) <$> f (cfgRawIncludedLibrariesPreamble s)

cfgRawGrammar :: C.ParsecFieldGrammar CfgRaw CfgRaw
cfgRawGrammar = CfgRaw
    <$> C.monoidalFieldAla "required-fields"              (C.alaSet' C.FSep C.Token) cfgRawRequiredFieldsL
    <*> C.freeTextFieldDef "sections"                     cfgRawSectionsL
    <*> C.freeTextFieldDef "preamble"                     cfgRawPreambleL
    <*> C.freeTextFieldDef "included-libraries"           cfgRawIncludedLibrariesL
    <*> C.freeTextFieldDef "included-libraries-preamble"  cfgRawIncludedLibrariesPreambleL

parseSections :: String -> [(String, String)]
parseSections = mapMaybe parseLine . lines
  where
    parseLine l = case words (trim l) of
        []       -> Nothing
        (k:rest) -> Just (k, unwords rest)

parseIncludedLibraries :: String -> [(FilePath, String)]
parseIncludedLibraries = mapMaybe parseLine . lines
  where
    parseLine l = case trim l of
        [] -> Nothing
        s  -> case break isSpace s of
            (path, rest) | not (null path) -> Just (path, trim rest)
            _ -> Nothing

-------------------------------------------------------------------------------
-- Entry
-------------------------------------------------------------------------------

data Entry = Entry
    { entrySynopsis     :: String
    , entryDescription  :: Maybe String
    , entryMrs          :: Set MRNumber
    , entryIssues       :: Set IssueNumber
    , entrySection      :: Maybe Section
    }
  deriving (Show)

hasDescription :: Entry -> Bool
hasDescription = isJust . entryDescription

entrySynopsisL :: Functor f => (String -> f String) -> Entry -> f Entry
entrySynopsisL f s = (\x -> s { entrySynopsis = x }) <$> f (entrySynopsis s)

entryDescriptionL :: Functor f => (Maybe String -> f (Maybe String)) -> Entry -> f Entry
entryDescriptionL f s = (\x -> s { entryDescription = x }) <$> f (entryDescription s)

entryMrsL :: Functor f => (Set MRNumber -> f (Set MRNumber)) -> Entry -> f Entry
entryMrsL f s = (\x -> s { entryMrs = x }) <$> f (entryMrs s)

entryIssuesL :: Functor f => (Set IssueNumber -> f (Set IssueNumber)) -> Entry -> f Entry
entryIssuesL f s = (\x -> s { entryIssues = x }) <$> f (entryIssues s)

entrySectionL :: Functor f => (Maybe Section -> f (Maybe Section)) -> Entry -> f Entry
entrySectionL f s = (\x -> s { entrySection = x }) <$> f (entrySection s)

parseEntryFile :: FilePath -> BS.ByteString -> IO Entry
parseEntryFile fp contents =
    either reportParseError return $ parseWith parseEntry fp contents
  where
    reportParseError err = exitWithExc $ PlainError $
      err ++ "\nChangelog entries use Cabal field syntax; see utils/changelog-d/README.md for details."

parseEntry :: [C.Field C.Position] -> C.ParseResult Entry
parseEntry fields0 = do
    traverse_ warnSection $ concat sections
    e <- C.parseFieldGrammar C.cabalSpecLatest fields entryGrammar
    when (null $ entrySynopsis e) $
        C.parseFatalFailure C.zeroPos "Synopsis cannot be empty"
    return e
  where
    (fields, sections) = C.partitionFields fields0

    warnSection :: C.Section C.Position -> C.ParseResult ()
    warnSection (C.MkSection (C.Name pos name) _ _) =
        C.parseWarning pos C.PWTUnknownSection $ "Unknown section " ++ C.fromUTF8BS name

entryGrammar :: C.ParsecFieldGrammar Entry Entry
entryGrammar = Entry
    <$> C.freeTextFieldDef "synopsis"                              entrySynopsisL
    <*> C.freeTextField    "description"                           entryDescriptionL
    <*> C.monoidalFieldAla "mrs"          (C.alaSet C.NoCommaFSep) entryMrsL
    <*> C.monoidalFieldAla "issues"       (C.alaSet C.NoCommaFSep) entryIssuesL
    <*> C.optionalField    "section"                               entrySectionL

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

data ValidationError
    = RequiredFieldError RequiredFieldError
    | UnknownSection String

instance Exception ValidationError
instance Show ValidationError where
  show (RequiredFieldError err) = show err
  show (UnknownSection s) = "Unknown section `" ++ s ++ "`"

data RequiredFieldError
    = MissingRequiredField String
    | UnknownRequiredField String

instance Show RequiredFieldError where
  show (MissingRequiredField f) = "Missing required field `" ++ f ++ "`"
  show (UnknownRequiredField f) = "Unknown required field `" ++ f ++ "`"

type Validator = Cfg -> Entry -> [ValidationError]

validateEntry :: Validator
validateEntry cfg entry = foldMap (\validator -> validator cfg entry)
  [ validateRequiredFields
  , validateSection
  ]

validateRequiredFields :: Validator
validateRequiredFields Cfg{..} Entry{..} = fmap RequiredFieldError $
  mapMaybe checkField $ Set.toList cfgRequiredFields
  where
    checkField :: String -> Maybe RequiredFieldError
    checkField reqField = case fieldIsEmpty reqField of
      Left err   -> Just err
      Right True -> Just $ MissingRequiredField reqField
      Right False -> Nothing

    fieldIsEmpty "synopsis"     = pure $ null entrySynopsis
    fieldIsEmpty "description"  = pure $ isNothing entryDescription
    fieldIsEmpty "mrs"          = pure $ null entryMrs
    fieldIsEmpty "issues"       = pure $ null entryIssues
    fieldIsEmpty "section"      = pure $ isNothing entrySection
    fieldIsEmpty f              = Left $ UnknownRequiredField f

validateSection :: Validator
validateSection Cfg{..} Entry{..} =
    case entrySection of
      Nothing          -> []
      Just (Section s)
          | null cfgSections                  -> []
          | any (\(k, _) -> k == s) cfgSections -> []
          | otherwise                         -> [UnknownSection s]

-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

newtype PlainError = PlainError String

instance Exception PlainError
instance Show PlainError where
  show (PlainError s) = "error: " ++ s

indentBullet :: String -> String
indentBullet = unlines . go . lines
  where
    go []     = []
    go (x:xs) = ("- " ++ x) : map indentLine xs
    indentLine "" = ""
    indentLine str = "  " ++ str

trim :: String -> String
trim = tr . tr where tr = dropWhile isSpace . reverse

isTmpFile :: FilePath -> Bool
isTmpFile ('.' : _) = True
isTmpFile _ = False

