{-# LANGUAGE CPP        #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad                               (unless, when)
import Data.Foldable                               (for_, traverse_)
import Data.List                                   (isPrefixOf, isSuffixOf, sort)
import Data.Maybe                                  (mapMaybe)
import Data.Monoid                                 (Sum (..))
import Data.String                                 (fromString)
import Distribution.License                        (License (..))
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Simple.Utils                   (fromUTF8LBS, ignoreBOM, toUTF8BS)
import System.Directory                            (getAppUserDataDirectory)
import System.Environment                          (getArgs)
import System.Exit                                 (exitFailure)
import System.FilePath                             ((</>))

import Data.Orphans ()

import qualified Codec.Archive.Tar                      as Tar
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Char8                  as B8
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.Map                               as Map
import qualified Distribution.PackageDescription.Parse  as ReadP
import qualified Distribution.PackageDescription.Parsec as Parsec
import qualified Distribution.Parsec.Common             as Parsec
import qualified Distribution.Parsec.Parser             as Parsec
import qualified Distribution.ParseUtils                as ReadP

import           Distribution.Compat.Lens
import qualified Distribution.Types.BuildInfo.Lens                 as L
import qualified Distribution.Types.Executable.Lens                as L
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.Library.Lens                   as L
import qualified Distribution.Types.PackageDescription.Lens        as L
import qualified Distribution.Types.SourceRepo.Lens                as L

#ifdef HAS_STRUCT_DIFF
import DiffInstances ()
import StructDiff
#endif

parseIndex :: Monoid a => (FilePath -> BSL.ByteString -> IO a) -> IO a
parseIndex action = do
    cabalDir  <- getAppUserDataDirectory "cabal"
    cfg       <- B.readFile (cabalDir </> "config")
    cfgFields <- either (fail . show) pure $ Parsec.readFields cfg
    let repos        = reposFromConfig cfgFields
        repoCache    = case lookupInConfig "remote-repo-cache" cfgFields of
            []        -> cabalDir </> "packages"  -- Default
            (rrc : _) -> rrc                      -- User-specified
        tarName repo = repoCache </> repo </> "01-index.tar"
    mconcat <$> traverse (parseIndex' action . tarName) repos


parseIndex' :: Monoid a => (FilePath -> BSL.ByteString -> IO a) -> FilePath -> IO a
parseIndex' action path = do
    putStrLn $ "Reading index from: " ++ path
    contents <- BSL.readFile path
    let entries = Tar.read contents
    Tar.foldEntries (\e m -> mappend <$> f e <*> m) (return mempty) (fail . show) entries

  where
    f entry = case Tar.entryContent entry of
        Tar.NormalFile contents _
            | ".cabal" `isSuffixOf` fpath -> action fpath contents
            | otherwise                   -> return mempty
        Tar.Directory -> return mempty
        _             -> putStrLn ("Unknown content in " ++ fpath) >> return mempty
     where
       fpath = Tar.entryPath entry

readFieldTest :: FilePath -> BSL.ByteString -> IO ()
readFieldTest fpath bsl = case Parsec.readFields $ bslToStrict bsl of
    Right _  -> return ()
    Left err -> putStrLn $ fpath ++ "\n" ++ show err

-- | Map with unionWith monoid
newtype M k v = M (Map.Map k v)
    deriving (Show)
instance (Ord k, Monoid v) => Monoid (M k v) where
    mempty = M Map.empty
    mappend (M a) (M b) = M (Map.unionWith mappend a b)

compareTest
    :: String  -- ^ prefix of first packages to start traversal
    -> FilePath -> BSL.ByteString -> IO (Sum Int, Sum Int, M Parsec.PWarnType (Sum Int))
compareTest pfx fpath bsl
    | not $ pfx `isPrefixOf` fpath   = mempty
    | otherwise = do
    let str = ignoreBOM $ fromUTF8LBS bsl

    putStrLn $ "::: " ++ fpath
    (readp, readpWarnings)  <- case ReadP.parseGenericPackageDescription str of
        ReadP.ParseOk ws x    -> return (x, ws)
        ReadP.ParseFailed err -> print err >> exitFailure
    traverse_ (putStrLn . ReadP.showPWarning fpath) readpWarnings

    let (warnings, errors, parsec') = Parsec.runParseResult $ Parsec.parseGenericPackageDescription (bslToStrict bsl)
    traverse_ (putStrLn . Parsec.showPWarning fpath) warnings
    traverse_ (putStrLn . Parsec.showPError fpath) errors
    parsec <- maybe (print readp >> exitFailure) return parsec'

    let patchLocation (Just "") = Nothing
        patchLocation x         = x

    -- Old parser is broken for many descriptions, and other free text fields
    let readp0  = readp
            & L.packageDescription . L.description .~ ""
            & L.packageDescription . L.synopsis    .~ ""
            & L.packageDescription . L.maintainer  .~ ""
            -- ReadP parses @location:@ as @repoLocation = Just ""@
            & L.packageDescription . L.sourceRepos . traverse . L.repoLocation %~ patchLocation
            & L.condExecutables  . traverse . _2 . traverse . L.exeName .~ fromString ""
            -- custom fields: no order
            & L.buildInfos . L.customFieldsBI %~ sort
    let parsec0  = parsec
            & L.packageDescription . L.description .~ ""
            & L.packageDescription . L.synopsis    .~ ""
            & L.packageDescription . L.maintainer  .~ ""
            -- ReadP doesn't (always) parse sublibrary or executable names
            & L.condSubLibraries . traverse . _2 . traverse . L.libName .~ Nothing
            & L.condExecutables  . traverse . _2 . traverse . L.exeName .~ fromString ""
            -- custom fields: no order. TODO: see if we can preserve it.
            & L.buildInfos . L.customFieldsBI %~ sort

    -- hs-source-dirs ".", old parser broken
    -- See e.g. http://hackage.haskell.org/package/hledger-ui-0.27/hledger-ui.cabal executable
    let parsecHsSrcDirs = parsec0 & toListOf (L.buildInfos . L.hsSourceDirs)
    let readpHsSrcDirs  = readp0  & toListOf (L.buildInfos . L.hsSourceDirs)
    let filterDotDirs   = filter (/= ".")

    let parsec1 = if parsecHsSrcDirs /= readpHsSrcDirs && fmap filterDotDirs parsecHsSrcDirs == readpHsSrcDirs
        then parsec0 & L.buildInfos . L.hsSourceDirs %~ filterDotDirs
        else parsec0

    -- Compare two parse results
    -- ixset-1.0.4 has invalid prof-options, it's the only exception!
    unless (readp0 == parsec1 || fpath == "ixset/1.0.4/ixset.cabal") $ do
#if HAS_STRUCT_DIFF
            prettyResultIO $ diff readp parsec
#else
            putStrLn "<<<<<<"
            print readp
            putStrLn "======"
            print parsec
            putStrLn ">>>>>>"
#endif
            exitFailure

    let readpWarnCount  = Sum (length readpWarnings)
    let parsecWarnCount = Sum (length warnings)

    when (readpWarnCount > parsecWarnCount) $ do
        putStrLn "There are more readpWarnings"
        -- hint has -- in brace syntax, readp thinks it's a section
        -- http://hackage.haskell.org/package/hint-0.3.2.3/revision/0.cabal
        unless ("/hint.cabal" `isSuffixOf` fpath) exitFailure

    let parsecWarnMap   = foldMap (\(Parsec.PWarning t _ _) -> M $ Map.singleton t 1) warnings
    return (readpWarnCount, parsecWarnCount, parsecWarnMap)

parseReadpTest :: FilePath -> BSL.ByteString -> IO ()
parseReadpTest fpath bsl = do
    let str = ignoreBOM $ fromUTF8LBS bsl
    case ReadP.parseGenericPackageDescription str of
        ReadP.ParseOk _ _     -> return ()
        ReadP.ParseFailed err -> putStrLn fpath >> print err >> exitFailure

parseParsecTest :: String -> FilePath -> BSL.ByteString -> IO (Sum Int)
parseParsecTest pfx fpath _   | not (pfx `isPrefixOf` fpath) = return (Sum 0)
parseParsecTest _   fpath bsl = do
    let bs = bslToStrict bsl
    let (_warnings, errors, parsec) = Parsec.runParseResult $ Parsec.parseGenericPackageDescription bs
    case parsec of
        Just _ -> return (Sum 1)
        Nothing -> do
            traverse_ (putStrLn . Parsec.showPError fpath) errors
            exitFailure

roundtripTest :: String -> FilePath -> BSL.ByteString -> IO (Sum Int)
roundtripTest pfx fpath _ | not (pfx `isPrefixOf` fpath) = return (Sum 0)
roundtripTest _   fpath bsl = do
    let bs = bslToStrict bsl
    x0 <- parse "1st" bs
    let bs' = showGenericPackageDescription x0
    y0 <- parse "2nd" (toUTF8BS bs')

    -- 'License' type doesn't support parse . pretty roundrip (yet).
    -- Will be fixed when we refactor to SPDX
    let y1 = if x0 ^. L.packageDescription . L.license == UnspecifiedLicense
                && y0 ^. L.packageDescription . L.license == UnknownLicense "UnspecifiedLicense"
             then y0 & L.packageDescription . L.license .~ UnspecifiedLicense
             else y0

    -- license-files: ""
    let stripEmpty = filter (/="")
    let x1 = x0 & L.packageDescription . L.licenseFiles %~ stripEmpty
    let y2 = y1 & L.packageDescription . L.licenseFiles %~ stripEmpty

    let y = y2 & L.packageDescription . L.description .~ ""
    let x = x1 & L.packageDescription . L.description .~ ""

    unless (x == y || fpath == "ixset/1.0.4/ixset.cabal") $ do
        putStrLn fpath
#if HAS_STRUCT_DIFF
        prettyResultIO $ diff x y
#else
        putStrLn "<<<<<<"
        print x
        putStrLn "======"
        print y
        putStrLn ">>>>>>"

#endif
        putStrLn bs'
        exitFailure

    return (Sum 1)
  where
    parse phase c = do
        let (_, errs, x') = Parsec.runParseResult $ Parsec.parseGenericPackageDescription c
        case x' of
            Just gpd | null errs -> pure gpd
            _                    -> do
                putStrLn $ fpath ++ " " ++ phase
                traverse_ print errs
                B.putStr c
                fail "parse error"
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["read-field"]   -> parseIndex readFieldTest
        ["parse-readp"]  -> parseIndex parseReadpTest
        ["parse-parsec"] -> do
            Sum n <- parseIndex (parseParsecTest "")
            putStrLn $ show n ++ " files processed"
        ["parse-parsec", pfx] -> do
            Sum n <- parseIndex (parseParsecTest pfx)
            putStrLn $ show n ++ " files processed"
        ["roundtrip"] -> do
            Sum n <- parseIndex (roundtripTest "")
            putStrLn $ show n ++ " files processed"
        ["roundtrip", pfx] -> do
            Sum n <- parseIndex (roundtripTest pfx)
            putStrLn $ show n ++ " files processed"
        [pfx]            -> defaultMain pfx
        _                -> defaultMain ""
  where
    defaultMain pfx = do
        (Sum readpCount, Sum parsecCount, M warn) <- parseIndex (compareTest pfx)
        putStrLn $ "readp warnings: " ++ show readpCount
        putStrLn $ "parsec count:   " ++ show parsecCount
        for_ (Map.toList warn) $ \(t, Sum c) ->
            putStrLn $ " - " ++ show t ++ " : " ++ show c

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

bslToStrict :: BSL.ByteString -> B.ByteString
#if MIN_VERSION_bytestring(0,10,0)
bslToStrict = BSL.toStrict
#else
-- Not effective!
bslToStrict = B.concat . BSL.toChunks
#endif

-------------------------------------------------------------------------------
-- Index shuffling
-------------------------------------------------------------------------------

-- TODO: Use 'Cabal' for this?
reposFromConfig :: [Parsec.Field ann] -> [String]
reposFromConfig fields = takeWhile (/= ':') <$> mapMaybe f fields
  where
    f (Parsec.Field (Parsec.Name _ name) fieldLines)
        | B8.unpack name == "remote-repo" =
            Just $ fieldLinesToString fieldLines
    f (Parsec.Section (Parsec.Name _ name) [Parsec.SecArgName _ secName] _fieldLines)
        | B8.unpack name == "repository" =
            Just $ B8.unpack secName
    f _ = Nothing

-- | Looks up the given key in the cabal configuration file
lookupInConfig :: String -> [Parsec.Field ann] -> [String]
lookupInConfig key = mapMaybe f
  where
    f (Parsec.Field (Parsec.Name _ name) fieldLines)
        | B8.unpack name == key =
            Just $ fieldLinesToString fieldLines
    f _ = Nothing

fieldLinesToString :: [Parsec.FieldLine ann] -> String
fieldLinesToString fieldLines =
    B8.unpack $ B.concat $ bsFromFieldLine <$> fieldLines
  where
    bsFromFieldLine (Parsec.FieldLine _ bs) = bs
