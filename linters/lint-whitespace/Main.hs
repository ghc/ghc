{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}


module Main where

-- base
import           Control.Exception
  ( IOException, handle )
import           Control.Monad
  ( forM, forM_, unless, when )
import           Data.Maybe
  ( isJust, mapMaybe )
import           System.Environment
  ( getArgs )
import           System.Exit
  ( ExitCode(..), exitWith )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( alter, empty, keys, findWithDefault )

-- mtl
import           Control.Monad.Writer
  ( liftIO, execWriter, tell )

-- process
import System.Process
  ( readProcess )

-- text
import           Data.Text
  ( Text )
import qualified Data.Text    as T
import qualified Data.Text.IO as T
  ( readFile )

-- linters-common
import           Linters.Common
  ( LintMsg(..), LintLvl(..)
  , GitType(..)
  , gitCatBlob, gitDiffTree
  , z40
  )

--------------------------------------------------------------------------------

main :: IO ()
main = do
    getArgs >>= \case
      ("commits":dir:refs)  -> mainCommits dir refs
      ("commits":_)         -> fail "usage: lint-whitespace commits <git-repo> [<commit-id>+]"
      ("files":fs)          -> mainFiles fs
      ("tracked":args)      -> mainTracked args
      _ -> fail "usage: lint-whitespace <files|commits|tracked> ..."

mainCommits :: FilePath -> [String] -> IO ()
mainCommits dir refs = do

    stats <- forM (map T.pack refs) $ \ref -> do
      (cid,deltas) <- gitDiffTree dir ref

      lintMsgs0 <- forM deltas $ \(origs, (gt, blobId), fname) -> if (gt == GitTypeRegFile && hasSuffix fname)
        then do
          let blobIds0 = [ b0 | (GitTypeRegFile, b0, _) <- origs, b0 /= z40 ]
          blob1  <- gitCatBlob dir blobId
          blobs0 <- mapM (gitCatBlob dir) blobIds0

          -- blobs0 will be empty in case in case of newly added files as well as renames/copies
          -- blobs0 will contain more than one entry for merge-commits

          return [ (fname, msg) | msg <- lintBlob blobs0 blob1 ]
        else return []

      checkLintMsgs ("commit " <> T.unpack cid) lintMsgs0

    finalReport stats

finalReport :: [Maybe LintLvl] -> IO ()
finalReport stats = do
    unless (null $ filter isJust stats) $
        putStrLn "====================================================================================="

    let stats1 = maximum (Nothing : stats)

    -- unless (stats1 == Nothing) $ do
    --     putStrLn "There were commit message linter issues! For more information see"
    --     putStrLn ""

    unless (stats1 < Just LintLvlErr) $ do
        putStrLn "Validation FAILED because at least one commit had linter errors!"
        exitWith (ExitFailure 1)

    putStrLn "whitespace validation passed!"

checkLintMsgs :: String -> [[(Text, LintMsg)]] -> IO (Maybe LintLvl)
checkLintMsgs herald lintMsgs0 = do
      let lintMsgs = concat lintMsgs0
          status = maximum (Nothing : [ Just lvl | (_, LintMsg lvl _ _ _) <- lintMsgs ])
          ok     = status < Just LintLvlErr

      unless (null lintMsgs) $ liftIO $ do
          putStrLn "====================================================================================="
          putStrLn (herald <> " has whitespace linter issues:")
          putStrLn ""
          forM_ lintMsgs $ \(fn, LintMsg lvl lno l m) -> do
              let lvls = case lvl of
                      LintLvlErr  -> "*ERROR*"
                      LintLvlWarn -> "Warning"
              putStrLn (" " <> lvls <> " " <> T.unpack fn <> ":" <> show lno <> ": " <> T.unpack m)
              putStrLn (" > " <> show l)
              putStrLn ""
              return ()

      unless ok $ liftIO $
          putStrLn ("Validation FAILED for " <> herald)

      return status

mainFiles :: [FilePath] -> IO ()
mainFiles fs = do
  stats <- forM fs $ \f -> do
    lintMsgs0 <- handle (\(_ :: IOException) -> return []) (lintFile <$> T.readFile f)
    checkLintMsgs ("file " <> f) [[(T.pack f, err) | err <- lintMsgs0 ]]
  finalReport stats

mainTracked :: [String] -> IO ()
mainTracked args = do
  (ignoredFiles, ignoredDirs) <- parseTrackedArgs args
  allFiles <- lines <$> readProcess "git" ["ls-tree", "--name-only", "-r", "HEAD"] ""
  let files = filter (isTracked ignoredFiles ignoredDirs) allFiles
  mainFiles files

-- Check a file for trailing whitespace and tabs
lintFile :: Text -> [LintMsg]
lintFile blob1 = execWriter $ do
    when (hasTabs blob1) $ do
        tell [ LintMsg LintLvlErr lno l "introduces TAB"
             | (lno,l) <- zip [1..] lns
             , "\t" `T.isInfixOf` l
             ]

    when (hasTrail blob1) $ do
        tell [ LintMsg LintLvlErr lno l "introduces trailing whitespace"
             | (lno,l) <- zip [1..] lns, hasTrail l ]
  where
    lns = T.lines blob1

lintBlob :: [Text] -> Text -> [LintMsg]
lintBlob blobs0 blob1 = execWriter $ do
    -- Perform simple invariant-preservation checks
    when (hasTabs blob1 && not (any hasTabs blobs0)) $ do
        tell [ LintMsg LintLvlErr lno l "introduces TAB"
             | (lno,l) <- zip [1..] lns
             , "\t" `T.isInfixOf` l
             ]

    when (hasTrail blob1 && not (any hasTrail blobs0)) $ do
        tell [ LintMsg LintLvlErr lno l "introduces trailing whitespace"
             | (lno,l) <- zip [1..] lns, hasTrail l ]

    when (missingFinalEOL blob1) $ if not (any missingFinalEOL blobs0)
          then tell [LintMsg LintLvlErr  llno lln "lacking final EOL"]
          else tell [LintMsg LintLvlWarn llno lln "lacking final EOL"]

  where
    lns = T.lines blob1
    llno = length lns
    lln = case lns of
        [] -> ""
        _  -> last lns

hasTabs :: Text -> Bool
hasTabs = T.any (=='\t')

hasTrail :: Text -> Bool
hasTrail t = or [ " \n"    `T.isInfixOf`  t
                , " \r\n"  `T.isInfixOf`  t
                , "\t\r\n" `T.isInfixOf`  t
                , " "      `T.isSuffixOf` t
                , "\t"     `T.isSuffixOf` t
                ]

missingFinalEOL :: Text -> Bool
missingFinalEOL = not . T.isSuffixOf "\n"

--------------------------------------------------

data Flag
  = IgnoreFiles
  | IgnoreDirs
  | UnknownFlag Text
  deriving stock ( Eq, Ord, Show )

parseTrackedArgs :: [String] -> IO ([Text], [Text])
parseTrackedArgs args = do
  unless (null bareArgs) $
    fail "usage: lint-whitespace tracked --ignore-files ... --ignore-dirs ..."
  unless (null unknownFlags) $
    fail $ "lint-whitespace tracked: unknown flags " ++ show unknownFlags ++ "\n\
           \supported flags are --ignore-files and --ignore-dirs"
  return (ignoredFiles, ignoredDirs)
  where
    (bareArgs, flagArgs) = splitOn flagMaybe (map T.pack args)
    ignoredFiles = Map.findWithDefault [] IgnoreFiles flagArgs
    ignoredDirs  = Map.findWithDefault [] IgnoreDirs  flagArgs
    unknownFlags = mapMaybe (\case { UnknownFlag unk -> Just unk ; _ -> Nothing})
                 $ Map.keys flagArgs

-- Assumes the input string has no whitespace.
flagMaybe :: Text -> Maybe Flag
flagMaybe "-f" = Just IgnoreFiles
flagMaybe "--ignore-files" = Just IgnoreFiles
flagMaybe "-d" = Just IgnoreDirs
flagMaybe "--ignore-dirs" = Just IgnoreDirs
flagMaybe str
  | let (hyphens, rest) = T.span ((==) '-') str
  , not (T.null hyphens)
  = Just (UnknownFlag rest)
  | otherwise
  = Nothing

splitOn :: forall a b. Ord b => (a -> Maybe b) -> [a] -> ([a], Map b [a])
splitOn f = go Nothing
  where
    go :: Maybe b -> [a] -> ([a], Map b [a])
    go _ [] = ([], Map.empty)
    go mb_b (a:as) = case f a of
      Nothing ->
        case go mb_b as of
          (xs, yxs) ->
            case mb_b of
              Nothing -> (a:xs, yxs)
              Just b  -> (xs, Map.alter (alter_fn a) b yxs)
      Just b -> go (Just b) as
    alter_fn :: a -> Maybe [a] -> Maybe [a]
    alter_fn a Nothing   = Just [a]
    alter_fn a (Just as) = Just (a:as)

--------------------------------------------------
-- Predicates used to filter which files we lint.

hasSuffix :: Text -> Bool
hasSuffix fn = any (`T.isSuffixOf` fn) suffixes
  where
    suffixes = T.words ".hs .hsc .lhs .cabal .c .h .lhs-boot .hs-boot .x .y"

autogenFiles :: [ Text ]
autogenFiles = [ "WCsubst.c", "iconv.c", "Table.hs" ]

ignoredPrefixes :: [Text]
ignoredPrefixes = [ "testsuite/", "libraries/base/tests"
                  , "utils/hp2ps", "utils/hpc", "utils/unlit"
                  ]

isTracked :: [Text] -> [Text] -> FilePath -> Bool
isTracked ignoredFiles ignoredDirs (T.pack -> fn)
  =  hasSuffix fn
  && not (fn `elem` ignoredFiles)
  && not (any (`T.isPrefixOf` fn) ignoredDirs)
