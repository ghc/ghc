module Rules.Changelog (changelogRules) where

import Base
import CommandLine
import Oracles.Setting (topDirectory, setting, Setting(..))
import Packages
import Settings.Program (programContext)

import qualified System.Directory as IO
import qualified System.Environment as IO

-- | Rules for generating and managing changelog entries.
--
-- Targets:
--   hadrian/build changelog                              -- generate RST release notes
--   hadrian/build changelog --changelog-version=10.2.1   -- with explicit version
--   hadrian/build libraries-changelog-markdown           -- emit per-library Markdown bullets to stdout
--   hadrian/build list-markdown-targets                  -- print one repo-relative path per
--                                                          markdown-targets: row, used by CI
--   hadrian/build changelog-clear                        -- remove old entries
changelogRules :: Rules ()
changelogRules = do
    phony "changelog" $ do
        mVersion <- cmdChangelogVersion
        version <- case mVersion of
            Just v  -> return v
            Nothing -> setting ProjectVersion

        ctx <- programContext stage0Boot changelogD
        progPath <- programPath ctx
        need [progPath]
        need templatedCabalFiles

        top <- topDirectory
        let outFile = top -/- "docs" -/- "users_guide" -/- version ++ "-notes.rst"
        quietly $ cmd (FileStdout outFile)
            [progPath] [top -/- "changelog.d/", "--version", version]
            :: Action ()
        putSuccess $ "| Generated release notes: " ++ outFile

    phony "libraries-changelog-markdown" $ do
        ctx <- programContext stage0Boot changelogD
        progPath <- programPath ctx
        need [progPath]
        need templatedCabalFiles

        top <- topDirectory
        cmd_ [progPath]
             [ top -/- "changelog.d/"
             , "--libraries-changelog-markdown"
             ]

    phony "list-markdown-targets" $ do
        ctx <- programContext stage0Boot changelogD
        progPath <- programPath ctx
        need [progPath]
        top <- topDirectory
        let args = [top -/- "changelog.d/", "--list-markdown-targets"]
        mOut <- liftIO $ IO.lookupEnv "TOOL_OUTPUT"
        case mOut of
          Nothing -> cmd_ [progPath] args
          Just fp -> quietly $ (cmd (FileStdout fp) [progPath] args :: Action ())

    phony "changelog-clear" $ do
        top <- topDirectory
        let dir = top -/- "changelog.d"
        entries <- liftIO $ IO.listDirectory dir
        let toRemove = filter (\f -> f /= "config" && not (isPrefixOf "." f)) entries
        liftIO $ mapM_ (IO.removeFile . (dir -/-)) toRemove
        putSuccess $ "| Removed " ++ show (length toRemove) ++ " changelog entries"
  where
    -- These cabal files are needed by changelog-d to determine the
    -- versions of packages shipped with GHC.
    templatedCabalFiles = map pkgCabalFile
        [ ghcBoot
        , ghcBootTh
        , ghcExperimental
        , ghcInternal
        , ghci
        , compiler
        , ghcHeap
        , templateHaskell
        , base
        ]
