module Rules.Changelog (changelogRules) where

import Base
import CommandLine
import Oracles.Setting (topDirectory, setting, Setting(..))
import Packages
import Settings.Program (programContext)

import qualified System.Directory as IO

-- | Rules for generating and managing changelog entries.
--
-- Targets:
--   hadrian/build changelog                              -- generate release notes
--   hadrian/build changelog --changelog-version=10.2.1   -- with explicit version
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

        -- These cabal files are needed by changelog-d to determine the
        -- versions of packages shipped with GHC.
        let templatedCabalFiles = map pkgCabalFile
                [ ghcBoot
                , ghcBootTh
                , ghcExperimental
                , ghcInternal
                , ghci
                , compiler
                , ghcHeap
                , templateHaskell
                ]
        need templatedCabalFiles

        top <- topDirectory
        let outFile = top -/- "docs" -/- "users_guide" -/- version ++ "-notes.rst"
        quietly $ cmd (FileStdout outFile)
            [progPath] [top -/- "changelog.d/", "--version", version]
            :: Action ()
        putSuccess $ "| Generated release notes: " ++ outFile

    phony "changelog-clear" $ do
        top <- topDirectory
        let dir = top -/- "changelog.d"
        entries <- liftIO $ IO.listDirectory dir
        let toRemove = filter (\f -> f /= "config" && not (isPrefixOf "." f)) entries
        liftIO $ mapM_ (IO.removeFile . (dir -/-)) toRemove
        putSuccess $ "| Removed " ++ show (length toRemove) ++ " changelog entries"
