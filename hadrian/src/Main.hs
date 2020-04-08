module Main (main) where

import System.Directory (getCurrentDirectory)
import Development.Shake
import Hadrian.Expression
import Hadrian.Utilities
import Settings.Parser

import qualified Base
import qualified CommandLine
import qualified Environment
import qualified Rules
import qualified Rules.Clean
import qualified Rules.Documentation
import qualified Rules.Nofib
import qualified Rules.SourceDist
import qualified Rules.Selftest
import qualified Rules.Test
import qualified UserSettings

main :: IO ()
main = do
    -- Provide access to command line arguments and some user settings through
    -- Shake's type-indexed map 'shakeExtra'.
    argsMap <- CommandLine.cmdLineArgsMap
    let extra = insertExtra UserSettings.buildProgressColour
              $ insertExtra UserSettings.successColour
              $ insertExtra (VerboseCommand UserSettings.verboseCommand) argsMap

        BuildRoot buildRoot = CommandLine.lookupBuildRoot argsMap

        rebuild = [ (RebuildLater, buildRoot -/- "stage0/**")
                  | CommandLine.lookupFreeze1 argsMap ||
                    CommandLine.lookupFreeze2 argsMap
                  ] ++
                  [ (RebuildLater, buildRoot -/- "stage1/**")
                  | CommandLine.lookupFreeze2 argsMap
                  ]

    cwd <- getCurrentDirectory
    shakeColor <- shouldUseColor
    let options :: ShakeOptions
        options = shakeOptions
            { shakeChange   = ChangeModtimeAndDigest
            , shakeFiles    = buildRoot -/- Base.shakeFilesDir
            , shakeProgress = progressSimple
            , shakeRebuild  = rebuild
            , shakeTimings  = True
            , shakeColor    = shakeColor
            , shakeExtra    = extra

            -- Setting shakeSymlink to False ensures files are copied out of
            -- shake's cloud cache instead of hard linked. This is important as
            -- the hard link mode makes all such files read only to avoid
            -- accidentally modifying cache files via the hard link. It turns
            -- out, many Hadrian rules attempt read access to such files and
            -- hence would in the hard link mode. These rules could be
            -- refactored to avoid write access, but setting shakeSymlink to
            -- False is a much simpler solution.
            , shakeSymlink  = False

            -- Enable linting file accesses in the build dir and ghc root dir
            -- (cwd) when using the `--lint-fsatrace` option.
            , shakeLintInside = [ cwd, buildRoot ]
            , shakeLintIgnore =
                -- Ignore access to the package database caches.
                -- They are managed externally by the ghc-pkg tool.
                [ buildRoot -/- "**/package.conf.d/package.cache"

                -- Ignore access to autom4te.cache directories.
                -- They are managed externally by auto tools.
                , "//autom4te.cache/**"

                -- Ignore in-tree GMP objects
                , buildRoot -/- "**/gmp/objs/**"
                ]
            }

        rules :: Rules ()
        rules = do
            Rules.buildRules
            Rules.Documentation.documentationRules
            Rules.Clean.cleanRules
            Rules.Nofib.nofibRules
            Rules.oracleRules
            Rules.Selftest.selftestRules
            Rules.SourceDist.sourceDistRules
            Rules.Test.testRules
            Rules.topLevelTargets
            Rules.toolArgsTarget

    shakeArgsWith options CommandLine.optDescrs $ \_ targets -> do
        let targets' = filter (not . null) $ removeKVs targets
        Environment.setupEnvironment
        return . Just $ if null targets'
                        then rules
                        else want targets' >> withoutActions rules
