module Main (main) where

import Development.Shake
import Hadrian.Utilities
import Settings.Parser
import System.Directory (getCurrentDirectory)
import System.IO
import System.Exit
import System.Environment
import Control.Exception
import Data.IORef

import qualified Base
import qualified CommandLine
import qualified Environment
import qualified Rules
import qualified Rules.Clean
import qualified Rules.Docspec
import qualified Rules.Documentation
import qualified Rules.Lint
import qualified Rules.Nofib
import qualified Rules.Selftest
import qualified Rules.SourceDist
import qualified Rules.Test
import qualified UserSettings

main :: IO ()
main = do
    -- Provide access to command line arguments and some user settings through
    -- Shake's type-indexed map 'shakeExtra'.
    argsMap <- CommandLine.cmdLineArgsMap
    let extra = insertExtra UserSettings.buildProgressColour
              $ insertExtra UserSettings.successColour
              $ argsMap

        BuildRoot buildRoot = CommandLine.lookupBuildRoot argsMap

        rebuild = [ (RebuildLater, buildRoot -/- "stage0/**")
                  | CommandLine.lookupFreeze1 argsMap ||
                    CommandLine.lookupFreeze2 argsMap
                  ] ++
                  [ (RebuildLater, buildRoot -/- "stage1/**")
                  | CommandLine.lookupFreeze2 argsMap
                  ] ++
                  (if CommandLine.lookupSkipDepends argsMap
                   then [(RebuildLater, buildRoot -/- "**/.dependencies.mk"), (RebuildLater, buildRoot -/- "**/.dependencies")]
                   else [])

    cwd <- getCurrentDirectory
    shakeColor <- shouldUseColor
    let options :: ShakeOptions
        options = shakeOptions
            { shakeChange   = ChangeModtimeAndDigest
            , shakeFiles    = buildRoot -/- Base.shakeFilesDir
            , shakeProgress = progressSimple
            , shakeRebuild  = rebuild
            , shakeTimings  = False
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
            , shakeOutput = \v -> case v of
                -- We don't want to print internal shake diagnostic messages as
                -- they are too verbose to be of any use. See #20484.
                Diagnostic -> const (pure ())
                _ -> shakeOutput shakeOptions v
            }

        rules :: Rules ()
        rules = do
            Rules.buildRules
            Rules.Docspec.docspecRules
            Rules.Documentation.documentationRules
            Rules.Clean.cleanRules
            Rules.Lint.lintRules
            Rules.Nofib.nofibRules
            Rules.oracleRules
            Rules.Selftest.selftestRules
            Rules.SourceDist.sourceDistRules
            Rules.Test.testRules
            Rules.topLevelTargets
            Rules.toolArgsTarget

    -- This IORef is used to communicate the result of shake parsing
    -- command line options (which happens in shakeArgsOptionsWith, but
    -- isn't exposed to the user) to the exception handler, which uses the
    -- verbosity and colour information to decide how much of the error to display.
    shake_opts_var <- newIORef options
    handleShakeException shake_opts_var $ shakeArgsOptionsWith options CommandLine.optDescrs $ \shake_opts _ targets -> do
        writeIORef  shake_opts_var shake_opts
        let targets' = filter (not . null) $ removeKVs targets
        Environment.setupEnvironment
        return . Just $ (shake_opts, if null targets'
                                      then rules
                                      else want targets' >> withoutActions rules)

handleShakeException :: IORef ShakeOptions -> IO a -> IO a
handleShakeException shake_opts_var shake_run = do
  args <- getArgs
  -- Using withArgs here is a bit of a hack but the API doesn't allow another way
  -- See https://github.com/ndmitchell/shake/issues/811
  -- Passing --exception means shake throws an exception rather than
  -- catching ShakeException and displaying the error itself to the user.
  catch (withArgs ("--exception" : args) $ shake_run) $ \(_e :: ShakeException) -> do
    shake_opts <- readIORef shake_opts_var
    let
      FailureColour col = lookupExtra red (shakeExtra shake_opts)
      esc = if shakeColor shake_opts then escape col else id
    if shakeVerbosity shake_opts >= Verbose
      then
        hPrint stderr _e
      else
        -- The SomeExceptionWithLocation here is normally an IOError which lacks
        -- very much structure, in the future we could try to catch
        -- a more structured exception and further refine the
        -- displayed output. https://github.com/ndmitchell/shake/pull/812
        hPrint stderr (shakeExceptionInner _e)
    hPutStrLn stderr (esc "Build failed.")
    exitFailure

escForeground :: String -> String
escForeground code = "\ESC[" ++ code ++ "m"

escNormal :: String
escNormal = "\ESC[0m"

escape :: String -> String -> String
escape code x = escForeground code ++ x ++ escNormal
