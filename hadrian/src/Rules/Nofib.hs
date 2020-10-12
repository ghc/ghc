module Rules.Nofib where

import Base
import CommandLine
import Expression
import Oracles.Setting
import Packages

import System.Environment
import System.Exit

defNofibLogFile :: FilePath
defNofibLogFile = "nofib-log"

-- | Rules for running the @nofib@ benchmark suite.
nofibRules :: Rules ()
nofibRules = do
    root <- buildRootRules
    flags <- lookupNofibArgs

    -- a phony "nofib" rule that just triggers
    -- the rule below.
    let nofibLogFile = fromMaybe defNofibLogFile (logFile flags)
    "nofib" ~> need [root -/- nofibLogFile]

    -- a rule to produce <build root>/<nofib-logFile>
    -- by running the nofib suite and capturing
    -- the relevant output.
    root -/- nofibLogFile %> \fp -> do
        putVerbose (show flags)
        needNofibDeps

        makePath <- builderPath (Make "nofib")
        top      <- topDirectory
        ghcPath  <- builderPath (Ghc CompileHs Stage2)

        -- some makefiles in nofib rely on a $MAKE
        -- env var being defined
        liftIO (setEnv "MAKE" makePath)

        -- this runs make commands in the nofib
        -- subdirectory, passing the path to
        -- the GHC to benchmark.
        let bootArgs = mkNofibBootArgs (top -/- ghcPath) flags
        let runArgs  = mkNofibRunArgs flags
        unit $ cmd (Cwd "nofib") [makePath] ["clean"]
        unit $ cmd (Cwd "nofib") [makePath] (bootArgs ++ ["boot"])
        (Exit e, Stdouterr log) <- cmd (Cwd "nofib") [makePath] (bootArgs ++ runArgs)
        writeFile' fp log
        if e == ExitSuccess
            then putLoud $ "nofib log available at " ++ fp
            else error $ "nofib failed, full log available at " ++ fp

-- | Build the dependencies required by @nofib@.
needNofibDeps :: Action ()
needNofibDeps = do
    unlitPath <- programPath (vanillaContext Stage1 unlit)
    mtlPath   <- pkgConfFile (vanillaContext Stage1 mtl  )
    need [ unlitPath, mtlPath ]
    needBuilder (Ghc CompileHs Stage2)

mkNofibBootArgs :: FilePath -> NofibArgs -> [String]
mkNofibBootArgs ghcPath args =
    [ "WithNofibHc=" ++ ghcPath
    , "mode=" ++ (timeModeToStr (mode args))
    ]

mkNofibRunArgs :: NofibArgs -> [String]
mkNofibRunArgs args =
    [ if cachegrind then "EXTRA_RUNTEST_OPTS=-cachegrind" else ""
    , "NoFibRuns=" ++ (if cachegrind then "1" else (numOfRuns args))
    , "mode=" ++ (timeModeToStr (mode args))
    ] where cachegrind = cacheGrind args

timeModeToStr :: NofibMode -> String
timeModeToStr TestSlow   = "slow"
timeModeToStr TestNormal = "norm"
timeModeToStr TestFast   = "fast"
