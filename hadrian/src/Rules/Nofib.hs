module Rules.Nofib where

import Base
import Expression
import Oracles.Setting
import Packages

import System.Environment
import System.Exit

nofibLogFile :: FilePath
nofibLogFile = "nofib-log"

-- | Rules for running the @nofib@ benchmark suite.
nofibRules :: Rules ()
nofibRules = do
    root <- buildRootRules

    -- a phony "nofib" rule that just triggers
    -- the rule below.
    "nofib" ~> need [root -/- nofibLogFile]

    -- a rule to produce <build root>/nofig-log
    -- by running the nofib suite and capturing
    -- the relevant output.
    root -/- nofibLogFile %> \fp -> do
        needNofibDeps

        makePath <- builderPath (Make "nofib")
        top      <- topDirectory
        ghcPath  <- builderPath (Ghc CompileHs Stage2)

        -- some makefiles in nofib rely on a $MAKE
        -- env var being defined
        liftIO (setEnv "MAKE" makePath)

        -- this runs make commands in the nofib
        -- subdirectory, passing the path to
        -- the GHC to benchmark and perl to
        -- nofib's makefiles.
        let nofibArgs = ["WithNofibHc=" ++ (top -/- ghcPath)]
        unit $ cmd (Cwd "nofib") [makePath] ["clean"]
        unit $ cmd (Cwd "nofib") [makePath] (nofibArgs ++ ["boot"])
        (Exit e, Stdouterr log) <- cmd (Cwd "nofib") [makePath] nofibArgs
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
