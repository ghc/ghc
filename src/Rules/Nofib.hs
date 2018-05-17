module Rules.Nofib where

import Base
import Expression
import GHC
import Oracles.Setting
import Target

import System.Environment
import System.Exit

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
    top <- topDirectory
    ghcPath <- builderPath (Ghc CompileHs Stage2)
    perlPath <- builderPath Perl

    -- some makefiles in nofib rely on a $MAKE
    -- env var being defined
    liftIO (setEnv "MAKE" makePath)

    -- this runs make commands in the nofib
    -- subdirectory, passing the path to
    -- the GHC to benchmark and perl to
    -- nofib's makefiles.
    let nofibArgs = ["WithNofibHc=" ++ (top -/- ghcPath), "PERL=" ++ perlPath]
    unit $ cmd (Cwd "nofib") [makePath] ["clean"]
    unit $ cmd (Cwd "nofib") [makePath] (nofibArgs ++ ["boot"])
    (Exit e, Stdouterr log) <- cmd (Cwd "nofib") [makePath] nofibArgs
    writeFile' fp log
    if e == ExitSuccess
      then putLoud $ "nofib log available at " ++ fp
      else error $ "nofib failed, full log available at " ++ fp

nofibLogFile :: FilePath
nofibLogFile = "nofib-log"


-- the dependencies that nofib seems to require.
needNofibDeps :: Action ()
needNofibDeps = do
  unlitPath <- programPath (Context Stage1 unlit vanilla)
  mtlPath <- pkgConfFile (Context Stage1 mtl vanilla)
  need [ unlitPath, mtlPath ]
  needBuilder (Ghc CompileHs Stage2)
