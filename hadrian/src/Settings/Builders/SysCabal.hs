module Settings.Builders.SysCabal (sysCabalBuilderArgs) where

import Settings.Builders.Common
import Packages

sysCabalBuilderArgs :: Args
sysCabalBuilderArgs = mconcat
    [ builder (SysCabal "groups")? do
        verbosity <- expr getVerbosity
        stage <- getStage
        --top <- expr topDirectory
        pkgDb <- expr $ packageDbPath stage
        ghcPath <- expr $ builderPath (Ghc CompileHs stage)
        mconcat [ arg "install"
                , arg =<< ((++ "/") <$> getInput)
                , arg "--with-compiler"
                , arg ghcPath
                , arg "--package-db"
                , arg pkgDb
                , arg "--ipid"
                , arg "$pkg-$version"

                , verbosity < Chatty ? arg "-v0" ] ]
