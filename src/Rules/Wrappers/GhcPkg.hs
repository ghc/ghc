module Rules.Wrappers.GhcPkg (ghcPkgWrapper) where

import Base
import Expression
import Settings

ghcPkgWrapper :: FilePath -> Expr String
ghcPkgWrapper program = do
    lift $ need [sourcePath -/- "Rules/Wrappers/GhcPkg.hs"]
    top   <- getTopDirectory
    stage <- getStage
    -- Use the package configuration for the next stage in the wrapper.
    -- The wrapper is generated in StageN, but used in StageN+1.
    let packageDb = top -/- packageDbDirectory (succ stage)
    return $ unlines
        [ "#!/bin/bash"
        , "exec " ++ (top -/- program)
          ++ " --global-package-db " ++ packageDb ++ " ${1+\"$@\"}" ]
