module Rules.Wrappers.GhcPkg (ghcPkgWrapper) where

import Base
import Expression
import Oracles

-- Note about wrapper:
-- bindir is usually GhcSourcePath / inplace / bin
-- topdir is usually GhcSourcePath / inplace / lib
-- datadir is usually the same as topdir

ghcPkgWrapper :: FilePath -> Expr String
ghcPkgWrapper program = do
    lift $ need [sourcePath -/- "Rules/Wrappers/GhcPkg.hs"]
    top <- getSetting GhcSourcePath
    let pkgConf = top -/- "inplace" -/- "lib" -/- "package.conf.d"
    return $ unlines
        [ "#!/bin/bash"
        , "exec " ++ (top -/- program)
          ++ " --global-package-db " ++ pkgConf ++ " ${1+\"$@\"}" ]
