module Rules.Wrappers.RunGhc (runGhcWrapper) where

import Base
import Expression
import Oracles.Path

runGhcWrapper :: FilePath -> Expr String
runGhcWrapper program = do
    lift $ need [sourcePath -/- "Rules/Wrappers/RunGhc.hs"]
    top <- getTopDirectory
    return $ unlines
        [ "#!/bin/bash"
        , "exec " ++ (top -/- program)
          ++ " -f" ++ (top -/- "inplace/lib/bin/ghc-stage2") -- HACK
          ++ " -B" ++ (top -/- "inplace/lib") ++ " ${1+\"$@\"}" ]
