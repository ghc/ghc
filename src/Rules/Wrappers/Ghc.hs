module Rules.Wrappers.Ghc (ghcWrapper) where

import Base
import Expression
import Oracles.Path

ghcWrapper :: FilePath -> Expr String
ghcWrapper program = do
    lift $ need [sourcePath -/- "Rules/Wrappers/Ghc.hs"]
    top <- getTopDirectory
    return $ unlines
        [ "#!/bin/bash"
        , "exec " ++ (top -/- program)
          ++ " -B" ++ (top -/- "inplace/lib") ++ " ${1+\"$@\"}" ]
