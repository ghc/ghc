module Rules.Wrappers.Runhaskell (runhaskellWrapper) where

import Base
import Expression
import Oracles.Path

runhaskellWrapper :: FilePath -> Expr String
runhaskellWrapper program = do
    lift $ need [sourcePath -/- "Rules/Wrappers/Runhaskell.hs"]
    top <- getTopDirectory
    return $ unlines
        [ "#!/bin/bash"
        , "exec " ++ (top -/- program)
          ++ " -f" ++ (top -/- "inplace/lib/bin/ghc-stage2") -- HACK
          ++ " -B" ++ (top -/- "inplace/lib") ++ " ${1+\"$@\"}" ]
