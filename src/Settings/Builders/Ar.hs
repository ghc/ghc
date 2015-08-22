module Settings.Builders.Ar (arArgs, arPersistentArgsCount) where

import Builder
import Expression
import Predicates (builder)

arArgs :: Args
arArgs = builder Ar ? do
    file <- getFile
    objs <- getSources
    mconcat [ arg "q"
            , arg file
            , append objs ]

-- This count includes arg "q" and arg file parameters in arArgs (see above).
-- Update this value appropriately when changing arArgs.
arPersistentArgsCount :: Int
arPersistentArgsCount = 2
