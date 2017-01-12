module Settings.SourceArgs (SourceArgs (..), sourceArgs) where

import GHC
import Predicate

-- TODO: Move C source arguments here
data SourceArgs = SourceArgs
    { hsDefault  :: Args
    , hsLibrary  :: Args
    , hsCompiler :: Args
    , hsGhc      :: Args }

sourceArgs :: SourceArgs -> Args
sourceArgs SourceArgs {..} = do
    hsCompile <- builder $ Ghc CompileHs
    hsLink    <- builder $ Ghc LinkHs
    pkg       <- getPackage
    mconcat [ (hsCompile || hsLink) ?                    hsDefault
            ,  hsCompile            ? isLibrary pkg    ? hsLibrary
            ,  hsCompile            ? package compiler ? hsCompiler
            , (hsCompile || hsLink) ? package ghc      ? hsGhc ]
