module Settings.Optimisation (Optimisation (..), optimisationArgs) where

import GHC
import Predicate

-- TODO: Move C optimisation settings here
data Optimisation = Optimisation
    { hsDefault  :: Args
    , hsLibrary  :: Args
    , hsCompiler :: Args
    , hsGhc      :: Args }

optimisationArgs :: Optimisation -> Args
optimisationArgs Optimisation {..} = do
    hsCompile <- builder $ Ghc CompileHs
    hsLink    <- builder $ Ghc LinkHs
    pkg       <- getPackage
    mconcat [ (hsCompile || hsLink) ?                    hsDefault
            ,  hsCompile            ? isLibrary pkg    ? hsLibrary
            ,  hsCompile            ? package compiler ? hsCompiler
            , (hsCompile || hsLink) ? package ghc      ? hsGhc ]
