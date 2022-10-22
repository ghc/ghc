module GHC.Core.FamInstEnv where

import GHC.Core.Coercion.Axiom (CoAxiom, BranchIndex, Branched)
import GHC.Core.TyCo.Rep (Coercion)
import GHC.Core.Type (Type)
import Data.Maybe (Maybe)

chooseBranch :: CoAxiom Branched -> [Type]
             -> Maybe (BranchIndex, [Type], [Coercion])
