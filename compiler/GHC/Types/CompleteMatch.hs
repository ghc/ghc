-- | COMPLETE signature
module GHC.Types.CompleteMatch
   ( CompleteMatch
   , CompleteMatches
   )
where

import GHC.Types.Unique.DSet
import GHC.Core.ConLike

-- | A list of conlikes which represents a complete pattern match.
-- These arise from @COMPLETE@ signatures.
-- See also Note [Implementation of COMPLETE pragmas].
type CompleteMatch = UniqDSet ConLike

type CompleteMatches = [CompleteMatch]

