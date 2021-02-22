-- | COMPLETE signature
module GHC.Types.CompleteMatch where

import GHC.Prelude
import GHC.Core.TyCo.Rep
import GHC.Types.Unique.DSet
import GHC.Core.ConLike
import GHC.Utils.Outputable
import GHC.Core.Unify

import Data.Maybe

-- | A list of conlikes which represents a complete pattern match.
-- These arise from @COMPLETE@ signatures.
-- See also Note [Implementation of COMPLETE pragmas].
data CompleteMatch = CompleteMatch { cmConLikes :: UniqDSet ConLike, cmScrutineeType :: Maybe Type }

vanillaCompleteMatch :: UniqDSet ConLike -> CompleteMatch
vanillaCompleteMatch cls = CompleteMatch { cmConLikes = cls, cmScrutineeType = Nothing }

instance Outputable CompleteMatch where
  ppr (CompleteMatch cls mty) = case mty of
    Nothing -> ppr cls
    Just ty -> ppr cls <> text "@" <> parens (ppr ty)

type CompleteMatches = [CompleteMatch]

completeMatchAppliesAtType :: Type -> CompleteMatch -> Bool
completeMatchAppliesAtType ty cm = all (isJust . (\t -> tcUnifyTyKi t ty)) (cmScrutineeType cm)
 -- NB: We're using all (from Foldable) on a Maybe here.
