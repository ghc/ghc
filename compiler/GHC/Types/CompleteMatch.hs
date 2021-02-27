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
data CompleteMatch = CompleteMatch
  { cmConLikes :: UniqDSet ConLike -- ^ The set of `ConLike` values
  , cmResultType :: Maybe Type -- ^ A type to be unified with the type of the scrutinee of a pattern match to determine if this pragma applies to the given match.
                               -- It should be a type which is possibly the type of result of each of the constructors, though at present, we don't check this.
  }

vanillaCompleteMatch :: UniqDSet ConLike -> CompleteMatch
vanillaCompleteMatch cls = CompleteMatch { cmConLikes = cls, cmResultType = Nothing }

instance Outputable CompleteMatch where
  ppr (CompleteMatch cls mty) = case mty of
    Nothing -> ppr cls
    Just ty -> ppr cls <> text "@" <> parens (ppr ty)

type CompleteMatches = [CompleteMatch]

completeMatchAppliesAtType :: Type -> CompleteMatch -> Bool
completeMatchAppliesAtType ty cm = all (isJust . (\t -> tcUnifyTyKi t ty)) (cmResultType cm)
 -- NB: We're using all (from Foldable) on a Maybe here.
