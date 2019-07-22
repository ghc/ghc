module IncompleteMatches (
        IncompleteMatches, initIM, markMatchedIM, unmatchedConLikesIM
    ) where

import GhcPrelude

import UniqSet
import Outputable
import TyCon
import ConLike
import HscTypes
import Type
import DsMonad

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

type ConLikeSet = UniqSet ConLike -- TODO: UniqDSet?

newtype IncompleteMatches = IM (NonEmpty ConLikeSet)

instance Outputable IncompleteMatches where
  ppr (IM cs) = ppr (NonEmpty.toList cs)

initIM :: Type -> DsM (Maybe IncompleteMatches)
initIM ty = case splitTyConApp_maybe ty of
  Nothing -> pure Nothing
  Just (tc, _) -> do
    let mb_rdcs = map RealDataCon <$> tyConDataCons_maybe tc
    let maybe_to_list = maybe [] (:[])
    let rdcs = maybe_to_list mb_rdcs
    pragmas <- dsGetCompleteMatches tc
    let fams = mapM dsLookupConLike . completeMatchConLikes
    pscs <- mapM fams pragmas
    pure (IM . fmap mkUniqSet <$> NonEmpty.nonEmpty (rdcs ++ pscs))
{-
    pure (mapMaybe figure_out_premise candidates)
    -- Find any COMPLETE sets that only apply to specific instantiations of 'ty'
    -- Think of a COMPLETE set involving @Just42 :: Maybe Int@. This should have
    -- a premise of @Maybe Int@, which wouldn't match a 'ty' @Maybe a@.
    -- We don't filter them out right away, because we might later get to know
    -- that @a ~ Int@ and call @anyCompleteMatch@ on @Maybe Int@.
      where
        -- We don't properly type-check COMPLETE pragmas, so we might end up
        -- with sets that don't have a unifying type. We filter those out, hence
        -- the Maybe
        figure_out_premise :: [ConLike] -> Maybe ConLikeSet
        figure_out_premise cls = Just (undefined, mkUniqSet cls)
          where
            p (RealDataCon _) = True
            p (PatSynCon ps)  = isJust (tcMatchTy (projResTy (patSynSig ps)) ty)
            projResTy (_, _, _, _, _, res_ty) = res_ty
    -- See Note [Filtering out non-matching COMPLETE sets]
-}

markMatchedIM :: ConLike -> IncompleteMatches -> IncompleteMatches
markMatchedIM con (IM ms) = IM (fmap (`delOneFromUniqSet` con) ms)

-- | Returns 'Nothing' when one of the incomplete match sets has become empty
-- (thus the match has become complete) and 'Just' a non-empty list of arbitrary
-- unmatched 'ConLike's from every incomplete match otherwise.
unmatchedConLikesIM :: IncompleteMatches -> Maybe (NonEmpty ConLike)
unmatchedConLikesIM (IM ms) = traverse f ms
  where
    f cs = case nonDetEltsUniqSet cs of
      -- TODO: Figure out why nonDetEltsUniqSet is OK here. I guess it is, as
      -- long as we don't use the sets for warning messages, which eventually
      -- we probably will...
      []   -> Nothing
      cl:_ -> Just cl
