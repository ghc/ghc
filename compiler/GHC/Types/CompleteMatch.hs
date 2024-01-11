{-# LANGUAGE TypeApplications #-}

-- | COMPLETE signature
module GHC.Types.CompleteMatch where

import GHC.Prelude
import GHC.Core.TyCo.Rep
import GHC.Types.Unique.DSet
import GHC.Core.ConLike
import GHC.Core.TyCon
import GHC.Core.Type ( splitTyConApp_maybe )
import GHC.Utils.Outputable

-- | A list of conlikes which represents a complete pattern match.
-- These arise from @COMPLETE@ signatures.
-- See also Note [Implementation of COMPLETE pragmas].
data CompleteMatch = CompleteMatch
  { cmConLikes :: UniqDSet ConLike -- ^ The set of `ConLike` values
  , cmResultTyCon :: Maybe TyCon   -- ^ The optional, concrete result TyCon the set applies to
  }

vanillaCompleteMatch :: UniqDSet ConLike -> CompleteMatch
vanillaCompleteMatch cls = CompleteMatch { cmConLikes = cls, cmResultTyCon = Nothing }

instance Outputable CompleteMatch where
  ppr (CompleteMatch cls mty) = case mty of
    Nothing -> ppr cls
    Just ty -> ppr cls <> text "@" <> parens (ppr ty)

type CompleteMatches = [CompleteMatch]

completeMatchAppliesAtType :: Type -> CompleteMatch -> Bool
completeMatchAppliesAtType ty cm = all @Maybe ty_matches (cmResultTyCon cm)
  where
    ty_matches sig_tc
      | Just (tc, _arg_tys) <- splitTyConApp_maybe ty
      , tc == sig_tc
      || sig_tc `is_family_ty_con_of` tc
         -- #24326: sig_tc might be the data Family TyCon of the representation
         --         TyCon tc -- this CompleteMatch still applies
      = True
      | otherwise
      = False
    fam_tc `is_family_ty_con_of` repr_tc =
      (fst <$> tyConFamInst_maybe repr_tc) == Just fam_tc
