{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | COMPLETE signature
module GHC.Types.CompleteMatch
  ( CompleteMatchX(..)
  , CompleteMatch, CompleteMatches
  , DsCompleteMatch, DsCompleteMatches
  , mkCompleteMatch, vanillaCompleteMatch
  , completeMatchAppliesAtType
  ) where

import GHC.Prelude
import GHC.Core.TyCo.Rep
import GHC.Types.Unique
import GHC.Core.ConLike
import GHC.Core.TyCon
import GHC.Core.Type ( splitTyConApp_maybe )
import GHC.Types.Name ( Name )
import GHC.Types.Unique.DSet
import GHC.Utils.Outputable

type CompleteMatch   = CompleteMatchX Name
type DsCompleteMatch = CompleteMatchX ConLike

type CompleteMatches   = [CompleteMatch]
type DsCompleteMatches = [DsCompleteMatch]

-- | A list of conlikes which represents a complete pattern match.
-- These arise from @COMPLETE@ signatures.
-- See also Note [Implementation of COMPLETE pragmas].
data CompleteMatchX con = CompleteMatch
  { cmConLikes :: UniqDSet con  -- ^ The set of constructor names
  , cmResultTyCon :: Maybe Name -- ^ The optional, concrete result TyCon name the set applies to
  }
  deriving Eq

mkCompleteMatch :: UniqDSet con -> Maybe Name -> CompleteMatchX con
mkCompleteMatch nms mb_tc = CompleteMatch { cmConLikes = nms, cmResultTyCon = mb_tc }

vanillaCompleteMatch :: UniqDSet con -> CompleteMatchX con
vanillaCompleteMatch nms = mkCompleteMatch nms Nothing

instance Outputable con => Outputable (CompleteMatchX con) where
  ppr (CompleteMatch cls mty) = case mty of
    Nothing -> ppr cls
    Just ty -> ppr cls <> text "@" <> parens (ppr ty)

-- | Does this 'COMPLETE' set apply at this type?
--
-- See the part about "result type constructors" in
-- Note [Implementation of COMPLETE pragmas] in GHC.HsToCore.Pmc.Solver.
completeMatchAppliesAtType :: Type -> CompleteMatchX con -> Bool
completeMatchAppliesAtType ty cm = all @Maybe ty_matches (getUnique <$> cmResultTyCon cm)
  where
    ty_matches :: Unique -> Bool
    ty_matches sig_tc
      | Just (tc, _arg_tys) <- splitTyConApp_maybe ty
      , tc `hasKey` sig_tc
      || sig_tc `is_family_ty_con_of` tc
         -- #24326: sig_tc might be the data Family TyCon of the representation
         --         TyCon tc -- this CompleteMatch still applies
      = True
      | otherwise
      = False
    fam_tc `is_family_ty_con_of` repr_tc =
      case fst <$> tyConFamInst_maybe repr_tc of
        Just tc -> tc `hasKey` fam_tc
        Nothing -> False
