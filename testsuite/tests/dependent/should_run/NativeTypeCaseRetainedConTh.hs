{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes, TemplateHaskell #-}
module NativeTypeCaseRetainedConTh where

import qualified NativeTypeCaseRetainedConThProvider as P
import GHC.Types (Multiplicity(Many, One))
import Language.Haskell.TH

$(do
  original <- reify ''P.Stored
  let dataName = mkName "Copied"
      identify "Stored"      = mkName "Copied"
      identify "Vacuous"     = mkName "CopiedVacuous"
      identify "PlainStored" = mkName "CopiedPlain"
      identify unexpected = error unexpected
      renameTy (AppT fn arg) = AppT (renameTy fn) arg
      renameTy (ConT name) | name == ''P.Stored = ConT dataName
      renameTy other = other
      convert (ForallMatchC anno con) = ForallMatchC anno <$> convert con
      convert (ForallC bs ctx con) = ForallC bs ctx <$> convert con
      convert (GadtC names args result) =
        pure (GadtC (map (identify . nameBase) names) args (renameTy result))
      convert (RetainedGadtC [name] args result) =
        let valid = case (nameBase name, args) of
              ("Stored", [GadtValueArg (Bang _ SourceLazy) (PromotedT m1) _,
                          GadtIndexArg outer _,
                          GadtValueArg _ (PromotedT m2) (AppT _ (IndexVarT outerUse)),
                          GadtIndexArg inner _,
                          GadtValueArg _ (PromotedT m3) (AppT _ (IndexVarT innerUse)),
                          GadtValueArg _ _ _]) ->
                outer /= inner && outer == outerUse && inner == innerUse &&
                (m1, m2, m3) == ('One, 'Many, 'One)
              ("Vacuous", [GadtIndexArg _ _, GadtValueArg _ _ _]) -> True
              _ -> False
        in if valid
           then pure (RetainedGadtC [identify (nameBase name)] args (renameTy result))
           else fail (show args)
      convert con = fail (show con)
  case original of
    TyConI (DataD ctx _ args kind cons derivs) -> do
      copies <- traverse convert cons
      pure [DataD ctx dataName args kind copies derivs]
    _ -> fail (show original))

$([d|
  data Quoted a where
    Quoted :: forall a. ~Int %1 -> foreach (b :: Bool) -> P.Select b %Many ->
              foreach (b :: Bool) -> P.Select b %1 -> a %1 -> Quoted a
    QuotedVacuous :: forall a. foreach (b :: Bool) -> a %1 -> Quoted a
    QuotedPlain :: a %1 -> Quoted a
  |])
