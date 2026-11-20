{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TemplateHaskell #-}
{-# LANGUAGE GADTs, RankNTypes, TypeOperators #-}
module NativeTypeCaseReify where
import Data.Type.Equality ((:~:)(Refl))
import Data.Proxy (Proxy)
import Language.Haskell.TH
import NativeCase
import qualified NativeNestedCase as N

-- Reuse the private family occurrence returned by reify under its exact Name.
type Seen = $(do
  info <- reify ''F
  case info of
    TyConI (TySynD _ [_] (AppT (AppT family _) _)) ->
      pure (AppT (AppT family (PromotedT 'True)) (PromotedT 'True))
    _ -> fail (pprint info))

checked :: Seen :~: Int
checked = Refl

type SeenNested = $(do
  info <- reify ''N.Pick
  case info of
    TyConI (TySynD _ [_] (AppT (AppT family _) _)) ->
      let scrutinee = AppT (PromotedT 'Just) (AppT (PromotedT 'Just) (PromotedT 'True))
      in pure (AppT (AppT family scrutinee) scrutinee)
    _ -> fail (pprint info))

checkedNested :: SeenNested :~: Proxy 'True
checkedNested = Refl

type SeenArrow = $(do
  info <- reify ''N.ArrowResult
  case info of
    TyConI (TySynD _ [_, _] (AppT arrow (AppT (AppT (AppT family _) _) _)))
      | AppT (AppT (AppKindT (ConT fun) (PromotedT matchability))
                          (PromotedT multiplicity)) (TupleT 0) <- arrow
      , nameBase fun == "FUN"
      , nameBase matchability == "Unmatchable"
      , nameBase multiplicity == "Many" ->
          let scrutinee = AppT (PromotedT 'Just) (AppT (PromotedT 'Just) (PromotedT 'True))
              result = AppT (AppT (AppT family (PromotedT 'LT)) scrutinee) scrutinee
          in pure (AppT arrow result)
    _ -> fail (pprint info))

checkedArrow :: SeenArrow :~: (() -> (Proxy 'LT, Proxy 'True))
checkedArrow = Refl

type SeenForall = $(do
  info <- reify ''N.ForallResult
  case info of
    TyConI (TySynD _ [] scheme)
      | ForallMatchT (PromotedT tagMa) tagGroup <- scheme
      , ForallT [KindedTV _ SpecifiedSpec (ConT tagKind)] [] flagForall <- tagGroup
      , ForallMatchT (PromotedT flagMa) flagGroup <- flagForall
      , ForallT [KindedTV _ SpecifiedSpec (AppT (ConT maybeKind) (ConT flagKind))] [] _ <- flagGroup
      , tagKind == ''Ordering, maybeKind == ''Maybe, flagKind == ''Bool
      , nameBase tagMa == "Unmatchable", tagMa == flagMa -> pure scheme
    _ -> fail (pprint info))

checkedForall :: SeenForall
checkedForall tag N.CaseOn = (tag, 1)
checkedForall tag N.CaseOff = (tag, 0)
checkedForall tag N.CaseMissing = (tag, '?')
