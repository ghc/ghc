{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TemplateHaskell #-}
{-# LANGUAGE GADTs, PolyKinds, RankNTypes, TypeOperators #-}
module NativeTypeCaseReify where
import Data.Type.Equality ((:~:)(Refl))
import Data.Proxy (Proxy(..))
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
      | ForallMatchT (PromotedT kindMa) kindGroup <- scheme
      , ForallT [KindedTV k SpecifiedSpec StarT] [] tagForall <- kindGroup
      , ForallMatchT (PromotedT tagMa) tagGroup <- tagForall
      , ForallT [KindedTV _ SpecifiedSpec (VarT tagKind)] [] flagForall <- tagGroup
      , ForallMatchT (PromotedT flagMa) flagGroup <- flagForall
      , ForallT [KindedTV _ SpecifiedSpec (AppT (ConT maybeKind) (ConT flagKind))] [] _ <- flagGroup
      , tagKind == k, maybeKind == ''Maybe, flagKind == ''Bool
      , nameBase kindMa == "Unmatchable", tagMa == kindMa, flagMa == kindMa -> pure scheme
    _ -> fail (pprint info))

checkedForall :: SeenForall
checkedForall tag N.CaseOn = (tag, 1)
checkedForall tag N.CaseOff = (tag, 0)
checkedForall tag N.CaseMissing = (tag, '?')

$(do
    ty <- reifyType 'caseIdentity
    let copy = mkName "reifiedIdentity"
    pure [SigD copy ty, ValD (VarP copy) (NormalB (VarE 'caseIdentity)) []])

checkedIdentity :: Bool
checkedIdentity = reifiedIdentity (Proxy :: Proxy 'False) True

$(do
    ty <- reifyType 'N.CaseField
    let copy = mkName "reifiedCaseField"
    pure [SigD copy ty, ValD (VarP copy) (NormalB (ConE 'N.CaseField)) []])

checkedCaseField :: N.CaseField ('Just 'True)
checkedCaseField = reifiedCaseField 42

$(do
    ty <- reifyType 'N.RankedCaseField
    let copy = mkName "reifiedRankedCaseField"
    pure [SigD copy ty, ValD (VarP copy) (NormalB (ConE 'N.RankedCaseField)) []])

checkedRankedCaseField :: N.RankedCaseField 'False
checkedRankedCaseField = reifiedRankedCaseField (\_ -> error "unused field")
