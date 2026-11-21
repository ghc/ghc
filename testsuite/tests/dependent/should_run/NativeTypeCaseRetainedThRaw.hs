{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, TemplateHaskell, TypeApplications #-}
module NativeTypeCaseRetainedThRaw
  (chooseInline, chooseDirect, renderDirect, observeInline, caseData
  , chooseQuoted, renderQuoted, QuotedPick, chooseNested, SpliceFlag(..)
  , nestedCases, chooseNestedRetained, outerCapturedQuote, outerCapturedExact
  , retainedCapturedExact, retainedCapturedQuote) where

import Data.Proxy (Proxy(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer, dataToExpQ, dataToPatQ, lift, liftTyped)

$(do
  ty <- [t| foreach (b :: Bool) -> case b of { True -> Int; False -> Char } |]
  rendered <- [t| foreach (b :: Bool) -> (case b of { True -> Int; False -> Char }) -> String |]
  scoped <- [t| foreach (b :: Maybe Bool) ->
                 case b of { Just b -> Proxy b; _ -> Proxy False } |]
  case (ty, scoped) of
    ( ForEachT BndrReq b _
        (CaseT (IndexVarT bUse)
          [TypeCaseAlt (PromotedT t) (ConT i), TypeCaseAlt (PromotedT f) (ConT c)])
      , ForEachT BndrReq outer _
        (CaseT (IndexVarT outerUse)
          [ TypeCaseAlt (AppT (PromotedT just) (VarT inner)) (AppT (ConT proxy) (VarT innerUse))
          , TypeCaseAlt WildCardT _
          ])
      )
      | b == bUse, t == 'True, f == 'False, i == ''Int, c == ''Char
      , outer == outerUse, outer /= inner, inner == innerUse
      , just == 'Just, proxy == ''Proxy
      -> pure [SigD (mkName "chooseInline") ty, SigD (mkName "renderInline") rendered]
    _ -> fail (show (ty, scoped)))

chooseInline True = 13
chooseInline False = 'q'

renderInline True i = show i
renderInline False c = [c]

observeInline :: foreach (b :: Bool) -> String
observeInline b = renderInline b (chooseInline b)

before :: $(runIO (appendFile "case-effects" "before\n") >> [t| Int |])
before = 19

chooseDirect :: $(do
  runIO (appendFile "case-effects" "case\n")
  addModFinalizer (runIO (appendFile "case-effects" "finalizer\n"))
  [t| foreach (b :: Bool) -> () -> case b of { True -> Int; False -> Char } |])
chooseDirect True () = before
chooseDirect False () = after

renderDirect :: $(do
  runIO (appendFile "case-effects" "render\n")
  [t| foreach (flag :: Bool) -> (case flag of { True -> Int; False -> Char }) -> String |])
renderDirect True i = show i
renderDirect False c = [c]

after :: $(runIO (appendFile "case-effects" "after\n") >> [t| Char |])
after = 'c'

caseData :: Bool
caseData =
  case $(dataToExpQ (const Nothing) (CaseT StarT [TypeCaseAlt WildCardT ConstraintT])) of
    $(dataToPatQ (const Nothing) (CaseT StarT [TypeCaseAlt WildCardT ConstraintT])) -> lifted == CaseT StarT [expected, TypeCaseAlt StarT WildCardT]
    _ -> False
  where
    lifted = $(lift (CaseT StarT [TypeCaseAlt WildCardT ConstraintT, TypeCaseAlt StarT WildCardT]))
    expected = $$(liftTyped (TypeCaseAlt WildCardT ConstraintT))

$(do
  ds <- [d|
    type QuotedPick (d :: Maybe Bool) =
      case d of { Just d -> Proxy d; Nothing -> Proxy False }

    chooseQuoted :: foreach (b :: Bool) -> case b of { True -> Int; False -> Char }
    chooseQuoted True = 23
    chooseQuoted False = 'd'

    renderQuoted :: foreach (b :: Bool) -> (case b of { True -> Int; False -> Char }) -> String
    renderQuoted True i = show i
    renderQuoted False c = [c] |]
  case ( [t | SigD name t <- ds, nameBase name == "chooseQuoted"]
       , [(bs, rhs) | TySynD name bs rhs <- ds, nameBase name == "QuotedPick"]
       ) of
    ( [ForEachT BndrReq b _ (CaseT (IndexVarT bUse) _)]
      , [ ([KindedTV outer _ _], CaseT (VarT outerUse)
            (TypeCaseAlt (AppT (PromotedT just) (VarT inner))
                         (AppT (ConT proxy) (VarT innerUse)) : _))
        ]
      )
      | b == bUse, outer == outerUse, outer /= inner, inner == innerUse
      , just == 'Just, proxy == ''Proxy -> pure ds
    _ -> fail (pprint ds))

data SpliceFlag (d :: Maybe Bool) where
  SpliceOn :: SpliceFlag ('Just 'True)
  SpliceOff :: SpliceFlag 'Nothing

chooseNested :: () -> $([t| forall (d :: Maybe Bool). SpliceFlag d ->
                            case d of { Just d -> Proxy d; Nothing -> Int } |])
chooseNested () SpliceOn = Proxy
chooseNested () SpliceOff = 29

nestedCases :: () ->
  ($(do runIO (appendFile "nested-effects" "first\n")
        addModFinalizer (runIO (appendFile "nested-effects" "finalizer\n"))
        [t| case True of { True -> Int; False -> Char } |]),
   $(runIO (appendFile "nested-effects" "second\n") >>
     [t| case False of { True -> Int; False -> Char } |]),
   case True of { True -> Bool; False -> String })
nestedCases () = (31, 'n', True)

chooseNestedRetained :: foreach (b :: Bool) -> $([t| foreach (b :: Bool) ->
  case b of { True -> Int; False -> Char } |])
chooseNestedRetained _ = \case
  True -> 43
  False -> 's'

type SpliceDomain = Maybe Bool

outerCapturedQuote :: forall (d :: SpliceDomain). SpliceFlag d ->
  $([t| case d of { Just d -> Proxy d; Nothing -> Int } |])
outerCapturedQuote SpliceOn = Proxy
outerCapturedQuote SpliceOff = 47

outerCapturedExact :: forall (d :: SpliceDomain). SpliceFlag d ->
  $(do
    Just outer <- lookupTypeName "d"
    shadow <- newName "d"
    row <- newName "d"
    let arg = AppT (ConT ''Proxy) (VarT shadow)
        result = CaseT (VarT outer)
          [ TypeCaseAlt (AppT (PromotedT 'Just) (VarT row)) (AppT (ConT ''Proxy) (VarT row))
          , TypeCaseAlt (PromotedT 'Nothing) (ConT ''Int)
          ]
    pure (ForallT [KindedTV shadow SpecifiedSpec (ConT ''Bool)] []
      (AppT (AppT ArrowT arg) result)))
outerCapturedExact SpliceOn _ = Proxy
outerCapturedExact SpliceOff _ = 53

retainedCapturedExact :: foreach (flag :: Bool) ->
  $(do
    Just outer <- lookupValueName "flag"
    inner <- newName "flag"
    pure (ForEachT BndrReq inner (ConT ''Bool) (CaseT (IndexVarT outer)
      [ TypeCaseAlt (PromotedT 'True) (ConT ''Int)
      , TypeCaseAlt (PromotedT 'False) (ConT ''Char)
      ])))
retainedCapturedExact True = \_ -> 59
retainedCapturedExact False = \_ -> 's'

retainedCapturedQuote :: foreach (m :: Maybe Bool) ->
  $([t| case m of { Just m -> Proxy m; Nothing -> Int } |])
retainedCapturedQuote (Just m) = Proxy @m
retainedCapturedQuote Nothing = 61
