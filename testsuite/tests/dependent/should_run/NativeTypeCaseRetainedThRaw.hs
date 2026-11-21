{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module NativeTypeCaseRetainedThRaw
  (chooseInline, chooseDirect, renderDirect, observeInline, caseData) where

import Data.Proxy (Proxy)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer, dataToExpQ, dataToPatQ)

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
    $(dataToPatQ (const Nothing) (CaseT StarT [TypeCaseAlt WildCardT ConstraintT])) -> True
    _ -> False
