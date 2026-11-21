{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module NativeTypeCaseRetainedThRaw (chooseInline) where

import Data.Proxy (Proxy)
import Language.Haskell.TH

$(do
  ty <- [t| foreach (b :: Bool) -> case b of { True -> Int; False -> Char } |]
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
      -> pure [SigD (mkName "chooseInline") ty]
    _ -> fail (show (ty, scoped)))

chooseInline True = 13
chooseInline False = 'q'
