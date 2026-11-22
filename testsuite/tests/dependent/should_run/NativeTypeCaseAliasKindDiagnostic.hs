{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeTypeCaseAliasKindDiagnostic where

import Data.Proxy (Proxy)

type UserPayload = Ordering

type BadResult (b :: Bool) =
  let payload = UserPayload
      chosen = payload
      result :: Maybe chosen
      result = case b of
        'True -> 'Just 'True
        'False -> 'Nothing
  in Proxy result

type BadOtherBranch (b :: Bool) =
  let payload = Ordering
      result :: Maybe payload
      result = case b of
        'True -> 'Just 'LT
        'False -> Maybe 'True
  in Proxy result
