{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeTypeCaseForwardKind where

import Data.Proxy (Proxy)

type Bad (b :: Bool) =
  let result :: Maybe payload
      result = case b of
        'True -> 'Just 'True
        'False -> 'Nothing
      payload = Bool
  in Proxy result
