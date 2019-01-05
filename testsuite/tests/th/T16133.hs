{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module T16133 where

import Data.Kind
import Language.Haskell.TH hiding (Type)

data P (a :: k) = MkP

$([d| f :: Int
      f = $(varE 'id `appTypeE` conT ''Int `appE` litE (integerL 42))

      type P' = $(conT ''P `appKindT` conT ''Type) |])
