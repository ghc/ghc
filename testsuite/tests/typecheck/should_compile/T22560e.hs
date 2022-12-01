module T22560e where

import Data.Kind
import Data.Proxy

type X = forall k. k -> Type

type Y :: X
type Y @k (a :: k) = Proxy a