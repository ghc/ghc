module T24470a where

import Data.Data
import Data.Kind

type SynBad :: forall k. k -> Type
type SynBad = Proxy :: j -> Type
