module T22560_fail_c where

import Data.Kind

type Dup :: forall k j. k -> Type
data Dup @k @k (a :: k)