module T22560_fail_d where

import Data.Kind

data T @k (a :: k)     -- No CUSK, no SAKS