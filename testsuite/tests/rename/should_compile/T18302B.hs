-- | Check that TYPE and (->) are re-exportable.
module T18302B where

import T18302A

type T = TYPE
type F = (->)

