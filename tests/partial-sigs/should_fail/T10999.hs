module T10999 where

import qualified Data.Set as Set

f :: _ => () -> _
f _ = Set.fromList undefined

g = map fst $ Set.toList $ f ()
