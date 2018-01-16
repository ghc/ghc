{-# LANGUAGE RoleAnnotations #-}

module T10698 where
import Data.Coerce

data Map k a  = Map k a
type role Map nominal representational

map1 :: (k1->k2) -> Map k1 a -> Map k2 a
map1 f (Map a b) = Map (f a) b
{-# NOINLINE  [1] map1 #-}
{-# RULES
"map1/coerce" map1 coerce = coerce
 #-}


map2 :: (a -> b) -> Map k a -> Map k b
map2 f (Map a b) = Map a (f b)
{-# NOINLINE [1] map2 #-}

{-# RULES
"map2/coerce" map2 coerce = coerce
 #-}
