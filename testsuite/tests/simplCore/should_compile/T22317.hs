{-# LANGUAGE BangPatterns #-}

module T22317 where

data T = T (Maybe Bool) (Maybe Bool) (Maybe Bool) (Maybe Bool)


m :: Maybe a -> Maybe a -> Maybe a
-- Don't make this INLINE; if you do, ,it's not unreasonable to inline it
m (Just v1) Nothing = Just v1
m _         mb      = mb

f :: T -> T -> T
f (T a1 b1 c1 d1) (T a2 b2 c2 d2)
  = let j1 !a = let j2 !b = let j3 !c = let j4 !d = T a b c d
                                        in j4 (m d1 d2)
                            in j3 (m c1 c2)
                in j2 (m b1 b2)
    in j1 (m a1 a2)
{-# OPAQUE f #-}
