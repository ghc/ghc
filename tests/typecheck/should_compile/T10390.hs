{-# LANGUAGE RankNTypes #-}

module T10390 where

class ApPair r where
  apPair :: (forall a . (ApPair a, Num a) => Maybe a) -> Maybe r

instance (ApPair a, ApPair b) => ApPair (a,b) where
  apPair x = apPair' x

apPair' :: (ApPair b, ApPair c)
        => (forall a . (Num a, ApPair a) => Maybe a) -> Maybe (b,c)
            -- NB constraints in a different order to apPair
apPair' f =  let (Just a) = apPair f
                 (Just b) = apPair f
          in Just $ (a, b)
