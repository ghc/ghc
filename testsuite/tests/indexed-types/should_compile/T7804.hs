{-# LANGUAGE TypeFamilies, RankNTypes #-}

module T7804 where

type family F f a

data Proxy a = P

sDFMap :: (forall a. Proxy f -> Proxy a -> Proxy (F f a)) -> Int
sDFMap _ = 3


{-
flat cache
 [G] F f_aqh aqj ~ fsk_aqp
 [G] F f_aqg aqj ~ fsk_aqq

 [W] aqk : f_aqh[2] ~ f_aqg
 [w] aql : fsk_aqp ~ fsk_aqq

 [G] F f_agh a_aqj ~ F f_aqg
-}