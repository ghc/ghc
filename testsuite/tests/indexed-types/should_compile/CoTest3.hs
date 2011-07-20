{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- This test uses the PushC rule of the System FC operational semantics
-- Writen by Tom Schrijvers

module CoTest3 where 

data T a = K (a ~ Int => a -> Int) 


{-# INLINE[2] f #-}
f :: T s1 ~ T s2 => T s1 -> T s2
f x = x

{-# INLINE[3] test #-}
test :: T s1 ~ T s2 => (s1 ~ Int => s1 -> Int) -> (s2 ~ Int => s2 -> Int) 
test g = case f (K g) of
           K r -> r
e :: s ~ Int => s -> s -> Int
e _ s = s

final :: s1 ~ s2 => s1 -> (s2 ~ Int => s2 -> Int)
final x = test (e x)
