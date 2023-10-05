{-# LANGUAGE TypeFamilies #-}
module T10808 where

type family F a
type family G a

data T1
type instance F T1 = Char
type instance G T1 = Int

data T2
type instance F T2 = Bool
type instance G T2 = Int

data R a = R { x :: F a, y :: G a }

r1 :: R T1
r1 = R { x = 'a', y = 2 }

r2 :: R T2
r2 = r1 { x = True }  -- error: Cannot match T1 with T2

r3 :: R T2
r3 = r1 { x = True, y = y r1 } -- OK
