{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes, ImpredicativeTypes #-}

module ICFP20 where

import Control.Monad.ST( ST, runST )

type SId = forall a. a->a

ids :: [SId]
ids = [id,id]

choose :: a -> a -> a
choose x y = x

auto :: SId -> SId
auto = id

single :: a -> [a]
single x = [x]

auto'2 :: SId -> b -> b
auto'2 x = id @SId x

auto3 :: SId -> b -> b
auto3 x = id x

poly :: SId -> (Int,Bool)
poly f = (f (3::Int), f True)

f :: (a->a) -> [a] -> a
f g xs = g (head xs)

inc :: Int -> Int
inc x = x+1

app :: (a->b) -> a -> b
app f x = f x

revapp :: a -> (a->b) -> b
revapp x f = f x

a3 = choose [] ids
a5 = id auto
a7 = choose id auto
a9 = f (choose id) ids
a10 = poly id
a11 = poly (\x -> x)
a12 = id poly (\x -> x)

c1 = length ids
c2 = tail ids
c3 = head ids
c4 = single id :: [SId]
c5 = id : ids
c6 = id : id : ids
c7 = single inc ++ single id
c8a = ids ++ single id
c8b = single id ++ ids
c9 = map poly (single id)

argST :: forall s. ST s Int
argST = error "urk"

d1a = poly $ id
d1b = app poly id
d2 = revapp id poly
d3 = runST argST
d4 = runST $ argST

st1 :: forall a. (forall s. ST s a) -> a
st1 x = id (runST @a) x

-- Not in the paper's table
c10 = head ids True
