{-# LANGUAGE TypeApplications, ScopedTypeVariables, PolyKinds,
             TypeFamilies, RankNTypes,
             FlexibleContexts #-}
-- tests about visible type application

module Vta1 where

import Data.Kind (Type)

quad :: a -> b -> c -> d -> (a, b, c, d)
quad = (,,,)

silly = quad @_ @Bool @Char @_ 5 True 'a' "Hello"

pairup_nosig x y = (x, y)

pairup_sig :: a -> b -> (a,b)
pairup_sig u w = (u, w)

answer_sig = pairup_sig @Bool @Int False 7 --
-- (False, 7) :: (Bool, Int)

answer_read = show (read @Int "3") -- "3" :: String
answer_show = show @Integer (read "5") -- "5" :: String
answer_showread = show @Int (read @Int "7") -- "7" :: String

intcons a = (:) @Int a

intpair x y = pairup_sig @Int x y

answer_pairup = pairup_sig @Int 5 True -- (5, True) :: (Int, Bool)
answer_intpair = intpair 1 "hello" -- (1, "hello") :: (Int, String)
answer_intcons = intcons 7 []      -- [7] :: [Int]

type family F a
type instance F Char = Bool

g :: F a -> a
g _ = undefined

f :: Char
f = g True

answer = g @Char False

mapSame :: forall b. (forall a. a -> a) -> [b] -> [b]
mapSame _ [] = []
mapSame fun (x:xs) = fun @b x : (mapSame @b fun xs)

pair :: forall a. a-> (forall b. b -> (a, b))
pair x y = (x, y)

b = pair @Int 3 @Bool True
c = mapSame id [1,2,3]
d = pair 3 @Bool True

pairnum :: forall a. Num a => forall b. b -> (a, b)
pairnum = pair 3

e = (pair 3 :: forall a. Num a => forall b. b -> (a, b)) @Int @Bool True
h = pairnum @Int @Bool True

data First (a :: Type -> Type) = F
data Proxy (a :: k) = P -- This expands to P (kind variable) (type variable)
data Three (a :: Type -> k -> Type) = T

foo :: Proxy a -> Int
foo _ = 0

first :: First a -> Int
first _ = 0

fTest = first F
fMaybe = first @Maybe F

test = foo P
bar = foo @Bool P -- should work

too :: Three a -> Int
too _ = 3

threeBase = too T
threeOk = too @Either T

blah = Nothing @Int

newtype N = MkN { unMkN :: forall a. Show a => a -> String }

n = MkN show

boo = unMkN n @Bool

boo2 :: forall (a :: Type -> Type) . Proxy a -> Bool
boo2 _ = False

base = boo2 P
bar'= boo2 @Maybe P -- should work
