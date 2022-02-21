{-# OPTIONS_GHC -O2 -fforce-recomp #-}
{-# LANGUAGE TypeFamilies #-}

-- | Serves as a unit test for isRecDataCon.
-- See Note [CPR for recursive data constructors] for similar examples.
module RecDataConCPR where

import Control.Monad.Trans.State
import Control.Monad.ST
import Data.Char

import {-# SOURCE #-} RecDataConCPRa

replicateOne :: Int -> [Int]
replicateOne 1 = [1]
replicateOne n = 1 : replicateOne (n-1)

data T = T (Int, (Bool, Char)) -- NonRec
t :: Char -> Bool -> Int -> T
t a b c = T (c, (b, a))

data U = U [Int] -- NonRec

u :: Int -> U
u x = U (replicate x 1000)

data U2 = U2 [U2] -- Rec

u2 :: Int -> U2
u2 x = U2 (replicate 1000 (u2 (x-1)))

data R0 = R0 R1 | R0End Int -- Rec, but out of fuel (and thus considered NonRec)
data R1 = R1 R2
data R2 = R2 R3
data R3 = R3 R4
data R4 = R4 R5
data R5 = R5 R6
data R6 = R6 R7
data R7 = R7 R8
data R8 = R8 R9
data R9 = R9 R0

r :: Bool -> Int -> R0
r False x = r True x
r True  x = R0 (R1 (R2 (R3 (R4 (R5 (R6 (R7 (R8 (R9 (R0End x))))))))))

data R20 = R20 R21 | R20End Int -- Rec
data R21 = R21 R20

r2 :: Bool -> Int -> R20
r2 False x = r2 True x
r2 True  x = R20 (R21 (R20End 4))

newtype Fix f = Fix (f (Fix f)) -- Rec

fixx :: Int -> Fix Maybe
fixx 0 = Fix Nothing
fixx n = Fix (Just (fixx (n-1)))

data N = N (Fix (Either Int)) -- NonRec
data M = M (Fix (Either M)) -- Rec

n :: Int -> N
n = N . go
  where
    go 0 = Fix (Left 42)
    go n = Fix (Right (go (n-1)))

m :: Int -> M
m = M . go
  where
    go 0 = Fix (Left (m 42))
    go n = Fix (Right (go (n-1)))

data F = F (F -> Int) -- NonRec
f :: Int -> F
f n = F (const n)

data G = G (Int -> G) -- NonRec
g :: Int -> G
g n = G (\m -> g (n+m))

newtype MyM s a = MyM (StateT Int (ST s) a) -- NonRec
myM :: Int -> MyM s Int
myM 0 = MyM $ pure 42
myM n = myM (n-1)

type S = (Int, Bool) -- NonRec
s :: Int -> S
s n = (n, True)

type family E a
type instance E Int = Char
type instance E (a,b) = (E a, E b)
type instance E Char = Blub
data Blah = Blah (E (Int, (Int, Int))) -- NonRec
data Blub = Blub (E (Char, Int))       -- Rec
data Blub2 = Blub2 (E (Bool, Int))     -- Unsure, because stuck

blah :: Int -> Blah
blah n = Blah (chr n, (chr (n+1), chr (n+2)))

blub :: Int -> Blub
blub n = Blub (blub (n-1), chr n)

blub2 :: Int -> Blub2
blub2 n = Blub2 (undefined :: E Bool, chr n)

-- Now for abstract TyCons, point (7) of the Note:
data BootNonRec1 = BootNonRec1 BootNonRec2 -- in RecDataConCPRa.hs-boot
data BootRec1 = BootRec1 BootRec2 -- in RecDataConCPRa.hs-boot, recurses back

bootNonRec :: Int -> BootNonRec2 -> BootNonRec1 -- Unsure, thus like NonRec
bootNonRec x b2 = BootNonRec1 b2

bootRec :: Int -> BootRec2 -> BootRec1 -- Unsure, thus like NonRec
bootRec x b2 = BootRec1 b2
