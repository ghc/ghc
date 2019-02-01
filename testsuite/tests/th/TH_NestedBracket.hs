{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module TH_NestedBracket where

import Language.Haskell.TH

import TH_NestedBracket_Lib

n1 :: Bool
n1 = $(normalLift True)

n2 :: Bool
n2 = $($(liftTwice True))

a :: Bool
a = $(foo True)

b :: ExpQ
b = $(foo2 True)

c :: Bool
c = $($(foo2 True))

d :: Bool
d = $($(foo3 True))

e :: Bool
e = $($(foo4) True)

f :: Bool
f = $($($(foo5 True)))

g :: (Bool, ExpQ)
g = $(mixed_levels True)

-- Desugared Variants

aD :: Bool
aD = $(fooD True)

bD :: ExpQ
bD = $(foo2D True)

cD :: Bool
cD = $($(foo2D True))

dD :: Bool
dD = $($(foo3D True))

eD :: Bool
eD = $($(foo4D) True)

fD :: Bool
fD = $($($(foo5D True)))

gD :: (Bool, ExpQ)
gD = $(mixed_levelsD True)



--c :: _
--c = $(foo3)
