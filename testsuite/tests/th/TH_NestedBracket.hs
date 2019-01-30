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

--c :: _
--c = $(foo3)
