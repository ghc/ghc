module Main where

data AList a = ANil | ACons a (AList a)

listtoalist []     = ANil
listtoalist (x:xs) = ACons x (listtoalist xs)

alisttolist ANil         = []
alisttolist (ACons a as) = (a : alisttolist as)

mapalist f ANil         = ANil
mapalist f (ACons a as) = ACons (f a) (mapalist f as)

tochar (C# c#) = c#
fromchar c# = C# c#
incchar c# = chr# (ord# c# +# 1#)

doalist as0
  = let	as1# = mapalist{-BChar-}    tochar   as0
	as2# = mapalist{-CharChar-} incchar  as1#
	as3  = mapalist{-CharB-}    fromchar as2#
    in as3

dolist xs = alisttolist (doalist (listtoalist xs))

main = do
    input <- getContents
    putStr (unlines (map dolist (lines input)))


data AListChar = ANilChar | AConsChar Char# AListChar

mapalistBChar f ANil         = ANilChar
mapalistBChar f (ACons a as) = AConsChar (f a) (mapalistBChar f as)

mapalistCharChar f ANilChar         = ANilChar
mapalistCharChar f (AConsChar a as) = AConsChar (f a) (mapalistCharChar f as)

mapalistCharB f ANilChar         = ANil
mapalistCharB f (AConsChar a as) = ACons (f a) (mapalistCharB f as)
