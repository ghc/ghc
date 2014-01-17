{-# OPTIONS_GHC -fcpr-depth=1 #-}

module InfiniteCPR where

data Rec1 = Foo Rec2 Rec2
data Rec2 = Bar Rec1 Rec1

f a =
    let x = Foo a y
        y = Bar x x
    in x

