{-# LANGUAGE TypeFamilies #-}
module DataFamDeriv where

data family Foo a
data Bar = Bar
data instance Foo Bar
    = Bar1 | Bar2 | Bar3 | Bar4 | Bar5 | Bar6 | Bar7 | Bar8 | Bar9
    deriving Eq



