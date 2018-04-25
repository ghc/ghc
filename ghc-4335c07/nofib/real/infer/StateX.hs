module StateX (StateX, returnSX, eachSX, thenSX, toSX, putSX, getSX, useSX) where

data  StateX s a      =   MkSX (s -> a)
rep (MkSX f)          =   f
returnSX returnX x    =   MkSX (\s -> returnX (x, s))
eachSX eachX xSX f    =   MkSX (\s -> rep xSX s `eachX` (\(x,s') -> (f x, s')))
thenSX thenX xSX kSX  =   MkSX (\s -> rep xSX s `thenX` (\(x,s') -> rep (kSX x) s'))
toSX eachX xX         =   MkSX (\s -> xX `eachX` (\x -> (x,s)))
putSX returnX s'      =   MkSX (\s -> returnX ((), s'))
getSX returnX         =   MkSX (\s -> returnX (s,s))
useSX eachX s xSX     =   rep xSX s `eachX` (\(x,s') -> x)
