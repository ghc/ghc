module State (State, returnS, eachS, thenS, putS, getS, useS) where

data  State s x       =   Abs (s -> (x, s))
rep                   ::  State s x -> (s -> (x,s))
rep (Abs f)           =   f
returnS               ::  x -> State s x
returnS x             =   Abs (\s -> (x, s))
eachS                 ::  State s x -> (x -> y) -> State s y
xS `eachS` f          =   Abs (\s -> let (x,s') = rep xS s in (f x, s'))
thenS                 ::  State s x -> (x -> State s y) -> State s y
xS `thenS` kS         =   Abs (\s -> let (x,s') = rep xS s in rep (kS x) s')
putS                  ::  s -> State s ()
putS s'               =   Abs (\s -> ((), s'))
getS                  ::  State s s
getS                  =   Abs (\s -> (s,s))
useS                  ::  State s x -> s -> x
useS xS s             =   let (x,s') = rep xS s in  x
