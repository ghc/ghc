module MaybeM
      (Maybe, returnM, eachM, thenM,
       failM, orM, guardM, filterM, theM, existsM, useM)
      where

--1.3: data  Maybe x  =  Just x | Nothing
returnM               ::  x -> Maybe x
returnM x             =   Just x
eachM                 ::  Maybe x -> (x -> y) -> Maybe y
(Just x) `eachM` f    =   Just (f x)
Nothing `eachM` f     =   Nothing
thenM                 ::  Maybe x -> (x -> Maybe y) -> Maybe y
(Just x) `thenM` kM   =   kM x
Nothing `thenM` kM    =   Nothing
failM                 ::  Maybe x
failM                 =   Nothing
orM                   ::  Maybe x -> Maybe x -> Maybe x
(Just x) `orM` yM     =   Just x
Nothing `orM` yM      =   yM
guardM                ::  Bool -> Maybe x -> Maybe x
b `guardM` xM         =   if  b  then  xM  else  failM
filterM               ::  (x -> Bool) -> Maybe x -> Maybe x
p `filterM` xM        =   xM `thenM` (\x -> p x `guardM` returnM x)
theM                  ::  Maybe x -> x
theM (Just x)         =   x
existsM               ::  Maybe x -> Bool
existsM (Just x)      =   True
existsM Nothing       =   False
useM                  ::  x -> Maybe x -> x
useM xfail (Just x)   =   x
useM xfail Nothing    =   xfail
