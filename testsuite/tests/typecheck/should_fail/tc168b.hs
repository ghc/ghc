{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module ShouldFail where

class C a b where { op :: a -> b }

g a = get (op a)

get :: (u,v,w,x,y,z) -> u
get (u,_,_,_,_,_) = u
