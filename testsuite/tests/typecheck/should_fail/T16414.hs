{-# LANGUAGE FlexibleContexts, FlexibleInstances, AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module T16414 where

data I = I

class All2 x => All x
class All x => All2 x

class AllZip2 f
instance AllZip2 f

f1 :: (All x, AllZip2 I) => x -> ()
f1 = f2

f2 :: AllZip2 f => x -> ()
f2 _ = ()
