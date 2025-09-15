{-# LANGUAGE DataKinds, TypeAbstractions #-}
{-# OPTIONS_GHC -Werror=unticked-promoted-constructors #-}
module T where

data Proxy a = P
data Op a b = a :- b

f (P @[a,b])    = ()
g (P @(a :- b)) = ()
