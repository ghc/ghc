{-# LANGUAGE NoTypeApplications, NoKindSignatures, NoDataKinds #-}
module T where

data Proxy a = P

f (P @[a,b])    = ()
g (P @1)        = ()
h (P @(t @k))   = ()
j (P @(t :: k)) = ()
k (P @('(a,b))) = ()
l (P @"str")    = ()
d (P @'c')      = ()
e (P @'True)    = ()
