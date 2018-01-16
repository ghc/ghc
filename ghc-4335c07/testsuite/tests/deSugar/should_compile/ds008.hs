-- !!! ds008 -- free tyvars on RHSs
--
-- these tests involve way-cool TyApps

module ShouldCompile where

f x = []

g x = (f [],[],[],[])

h x = g (1::Int)
