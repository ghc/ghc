-- !!! ds019 -- mixed var and uni-constructor pats

module ShouldCompile where

f (a,b,c) i     o = []
f d       (j,k) p = []
f (e,f,g) l     q = []
f h       (m,n) r = []
