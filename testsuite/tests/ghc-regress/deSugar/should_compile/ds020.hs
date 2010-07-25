-- !!! ds020 -- lazy patterns (in detail)
--

{-# LANGUAGE NPlusKPatterns #-}

module ShouldCompile where

a ~([],[],[])    = []
a ~(~[],~[],~[]) = []

b ~(x:xs:ys)      = []
b ~(~x: ~xs: ~ys) = []

c ~x ~ _ ~11111 ~3.14159265 = x

d 11	    = 4
d 12	    = 3
d ~(n+4)    = 2
d ~(n+43)   = 1
d ~(n+999)  = 0

f ~(x@[]) = []
f x@(~[]) = []

g ~(~(~(~([])))) = []

-- pattern bindings (implicitly lazy)

([],[],[])    = ([],[],[])
(~[],~[],~[]) = ([],[],[])

(x1: xs1: ys1) = []
(~x: ~xs: ~ys) = []

(x2 : xs2: ys2) | eq2  = []
	        | eq3  = [x2]
		| eq4  = [x2]
		| True = []
		where
		  eq2 = (2::Int) == (4::Int)
		  eq3 = (3::Int) == (3::Int)
		  eq4 = (4::Int) == (2::Int)

(x3,y3) | x3 >  3 = (4, 5)
        | x3 <= 3 = (2, 3)
-- above: x & y should both be \bottom.

(x4,(y4,(z4,a4))) | eq2  = ('a',('a',('a','a')))
	          | eq3  = ('b',('b',('b','b')))
	          | eq4  = ('c',('c',('c','c')))
	          | True = ('d',('d',('d','d')))
	          where
		     eq2 = (2::Int) == (4::Int)
		     eq3 = (3::Int) == (3::Int)
		     eq4 = (4::Int) == (2::Int)


