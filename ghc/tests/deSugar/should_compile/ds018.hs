-- !!! ds018 -- explicit lists and tuples (with disabled LARGE tuples!)
--
module ShouldCompile where

-- exprs

f x y z = [x,y,z,x,y,z]
f2 x y	= []

g1 x y  = ()

{- Although GHC *should* provide arbitrary tuples, it currently doesn't
   and probably won't in the near future, so this test is only a reminder.

g x y z = (x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z,
	   x,y,z,x,y,z) -- hey, we love big tuples
-}

-- pats

fa [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] = x

fb [] = []

{- See above
ga (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
    aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,
    an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az) = x
-}

gb () x = x
gb2 ()  = ()

-- need to think of some better ones...
