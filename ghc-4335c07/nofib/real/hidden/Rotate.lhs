> module Rotate (rotate) where
> import Numbers
> import Vectors
> import Matrices

> rotate	:: Vector -> Vector -> Vector
> rotate v w	| v==0			= w
>		| normv==vec [0,0,1]	= w
>		| normv==vec [0,0,-1]	= -w
>		| otherwise		= mat [p,q,r] `mulm` w
>		  where normv = norm(v)
>			p = norm(q*r)
>			q = norm(vec [0,0,1]*r)
>			r = normv
