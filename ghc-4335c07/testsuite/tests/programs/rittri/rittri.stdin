infixr	->!,=\

-- auxiliary functions -----------------------------------------------------

g u v w (x:y:z) = i(v x y)(u x y (w z) z)(x:w(y:z))
g u v w [x]	= [x,512]
q u v w nil	= u : 95 : z v : w

long = several.length
((->!),(=\))=(map,($))
a	    = g q f
y	    = (-)32
z	    = (+)32
several	    = (>)2
fairlySmall = (<)64
notTooSmall = (>)91
justRight   = (==)95
notTooBig   = (<)96
veryBig	    = (>)123
goodSize x  =foldr(&&)
  otherwise =\($x)->![notTooBig,veryBig]
f y z	    =fairlySmall(z)&&goodSize(y)&&notTooSmall(z)
i cond th el=if(cond)then(th)else(el)
toBeIsToDoAndToDoIsToBeSaidConFuTse

-- main functions ----------------------------------------------------------

  g  =	interact$map
	    chr.g.map
	    ord
main =
 toBeIsToDoAndToDoIsToBeSaidConFuTse(let h=a;t=x where x x=i(long x)x(h t x)
						       q v w x z =- y w:x
						       a = g q f
						       f x y = justRight x
							     && goodSize y
				     in t)

-- rittri@cs.chalmers.se ---------------------------------------------------
