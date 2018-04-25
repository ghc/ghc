
edge  * == (*, *)
cycle * == [*]

stronglyConnComp :: [edge num] -> [num] -> [[num]]

stronglyConnComp es vs
  = snd (span_tree (new_range reversed_edges)
                   ([],[])
                   ( snd (dfs (new_range es) ([],[]) vs) )
        )
    where
||    reversed_edges :: [edge num]
    reversed_edges = map swap es
 
||    swap :: edge num -> edge num
    swap (x,y) = (y, x)
 
||    new_range :: [edge num] -> num -> [num]
    new_range    []       w = []
    new_range ((x,y):xys) w
	= (y : (new_range xys w)),	x=w
	= (new_range xys w),		otherwise
 
||    span_tree :: (num -> [num])
||		       -> ([num], [[num]])
||		       -> [num]
||		       -> ([num], [[num]])
    span_tree r (vs,ns) []   = (vs,ns)
    span_tree r (vs,ns) (x:xs)
	= span_tree r (vs,ns) xs,		member vs x
	= span_tree r (vs',(x:ns'):ns) xs,	otherwise
	  where
	    (vs',ns') = dfs r (x:vs,[]) (r x)
 
   
dfs :: (num -> [num])
            -> ([num], [num])
            -> [num]
            -> ([num], [num])

dfs r (vs,ns)   []   = (vs,ns)
dfs r (vs,ns) (x:xs) = dfs r (vs,ns) xs, 		member vs x
                     = dfs r (vs',(x:ns')++ns) xs,	otherwise
                                   where
                                     (vs',ns') = dfs r (x:vs,[]) (r x)



main 
  = stronglyConnComp edges vertices
    where

    a = 1
    b = 2
    c = 3
    d = 4
    f = 5
    g = 6
    h = 7
    vertices = [a,b,c,d,f,g,h]
    edges = [(b, a),
	     (c, b),
	     (c, d),
	     (c, h),
	     (d, c),
	     (f, a),
	     (f, g),
	     (f, h),
	     (g, f),
	     (h, g)]

