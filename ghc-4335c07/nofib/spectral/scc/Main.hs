-- !!! strongly-connected components of a graph
-- (courtesy mainly of John Launchbury)

import Digraph

main = print (stronglyConnComp edges vertices)
  where
    -- here's a test graph: Figure 6.4 from SLPJ 87
    a, b, c, d, f, g, h :: Int
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
