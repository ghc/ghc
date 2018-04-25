module Geometric (Geom(..)) where
import Numbers
import Vectors
import EdgePlate
import Rotate

class Geom a where
	(|||) :: a -> a -> Bool			-- two entities are parrallel ?
	vertical :: a -> Bool
	rot :: Vector -> a -> a			-- rotation
	proj :: a -> a				-- projection
	scale :: Number -> Vector -> a -> a	-- scale the entity and
						-- put it in the first quadrant


instance Geom a => Geom [a] where
	rot viewdir = map (rot viewdir)
	proj = map proj
	scale f b = map (scale f b)

instance Geom Vector where
	v ||| w = v*w == 0
	vertical v = v ||| vec [0,0,1]
	rot = rotate
	proj v = vec [x(v),y(v),0]
	scale factor base v = factor `mulv` (v - base)

instance Geom Edge where
	l ||| k	= h(l) ||| h(k)
	vertical l = vertical (h(l))
	v `rot` l = edgeT (v `rot` s(l)) (v `rot` t(l))
	proj l = edgeT (proj(s(l))) (proj(t(l)))
	scale f b l = edgeT (scale f b (s(l))) (scale f b (t(l)))

instance Geom Plate where
	plt1 ||| plt2 = n(plt1) ||| n(plt2)
	vertical plt = z( n(plt) ) == 0
	v `rot` (Plt n ls)  = Plt n [v `rot` l| l<-ls]
	proj (Plt n ls) = Plt n [proj l| l<-ls]
