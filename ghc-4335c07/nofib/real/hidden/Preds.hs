module Preds(into,onto) where
import Numbers
import Vectors
import EdgePlate
import Geometric
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- section 9: Predicates

-- p `onto` l <=> proj(p) is member of set proj(l)
onto ::  Vector -> Edge -> Bool
p`onto`l | vertical l = proj(p)==proj(s(l))
	 | otherwise  = v ||| w && 0<=v`inpr`w && v`inpr`w<=w`inpr`w
			where v = proj(p-s(l))
			      w = proj(h(l))
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- section 9: Predicates

-- v `into` ls means that proj(v) is inside (including the border of) proj(ls).
into :: Vector -> Plate -> Bool
v`into`p @ (Plt _ ls)
	| vertical p  = or  [v`onto`l |l<-ls]
	| otherwise   = and [a>=0| a<-zs] || and [a<=0| a<-zs]
			where zs = [z ( (v-s(l)) * h(l) )| l<-ls]
