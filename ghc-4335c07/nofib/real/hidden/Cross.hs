module Cross(cross,overlap) where
import Numbers
import Vectors
import EdgePlate
import Solve
import Preds
import Data.List(nub)--1.3
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- section 6: Crossing edges (MR was: lines)

-- cross yields the list of crosspoints of two edges (0 or 1 cross)
-- s(l)+lambda* h(l) = s(k)+mu*h(k) => s(l)-s(k) = lambda*(-h(l))+mu*h(k)

cross :: Edge -> Edge -> [(Vector,Vector)]
cross l k = [(s(l) + lambda`mulv`h(l), s(k) + mu`mulv`h(k))
	    |(lambda,mu) <- solve (-h l) (h k) (s l - s k)
	    ,0 <= lambda && lambda <= 1 && 0 <= mu && mu <= 1]

-- overlap computes the important points on the border of plate ls
-- that are inside ks (and of course also inside ls)
overlap :: Plate -> Plate -> [Vector]
overlap (Plt _ ls) p2@(Plt _ ks)= nub ([s(l)| l<-ls, s(l)`into` p2] ++
				       [p| l<-ls, k<-ks , (p,q)<-cross l k] )
