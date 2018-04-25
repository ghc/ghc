module Comparing (above,cmp) where
import Numbers
import Vectors
import EdgePlate
import Preds
import Solve
import Geometric
import Cross
import Data.List(nub)--1.3
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- section 8: Comparing

cmp :: Plate -> [Edge] -> [Edge]
cmp p ls = concat [comp l p| l<-ls]

comp :: Edge -> Plate -> [Edge]
comp l p@(Plt _ ks) =
	if          inS &&     inT	then []
	else if crosses==[]		then [l]
	else if     inS && not inT	then [edgeT w (t l)]
	else if not inS &&     inT	then [edgeT (s l) v]
					else [edgeT (s l) v, edgeT w (t l)]
	where
	inS	= s(l) `into` p
	inT	= t(l) `into` p
	crosses	= nub [p| k<-ks, (p,q)<-cross l k]
	cr1	= head crosses
	cr2	= last crosses
	(v,w)	= if len(s(l)-cr1) <= len(s(l)-cr2)
		  then (cr1,cr2)
		  else (cr2,cr1)


-- ls `above` ks means that the edges of ks have to be inspected
-- individually, and hidden where lying inside ls.
-- Be careful not to call 'alt ls' when ls is vertical
above :: Plate -> Plate -> Bool
ls `above` ks =
        if vertical ls
        then if vertical ks
             then or [z(p) > z(q)| p<-olk, q<-okl]
             else or [  z(p)   > alt ks p| p<-olk]
        else if vertical ks
             then or [alt ls q >   z(q)  | q<-okl]
             else or [alt ls p > alt ks p| p<-ovl]
        where olk = overlap ls ks
              okl = overlap ks ls
              ovl = nub ([proj p|p<-olk]++[proj p|p<-okl])
