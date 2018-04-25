module Spark(sparkGraph,Spark(..))  where

import StdLib
import GRIP
import PSlib
import Graph
import Parse


sparkGraph ordering selectpes statFile = initGraph "Spark Activity Graph" (pes,selectpes)
					 (100*ticks,height) ("Time (ms)", "Sparks")
					 (map f ordering)
				++ scale (my_fromInt dimX/my_fromInt 100) 
						(my_fromInt dimY/my_fromInt height)
				++ concat (map2 plotCurve (map colourSpark order)
						  (outlinesTrace traces))
	where
	f a = (colourSpark a,displaySpark a,aggr a aggs)
	(pes,ticks,orderedStats) = getParameters stats
	height = axisScale h
	(traces,(aggs,h,w)) = akkumulate(processSparks (map extractor order)) nullstate
					(gatherSp (Sp 0 0 0 0 0) (getSp selectpes orderedStats))
	order = reverse ordering
	stats = parseFile statFile

processSparks :: [Sparks->Int] -> State -> Sparks -> (Trace,State)
processSparks extractors ((c,u,r,l),graphmax,_) s@(Sp n c' u' r' l')
                        = (trace,
				((c'+c,u'+u,r'+r,l'+l),max graphmax m,n))
		where
		trace@(T _ (m:_)) = makeTrace extractors n s

makeTrace fs n s = T n (f fs)
	where
	f [] = []
	f ex@(e:es) = sum (pam ex s):f es


type State = ((Int,Int,Int,Int),Int,Int)
nullstate = ((0,0,0,0),0,0)

data Trace = T Int [Int]

outlinesTrace :: [Trace] -> [[Point]]
outlinesTrace [T n a] = map (\x->[Pt n x]) a
outlinesTrace (T n a:more) = map2 (:) (map (\x->Pt n x) a) (outlinesTrace more)
 
pam [] _ = []
pam (f:fs) a = f a:pam fs a
 
gatherSp t [] = [t,(Sp (numberSp t+1) 0 0 0 0)]
gatherSp t l@(a:as) | numberSp t==numberSp a = gatherSp (addSparks t a) as
                    | otherwise = t:gatherSp (Sp (n+1) 0 0 0 0) l
					where 	n=numberSp t
 
addSparks (Sp n a b c d) (Sp _ e f g h) = Sp n (a+e) (b+f) (c+g) (d+h)

data Spark = USED | RESUMED | CREATED | LOST deriving (Eq)

aggr :: Spark -> (Int,Int,Int,Int) -> String
aggr CREATED (i,_,_,_) = show i
aggr USED (_,r,_,_) = show r
aggr RESUMED  (_,_,g,_) = show g
aggr LOST (_,_,_,f) = show f

extractor LOST = lost
extractor CREATED = created
extractor RESUMED  = resumed
extractor USED  = used

colourSpark :: Spark -> Int
colourSpark LOST = 8
colourSpark CREATED = 5
colourSpark RESUMED = 2
colourSpark USED = 0

displaySpark :: Spark -> String
displaySpark LOST = "Sparks Lost"
displaySpark CREATED = "Sparks Created"
displaySpark RESUMED = "Resumed Sparks"
displaySpark USED = "Used Sparks"
	
instance Parse Spark where
	parseType ('L':string) = (LOST,string)
	parseType ('C':string) = (CREATED,string)
	parseType ('R':string) = (RESUMED,string)
	parseType ('U':string) = (USED,string)
	parseType (string) = error ("No such Activity : "++show string++"\n")

