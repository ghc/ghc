module Activity (activityGraph,Activity(..))  where

import GRIP
import StdLib
import PSlib
import Graph
import Parse


activityGraph ordering selectpes statFile = 
				--show (pes,ticks) ++
				--show "DEBUG " ++ show (aggs) ++
			  	initGraph "Processor Activity Graph"
				 (pes,selectpes) (ticks*100,100) ("Time (ms)","% Activity") 
						(map f ordering)
				
				++ scale (my_fromInt dimX/my_fromInt 100) 
						(my_fromInt dimY/my_fromInt (maxticks))
				++ concat (map2 plotCurve (map colour order)
						  (outlinesTrace traces))
	where
	f a = (colour a,display a,aggr a aggs)
	active = if selectpes==[] then length pes-1 else length selectpes
	maxticks = active*ticks
	(pes,ticks,orderedStats) = getParameters stats
	(traces,aggs) = (akkumulate (processAct (map extractor order)) nullstate.
					gatherAct (Act 0 0 0 0 0 0).
					map (scaleAct ticks).
					getAct selectpes) orderedStats
	order = reverse ordering
	stats = parseFile statFile

processAct :: [Activities->Int] -> State -> Activities -> (Trace,State)
processAct extractors (i,r,g,f,t) a@(Act n i' r' g' f' t') 
                        = (trace, (i'+i,r'+r,g'+g,f'+f,t+t'))
		where
		trace@(T _ (m:_)) = makeTrace extractors n a

makeTrace fs n s = T n (f fs)
	where
	f [] = []
	f ex@(e:es) = sum (pam ex s):f es


type State = (Int,Int,Int,Int,Int)
nullstate = (0,0,0,0,0)

data Trace = T Int [Int]

outlinesTrace :: [Trace] -> [[Point]]
outlinesTrace [T n a] = map (\x->[Pt n x]) a
outlinesTrace (T n a:more) = map2 (:) (map (\x->Pt n x) a) (outlinesTrace more)
 
aggr IDLE (i,_,_,_,t) = printFloat (percentage i t) ++ "%"
aggr REDN (_,r,_,_,t) = printFloat (percentage r t) ++ "%"
aggr GC  (_,_,g,_,t) = printFloat (percentage g t) ++ "%"
aggr FLUSH (_,_,_,f,t) = printFloat (percentage f t) ++ "%"

percentage x y = my_fromInt x * 100 / my_fromInt y
 
gatherAct :: Activities -> [Activities] -> [Activities]
gatherAct t [] = [t,(Act (numberAct t+1) 0 0 0 0 0)]
gatherAct t l@(a:as) | numberAct t==numberAct a = gatherAct (addAct t a) as
                     | otherwise = t:gatherAct (Act (n+1) 0 0 0 0 0) l
					where 	n=numberAct t
 

pam [] _ = []
pam (f:fs) a = f a:pam fs a
 

data Activity = REDN | IDLE | FLUSH | GC deriving (Eq)

extractor REDN = reduction
extractor IDLE = idle
extractor GC = gc
extractor FLUSH = flush

colour REDN = 0
colour IDLE = 8
colour FLUSH = 5
colour GC = 2

instance Parse Activity where
	parseType ('R':string) = (REDN,string)
	parseType ('G':string) = (GC,string)
	parseType ('F':string) = (FLUSH,string)
	parseType ('I':string) = (IDLE,string)
	parseType (string) = error ("No such Activity : "++show string++"\n")

display REDN = "Reduction"
display GC = "Garbage Collection"
display FLUSH = "Flush Read/Write"
display IDLE = "Idle"
	
addAct (Act _ a b c d t1) (Act n e f g h t2) = Act n (a+e) (b+f) (c+g) (d+h) (t1+t2)
