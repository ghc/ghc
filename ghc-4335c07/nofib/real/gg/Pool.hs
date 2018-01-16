module Pool(poolGraph)  where

import StdLib
import GRIP
import PSlib
import Graph
import Parse

poolGraph selectpes statFile = 
	initGraph "Spark Pool Profile Graph"  (pes,selectpes) (100*ticks,height) 
			("Time (ms)", "Sparks") [(5,"Spark Residency",""), (0,"Sparks Used","")]
			++ scale (my_fromInt dimX/my_fromInt 200) (my_fromInt dimY/my_fromInt height)
			++ plotCurve 0 usedGraph
			++ plotCurve 5 sparkGraph 
	where
	(sparkGraph,usedGraph) = outlineGraphs (traces++[T width [0,0,0]])
	(pes,ticks,orderedStats) = getParameters stats
	height = axisScale h
	stats = parseFile statFile
	(traces,((sparks,used,resumed),_,h,width)) 
		= akkumulate processSparks nullstate (gatherSp (Sp 0 0 0 0 0) (getSp selectpes orderedStats))


processSparks ((c'',u'',r''),l'',m,_) (Sp n c u r l) 
			= (T n [p',u',l'], ((c''+c,u''+u,r''+r),l',max m p',n))
	where
	p' = l''+c
	u' = p'-l
	l' = p'-u-l


gatherSp t [] = [t]
gatherSp t l@(a:as) | numberSp t==numberSp a = gatherSp (addSparks t a) as
                    | otherwise = t:gatherSp (Sp (n+1) 0 0 0 0) l
                                        where   n=numberSp t


addSparks (Sp _ a b c d) (Sp n a' b' c' d') = Sp n (a+a') (b+b') (c+c') (d+d')

data Trace = T Int [Int] deriving Show{-was:Text-}

nullstate = ((0,0,0),0,0,0)

type Object = [Point]

outlineGraphs :: [Trace] -> ([Point],[Point])
outlineGraphs traces = aux traces
	where   
		aux [] = ([],[])
		aux (T n [p,u,l]:ts) = (Pt t p:Pt t l:Pt t' l:pas,
					Pt t u:Pt t' l:pbs)
			where
			(pas,pbs) = aux ts 
			t = n*2
			t' = n*2+1
