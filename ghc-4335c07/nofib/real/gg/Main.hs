module Main where

import GRIP
import PSlib
import Graph
import Parse
import Pool
import Activity
import Spark
--import Prog (prog)

import System.Environment

main = do
    str <- getArgs
    control (map parseLine (condenseArgs str))

control args = do
    stats <- if from=="stdin" then getContents else readFile from
    (if into=="stdout" then putStr else writeFile into) (form (graph stats))
  where
    form :: (String -> Postscript)
    form = if (sizeX==0) then (if (elem G args) then gspostscript else postscript)
	    else ePostscript (sizeX,sizeY)
    graph :: String -> Postscript
    graph stats = if (elem P args) then poolGraph processors stats
	    else if orderSp/=[] then sparkGraph orderSp processors stats 
		    else activityGraph orderAct processors stats

    (A orderAct) = lookUp (A defaultAct) args
    (S orderSp) = lookUp (S []) args
    (E sizeX sizeY) = lookUp (E 0 0) args
    (PS processors) = lookUp (PS []) args
    (IO (from,into)) = lookUp (IO ("stdin","stdout")) args

condenseArgs :: [String] -> [String]
condenseArgs [] = []
condenseArgs (arg@('-':_):more) = arg:condenseArgs more
condenseArgs [a,b] = [a++" "++b]
condenseArgs a = a

	
lookUp :: Args -> [Args] -> Args
lookUp a [] = a
lookUp a (b:bs) | a==b = b
		| otherwise = lookUp a bs

data Args = A [Activity]
	  | S [Spark]
	  | P
	  | E Int Int
	  | F
	  | G
	  | PS [PElement]
	  | IO (String,String) 

instance Eq Args where
	(==) (A _) (A _) = True
	(==) (S _) (S _) = True
	(==) P P = True
	(==) (E _ _) (E _ _) = True
	(==) F F = True
	(==) G G = True
	(==) (PS _) (PS _) = True
	(==) (IO _) (IO _) = True
	(==) _ _ = False

defaultAct = [GC,REDN,IDLE,FLUSH]
defaultSp = [RESUMED,USED,CREATED,LOST]
defaultSize = (15::Int,10::Int) 
defaultPS = [PE "14" 1]

instance Parse Args where
	parseType ('-':'A':string) = (A order,more)
		where
		order = if (whiteSpace string)=="" then defaultAct
				 else x
		(x,more) = parse string
	parseType ('-':'S':string) = (S order,more)
                where
                order = if (whiteSpace string=="") then defaultSp
                                 else x
                (x,more) = parse string
	parseType ('-':'P':string) = (P,string)
	parseType ('-':'E':string) = (E x y,"")
		where
		(x,y) = if (whiteSpace string) == "" then defaultSize
				else (p,q)
		(p,'x':a) = parse string
		(q,more) = parse a
	parseType ('-':'F':string) = (F,string)
	parseType ('-':'G':string) = (G,string)
	parseType ('-':'p':string) = (PS processors,more)
                where
                order = if (whiteSpace string=="") then defaultPS
                                 else processors
                (processors,more) = parse string
	parseType ('-':string) = error("Illegal flag to GRIP-graph : -" ++ string ++ "\n")
	parseType string = (IO files,"")
		where 
		files = if string == "" then ("stdin","stdout") 
				else if (whiteSpace more) == "" then (file1,file1++".ps")
					else (file1,file2)
		(file1,more) = span (\x->x/=' ') string
		(file2,_) = span (\x->x/=' ') (whiteSpace (more++" "))
