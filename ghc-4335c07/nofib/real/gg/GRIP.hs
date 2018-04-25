module GRIP where

import PSlib
import StdLib
import Parse



akkumulate :: (state->a->(b,state)) -> state -> [a] -> ([b],state)
akkumulate f st [] = ([],st)
akkumulate f st (a:as) = (b:bs,st'')
	where
	(b,st') = f st a
	(bs,st'') = akkumulate f st' as

getParameters :: [Line] -> ([PElement],Int,[Line])
getParameters lines = (pe,ticks,lines')
	where
	(pe,ticks,lines') = f [] 0 lines
	f l m [] = ([],m,l)
	f l m (l'@(Ln _ (Act _ _ _ _ _ t) _):more) = f (insert l' l) (max m t) more
	f l m (PEs x:more) = (x:xs,t,l')	where (xs,t,l') = f l m more
	f l m (_:more) = f l m more

getAct :: [PElement] -> [Line] -> [Activities]
getAct [] = aux (\x->True)
getAct pes = aux (\x->elem x pes) 

aux _ [] = []
aux f ((Ln pe a s):more) | f pe = a:aux f more
			 | otherwise = aux f more
aux f (_:more) = aux f more

getSp :: [PElement] -> [Line] -> [Sparks]
getSp [] = aux' (\x->True)
getSp pes = aux' (\x->elem x pes)

aux' _ [] = []
aux' f ((Ln pe a s):more) | f pe = s:aux' f more
			  | otherwise = aux' f more
aux' f (_:more) = aux' f more


scaleAct m a@(Act n i r g f t) 	| m==t = a
				| otherwise = Act n (i*c) (r*c) (g*c) (f*c) (t*c)
					where	c = m `div` t

data Sparks = Sp Int Int Int Int Int deriving (Show{-was:Text-},Eq)
	-- bucket sprkd sused resum lost 

numberSp (Sp n _ _ _ _) = n
created (Sp _ s _ _ _) = s
used (Sp _ _ u _ _ ) = u
resumed (Sp _ _ _ r _) = r
lost (Sp _ _ _ _ l) = l

data Activities = Act Int Int Int Int Int Int deriving (Show{-was:Text-},Eq)
	-- bucket idle redn gc flush/read total

numberAct (Act b _ _ _ _ _) = b
idle (Act _ i _ _ _ _) = i
reduction (Act _ _ r _ _ _) = r
gc (Act _ _ _ g _ _) = g
flush (Act _ _ _ _ f _) = f
total (Act _ _ _ _ _ t) = t

data Line =  Ln PElement Activities Sparks | PEs PElement | BucketFull Int | Null deriving (Show{-was:Text-},Eq)

instance Parse Line where
	parseType ('B':string) = 
		((Ln pe (Act bucket idle redn gc (flush+read) (idle+redn+gc+flush+read+io)) 
				(Sp bucket sprkd sused resum lost)),more)
			where
			(pe,':':p) = parse string   
			(bucket,':':a) = parse p
			(idle,b) = parse a
			(redn,c) = parse b
			(gc,d) = parse c
			(flush,e) = parse d
			(read,k) = parse e
			(_,f) = span ((/=) ' ') (whiteSpace k)
			(sprkd,g) = parse f
			(sused,h) = parse g
			(resum,i) = parse h
			(lost,j) = parse i
			(io,more) = parse j
	parseType ('P':'S':string) = (PEs pe,more)
		where
		(pe,more) = parse (tail (dropWhile ((/=) 'r') string))
--	parseType ('S':' ':string) = test (reverse string) 
--			where 
--			test ('.':'m':_) = (BucketFull x,"")
--			test _ = (Null,"")
--			(x,_) = parse string
	parseType string = (Null,string)

instance Ord Line where
	(<=) x@(Ln _ (Act b _ _ _ _ _) _) y@(Ln _ (Act b' _ _ _ _ _) _) = b<=b'

data PElement = PE String Int deriving (Eq,Show{-was:Text-})

instance Parse PElement where
	parseType string = (PE name no,more)
		where
		(name,'.':a) = span ((/=) '.') string
		(no,more) = parse a

