module Prog(prog) where

-- ************** SeqSer *************

-- strictly serial search
-- sequential 

--partain:import Libfuns
import Auxil
import Key

prog :: String -> String
prog _ = show cichelli

data Status a = NotEver Int | YesIts Int a deriving ()
instance (Show a) => Show (Status a) where
    showsPrec d (NotEver i) = showParen (d >= 10) showStr
      where
	showStr = showString "NotEver" . showChar ' ' . showsPrec 10 i

    showsPrec d (YesIts i a) = showParen (d >= 10) showStr
      where
	showStr = showString "YesIts" . showChar ' ' . showsPrec 10 i
		  . showChar ' ' . showsPrec 10 a

--  readsPrec p = error "no readsPrec for Statuses"
--  readList = error "no readList for Statuses"
    showList []	= showString "[]"
    showList (x:xs)
		= showChar '[' . shows x . showl xs
		  where showl []     = showChar ']'
			showl (x:xs) = showChar ',' . shows x . showl xs

type FeedBack = Status HashFun

cichelli :: FeedBack
cichelli = findhash hashkeys
                where
-- #ifdef SORTED
                hashkeys = (blocked.freqsorted) attribkeys
-- #else
--                hashkeys = blocked attribkeys
-- #endif

	
findhash :: [Key] -> FeedBack 
findhash = findhash' (H Nothing Nothing []) []


findhash' :: HashSet -> HashFun -> [Key] -> FeedBack
findhash' keyHashSet charAssocs [] = (YesIts 1 charAssocs)
findhash' keyHashSet charAssocs (k@(K s a z n):ks) =
  ( case (assocm a charAssocs, assocm z charAssocs) of
	  (Nothing,Nothing) -> if a==z then 
				firstSuccess (\m->try [(a,m)]) [0..maxval] 
				else 
				firstSuccess (\(m,n)->try [(a,m),(z,n)]) 
					    [(m,n) | m<-[0..maxval], n<-[0..maxval]]
          (Nothing,Just zc) -> firstSuccess (\m->try [(a,m)]) [0..maxval] 
	  (Just ac,Nothing) -> firstSuccess (\n->try [(z,n)]) [0..maxval]
	  (Just ac,Just zc) -> try [] )
  where
  try newAssocs = ( case hinsert (hash newCharAssocs k) keyHashSet of
             Nothing -> (NotEver 1)
             Just newKeyHashSet -> findhash' newKeyHashSet newCharAssocs ks )
             where
             newCharAssocs = newAssocs ++ charAssocs

-- Returns the first successful `working' function on a list of possible arguments
firstSuccess :: (a -> FeedBack) -> [a] -> FeedBack 
firstSuccess f possibles =  first 0 (map f possibles) 

first :: Int -> [FeedBack] -> FeedBack
first k [] = NotEver k
first k (a:l) = case a of
                (YesIts leaves y) -> YesIts (k+leaves) y
                (NotEver leaves)    -> first (k+leaves) l
