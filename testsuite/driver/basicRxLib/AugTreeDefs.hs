module AugTreeDefs 
     (Aug,AugTree,
      AugInfo(..),updateAugTree,
      getAugInfo,
      getLbl,
      firstPos,
      lastPos,children,followPos,
      followInfo,followInfoNoInt,follows,followsWithoutInt,
      isNullable)      

where


import OrdList
import BagRE
import FiniteMap
import AbsTreeDefs

{- An augmented Abstract syntax tree contains more info:
     1 Labels on the leafs
   and for each node, n, info about
     2 firstpos (the leafs with which the regexp rooted at n can start)
     3 lastpos  (the leafs with which the regexp rooted at n can end)
     4 children (all the leaf nodes which are child nodes of n)
     5 followpos information about what follows each leaf node, based
		 on regexp rooted at n.
                 this is a pair, there's follow info counting intervals,
                 and follow info without counting intervals.
                 Adding subexp info requires the first, adding
		 interval info requires the second.
     6 Nullable (Whether regular expression rooted at n can match 0 times)
-}

type AugTree a = AbsTree a (Aug a)
data Aug a = AUG (Maybe Int) -- label for node, only labelled if its a leaf
		 AugInfo     -- extra AugInfo

data AugInfo = AUGINFO (OrdList Int, -- The firstpos info
		        OrdList Int, -- The lastpos info
		        OrdList Int, -- The children info
                        ([(Int,BagRE Int)],  -- without interval follows
					     -- for use with HandleInterval
			 [(Int,OrdList Int)])-- including interval follows
                                             -- for use with HandleSubexp
		       )	     -- follow info for regexp rooted here
		       Bool	 -- Nullable

-- update an AugTree

updateAugTree :: Maybe Int 
	      -> (OrdList Int,
	          OrdList Int,
	          OrdList Int,
                  ([(Int,BagRE Int)],
                   [(Int,OrdList Int)]))
	      -> Bool
	      -> AugTree a
	      -> AugTree a
updateAugTree a b c re = updateExtraRE (AUG a (AUGINFO b c)) re 


-- interrogate AugTree

getAugInfo :: AugTree a	-- an augmented tree 
           -> AugInfo	-- the firstpos,lastpos... info

getAugInfo re = case (getExtraRE re) of
		 (AUG _ ainfo) -> ainfo

getLbl :: AugTree a -> Maybe Int
getLbl re = case (getExtraRE re) of
             (AUG lbl _) -> lbl 

firstPos :: AugTree a -> OrdList Int
firstPos re = case (getAugInfo re) of
                 (AUGINFO (fs,_,_,_) _) -> fs 

lastPos :: AugTree a -> OrdList Int
lastPos re = case (getAugInfo re) of
                 (AUGINFO (_,ls,_,_) _) -> ls 

children :: AugTree a -> OrdList Int
children re = case (getAugInfo re) of
                 (AUGINFO (_,_,chs,_) _) -> chs 

followInfoNoInt :: AugTree a -> [(Int,BagRE Int)]
followInfoNoInt re = case (getAugInfo re) of
                      (AUGINFO (_,_,_,fi) _) -> fst fi

followInfo :: AugTree a -> [(Int,OrdList Int)]
followInfo re = case (getAugInfo re) of
                   (AUGINFO (_,_,_,fi) _) -> snd fi

followPos :: Int -> AugTree a -> OrdList Int
followPos x re = (snd.head.(filter ((== x).fst)).followInfo) re

follows :: OrdList Int -> AugTree a -> [(Int,OrdList Int)]
{- For instance with "((a)(b))*c{2,}", if there are 3 nodes, "a", "b" and "c".
   follows [2,3] will give, [(2,[1,3]),(3,[3,4])].

-}
follows xs t = filter (\(a,b) -> a `elem` xs) (followInfo t)

followsWithoutInt :: OrdList Int -> AugTree a -> [(Int,OrdList Int)]
{- For instance with "((a)(b))*c{2,}", if there are 3 nodes, "a", "b" and "c".
   follows [2,3] will give, [(2,[1,3]),(3,[4])].
-}
followsWithoutInt xs t = map (\(a,b) -> (a,bagREToOL b)) 
                             ( filter (\(a,b) -> a `elem` xs) 
                                      (followInfoNoInt t) )

isNullable :: AugTree a -> Bool
isNullable t = case getAugInfo t of
                  (AUGINFO _ null) -> null

