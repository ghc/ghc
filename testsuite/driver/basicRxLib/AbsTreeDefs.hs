module AbsTreeDefs
   (AbsTree(..),Regexp,
    mkEl,mkBackref,mkEnd,justPrefix,
    mkSub,mkOpt,mkStar,mkPlus,mkMin,mkExact,mkMinMax,mkNg,
    mkCon,mkAlt,
    mkMinDef,mkMinMaxDef,
    addEnd,
    isElem,isBref,isEnd,
    isSub,isStar,isPlus,isOpt,isMin,isMinmax,isNg,isInterval,isRepeatable,
    isCon,isAlt,
    isLeaf,isUn,isBin,child,left,right,updateChild,updateLeft,updateRight,
    getMin,getMinMax,getPrefix,
    getElem,getBref,getSEName,
    getExtraRE,updateExtraRE,
    foldPostT,foldPreT
    )


where

import FiniteMap
import Matchers

data AbsTree a b = ABS (Regexp a b,b) -- regexp with some extra info at each node

data Regexp a b = 
          -- These have no children
               EL (MatcherImpl a)		-- An element matcher
		  Bool			-- is this a prefix matcher
             | BACKREF	String		-- Match the nth subexp, for given n
			([a]->MatcherImpl a)-- The matcher function to use

             | END			-- End of the Regexp

          -- These are have 1 child
             | SUB  String		 -- the name to reference the subexp
                    (AbsTree a b)	 -- Root of a subexpression
             | STAR (AbsTree a b)	 -- Match 0 or more
             | PLUS (AbsTree a b)	 -- Match 1 or more
             | OPT  (AbsTree a b)	 -- Match 0 or 1
             | MIN Int		  -- Match at least n times, for given n
		   (AbsTree a b)
		   
             | MINMAX Int Int	  -- Match between n and m times, for given n,m
		      (AbsTree a b)
		      
             | NGMIN Int	    -- Match at least n times, non-greedily
		     (AbsTree a b)
		     
             | NGMINMAX Int Int	-- Match between n and m times, non-greedily
			(AbsTree a b) 
			
          -- These have two children
             | CON (AbsTree a b)	-- Match concatenation of two subregexps
		   (AbsTree a b)
		   
             | ALT (AbsTree a b)	-- Match first or second subregexp
		   (AbsTree a b)
		   
          
-- Build a regexp tree      
-- mk functions are wrappers to build the abstract syntax tree for a node.

junk = error "Not defined yet"

-- leaf nodes
mkEl f = ABS (EL f False,junk)
mkBackref x f = ABS (BACKREF x f,junk)
mkEnd = ABS (END,junk)
justPrefix (ABS (EL f _,stuff))= ABS (EL f True,stuff) 

-- nodes with one child
mkSub s n = ABS (SUB s n,junk)
mkOpt n = ABS (OPT n,junk)
mkStar n = ABS (STAR n, junk)
mkPlus n = ABS (PLUS n,junk)

mkMin 0 n = ABS (STAR n,junk)
mkMin 1 n = ABS (PLUS n,junk)
mkMin x n | isElem n = 
            let bef = (mkEl . foldl1 regseq . map getElem .take (x-1)) (repeat n) 
            in  mkCon bef (mkPlus n)
          | otherwise = ABS (MIN x n,junk)
-- JRS: redundant clause: mkMin x n = ABS (MIN x n,junk)

-- make a min node that is not to be optimised into a plus or star
mkMinDef x n other = ABS (MIN x n,other)

mkExact x n | isElem n =
                (mkEl . foldl1 regseq . map getElem . take x) (repeat n) 
mkExact 1 n = n
mkExact x n = ABS (MINMAX x x n,junk)

mkMinMax 0 1 n = ABS (OPT n,junk) 
mkMinMax x y n | x <= 1 = ABS (MINMAX x y n,junk)
mkMinMax x y n | isElem n = 
              let bef = (mkEl . foldl1 regseq . map getElem.take (x-1)) (repeat n)
              in  mkCon bef (ABS (MINMAX 1 (y-x+1) n,junk))
mkMinMax x y n = ABS (MINMAX x y n, junk)
            
-- make a MINMAX node that is not to be optimised into a plus or star
mkMinMaxDef x y n other = ABS (MINMAX x y n,other)

-- make a repetition node into a nongreedy one
mkNg n =
           case n of 
		(ABS (STAR n,junk)) -> mkAlt -- both nodes must be nullable as AugTrees
                                             --(this is done by making them intervals)
                                      (ABS (NGMINMAX 0 1 --prefer to match 0 length
                                           (mkEl matchNull),junk))
                                      (ABS (NGMIN 0  n ,junk))
		(ABS (PLUS n,junk)) -> ABS (NGMIN 1  n ,junk)
		(ABS (OPT n,junk)) -> mkAlt -- both nodes must be nullable as AugTrees
                                            --(this is done by making them intervals)
                                     (ABS (NGMINMAX 0 1  --prefer to match 0 length
                                          (mkEl matchNull),junk))
                                     (ABS (NGMINMAX 0 1  n,
                                           junk))
		(ABS (MIN x  n,junk)) -> ABS (NGMIN x  n ,junk)
                                          
		(ABS (MINMAX x y  n,junk)) 
			-> if (x /= y) then -- if matching exactly num times,
					    -- then non-greedy irrelevant
			     ABS (NGMINMAX x y  n ,junk)
			   else if x > 0 then
			     ABS (NGMINMAX x y  n ,junk)
                           else 
                             mkAlt (ABS (NGMINMAX 0 1  
                                        (mkEl matchNull),junk))
                                   (ABS (NGMINMAX x y  n ,junk))
                _ -> n

-- binary nodes
mkCon n1 n2 = ABS (CON n1 n2 ,junk)
mkAlt n1 n2 = ABS (ALT n1 n2 ,junk)


-- add a final node to the tree

addEnd :: AbsTree a b  -- a tree without a final node
       -> AbsTree a b  -- a tree with a final node
addEnd n = ABS (CON n (ABS (END, junk)) ,junk)


-- Interrogate regexps
-- all of type AbsTree a b -> Bool

isElem (ABS (EL _ _, _)) = True
isElem _ = False

isBref (ABS (BACKREF _ _ ,_)) = True
isBref _ = False

isEnd (ABS (END, _)) = True
isEnd _ = False

isSub (ABS (SUB _ _, _)) = True
isSub _ = False

isStar (ABS (STAR _, _)) = True 
isStar _ = False

isPlus (ABS (PLUS _, _)) = True
isPlus _ = False

isOpt (ABS (OPT _, _)) = True
isOpt _ = False

isMin (ABS (MIN _ _, _)) = True
isMin (ABS (NGMIN _ _, _)) = True
isMin _ = False

isMinmax (ABS (MINMAX _ _ _, _)) = True
isMinmax (ABS (NGMINMAX _ _ _, _)) = True
isMinmax _ = False

isNg (ABS (NGMIN _ _, _)) = True
isNg (ABS (NGMINMAX _ _ _, _)) = True
isNg _ = False

isInterval x = isMinmax x || isMin x
isRepeatable x = isMinmax x && snd (getMinMax x) > 1 || isMin x || isPlus x || isStar x

isCon (ABS (CON _ _, _)) = True
isCon _ = False

isAlt (ABS (ALT _ _, _)) = True
isAlt _ = False

isLeaf x = isElem x || isBref x || isEnd x
isUn x = isInterval x || isStar x || isPlus x || isOpt x || isSub x
isBin x = isCon x || isAlt x


-- get contents of regexps

child :: AbsTree a b -> AbsTree a b
child (ABS (SUB _ re, _)) = re
child (ABS (STAR re, _)) = re
child (ABS (PLUS re, _)) = re
child (ABS (OPT re, _)) = re
child (ABS (MIN _ re, _)) = re
child (ABS (NGMIN _ re, _)) = re
child (ABS (MINMAX _ _ re, _)) = re
child (ABS (NGMINMAX _ _ re, _)) = re

left :: AbsTree a b -> AbsTree a b
left (ABS (CON re1 _, _)) = re1 
left (ABS (ALT re1 _, _)) = re1

right :: AbsTree a b -> AbsTree a b
right (ABS (CON _ re2, _)) = re2
right (ABS (ALT _ re2, _)) = re2

-- get the extra info from an AbsTree
getExtraRE :: AbsTree a b -> b
getExtraRE (ABS (_,other)) = other

-- get contents of an element regexps
getElem :: AbsTree a b	  -- a regexp (only an El one allowed)
        -> MatcherImpl a	  -- the matcher function at that leaf
 

getElem (ABS (EL f _, _)) = f

getPrefix :: AbsTree a b -- a regexp (only an El one allowed)
          -> Bool	 -- whether that regexp is a prefix only matcher
getPrefix (ABS (EL _ prefix, _)) = prefix

-- get contents of a backref regexp 
getBref :: AbsTree a b
	-> (String,	      -- which subexp to refer to
	    [a] -> MatcherImpl a) -- the matcher function to use


getBref (ABS (BACKREF x f, _)) = (x,f)


-- get the minimum num interations from an interval regexp
getMin :: AbsTree a b -> Int
getMin (ABS (MIN x _, _)) = x
getMin (ABS (MINMAX x _ _, _)) = x
getMin (ABS (NGMIN x _, _)) = x
getMin (ABS (NGMINMAX x _ _, _)) = x

-- get the minimum & maximum num interations from a minmax regexp
getMinMax :: AbsTree a b -> (Int,Int)
getMinMax (ABS (MINMAX x y _ ,_)) = (x,y)
getMinMax (ABS (NGMINMAX x y _ ,_)) = (x,y)

-- get the name of the subexp to use to reference it
getSEName :: AbsTree a b -> String
getSEName (ABS (SUB s _,_)) = s

-- update an AbsTree

updateExtraRE :: b		-- new bit of extra info 
	      -> AbsTree a b	-- the regexp to be updated
	      -> AbsTree a b	-- the new regexp

updateExtraRE newother (ABS (re,_)) = (ABS (re,newother))


updateChild :: AbsTree a b -> AbsTree a b -> AbsTree a b
updateChild newchild (ABS (MINMAX x y  child ,other))
                     = (ABS (MINMAX x y  newchild ,other))
updateChild newchild (ABS (MIN x  child ,other))
		     = (ABS (MIN x  newchild ,other))
updateChild newchild (ABS (NGMIN x  child ,other))
		     = (ABS (NGMIN x  newchild ,other))
updateChild newchild (ABS (NGMINMAX x  y  child ,other))
		     = (ABS (NGMINMAX x y  newchild ,other))
updateChild newchild (ABS (STAR child ,other)) 
		     = (ABS (STAR newchild ,other))
updateChild newchild (ABS (OPT child ,other))
		     = (ABS (OPT newchild ,other))
updateChild newchild (ABS (PLUS child ,other)) 
		     = (ABS (PLUS newchild ,other))
updateChild newchild (ABS (SUB s child ,other)) 
		     = (ABS (SUB s newchild ,other))

updateLeft :: AbsTree a b -> AbsTree a b -> AbsTree a b
updateLeft newchild (ABS (CON l r ,other))
		     = (ABS (CON newchild r ,other))
updateLeft newchild (ABS (ALT l r ,other))
		     = (ABS (ALT newchild r ,other))


updateRight :: AbsTree a b -> AbsTree a b -> AbsTree a b
updateRight newchild (ABS (CON l r ,other))
		     = (ABS (CON l newchild ,other))
updateRight newchild (ABS (ALT l r ,other))
		     = (ABS (ALT l newchild ,other))




foldPostT :: (AbsTree a b -> c -> c)	-- function to apply to each node
          -> c				-- initial	
          -> AbsTree a b		-- Tree to fold over
          -> c				-- result

foldPostT f a t 
        | isLeaf t = f t a
	| isUn t = f t (foldPostT f a (child t))
	| isBin t = f t (foldPostT f (foldPostT f a (left t)) (right t))

foldPreT  :: (AbsTree a b -> c -> c)	-- function to apply to each node
          -> c				-- initial	
          -> AbsTree a b		-- Tree to fold over
          -> c				-- result

foldPreT f a t 
        | isLeaf t = f t a
	| isUn t = (foldPostT f (f t a) (child t))
	| isBin t =(foldPostT f (foldPostT f (f t a) (left t)) (right t))



works (ABS (EL a x, other)) = (ABS (EL a x, 10))
works (ABS (BACKREF a d, other)) = (ABS (BACKREF a d, 10))
works (ABS (END,_)) = (ABS (END,10))
works (ABS (SUB x c, other)) = (ABS (SUB x (works c), 10))
works (ABS (STAR c,other)) = (ABS (STAR (works c),10))
works (ABS (PLUS c,other)) = (ABS (PLUS (works c),10))
works (ABS (OPT c,other)) = (ABS (OPT (works c),10))
works (ABS (MIN a c,other)) = (ABS (MIN a (works c),10))
works (ABS (NGMIN a c,other)) = (ABS (NGMIN a (works c),10))
works (ABS (MINMAX a b c,other)) = (ABS (MINMAX a b (works c),10))
works (ABS (NGMINMAX a b c,other)) = (ABS (NGMINMAX a b (works c),10))
works (ABS (CON l r,_)) = (ABS (CON (works l) (works r),10))
works (ABS (ALT l r,_)) = (ABS (ALT (works l) (works r),10))
 





