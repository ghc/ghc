module AugmentTree 
  (augmentT)
where


import FiniteMap
import OrdList
import BagRE
import AbsTreeDefs
import AugTreeDefs
import ConstructorMonad

-- Augment the abstract syntax tree to contain extra info

augmentT :: AugTree a -> (AugTree a,Int)
augmentT t = initLbl (mapCM augM t) 1

augM :: AugTree a -> LblM (AugTree a)
augM t      | isLeaf t = getNewLbl `thenLbl` \name ->
			 returnLbl (doAug (Just name) t) 
	    | otherwise = returnLbl (doAug Nothing t)


doAug :: Maybe Int -> AugTree a -> AugTree a
doAug x t 
    | isLeaf t = 
	let (Just n) = x
	in		   
          updateAugTree x
		       (singletonOL n,singletonOL n,singletonOL n,([],[]))
		       False
		       t

    | isUn t = let (AUGINFO (fst,lst,chs,(flwNI,flwI)) nullable) = getAugInfo (child t)
                   followers = if isRepeatable t && isInterval t then
                                  (flwNI,
                                   foldS (update fst) flwI lst )
                               else 
                                 if isRepeatable t && (not (isInterval t)) then
                                  (foldS (updateNoInt fst) flwNI lst,
                                   foldS (update fst) flwI lst)
                               else
                                  (flwNI,flwI)

                   auginfo = (fst,lst,chs,followers)  
	       in
		      if isInterval t then
                        updateAugTree x auginfo (nullable || (getMin t == 0)) t
                      else if isOpt t || isStar t then
			updateAugTree x auginfo True t
		      else 
			updateAugTree x auginfo nullable t

    | isBin t = 
       let
          (AUGINFO (fst1,lst1,chs1,(flw1a,flw1b)) null1) = getAugInfo (left t) 
          (AUGINFO (fst2,lst2,chs2,(flw2a,flw2b)) null2) = getAugInfo (right t)

       in
          if isCon t then
                 let 
                     fstn = if null1 then 
                                 unionOL fst1 fst2
                            else    
                                  fst1
                     lstn = if null2 then
                                  unionOL lst1 lst2
                            else
                                  lst2
                     nulln = null1 && null2

                     followers = 
                       (foldS (updateNoInt (firstPos (right t)))
                              (flw1a++flw2a)
                              (lastPos (left t))
                        ,
                        foldS (update (firstPos (right t)))
                              (flw1b++flw2b)
                              (lastPos (left t))
                       )

                 in
                    updateAugTree x (fstn,lstn,unionOL chs1 chs2,followers) nulln t


          else -- is an "ALT" node 
            let followers = (flw1a++flw2a,flw1b++flw2b)
            in
            updateAugTree x (unionOL fst1 fst2, unionOL lst1 lst2, unionOL chs1 chs2,followers)
                          (null1 || null2) t 
    


updateNoInt :: OrdList Int	 -- followers 
	  -> Int		 -- node to follow
          -> [(Int,BagRE Int)] -- current follow info
          -> [(Int,BagRE Int)] -- updated follow info

updateNoInt newnodes node [] = [(node,listToBagRE newnodes)]
updateNoInt newnodes node ((cn,cfollows):xs) = 
                if cn == node then
                   (cn,cfollows `unionBagRE` (listToBagRE newnodes)):xs
                else if cn < node then
                   (cn,cfollows):(updateNoInt newnodes node xs)
                else
                   (node, listToBagRE newnodes):(cn,cfollows):xs
                     

update :: OrdList Int	-- followers 
       -> Int		-- node to follow
       -> [(Int,OrdList Int)] -- current follow info
       -> [(Int,OrdList Int)] -- updated follow info

update newnodes node [] = [(node,newnodes)]
update newnodes node ((cn,cfollows):xs) = 
                if cn == node then
                   (cn,cfollows `unionOL` newnodes):xs
                else if cn < node then
                   (cn,cfollows):(update newnodes node xs)
                else
                   (node,newnodes):(cn,cfollows):xs
                     


