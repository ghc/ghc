module ConstructorMonad 
       (LblM,
        initLbl,
        returnLbl,
        getNewLbl,
        thenLbl,
        thenLbl_,
        foldPostM,foldPreM,mapCM,foldlM
       )

where


import FiniteMap
import AbsTreeDefs

infixr 9 `thenLbl_`, `thenLbl`

{- This monad is used to allow easy inclusion of
   a name supply for the compileRegexp functions -}
 
type LblM a   =  Int		 -- name supply
              -> (a,		 -- result of the LblM action
                  Int)		 -- the remaining name supply

-- run the Lbl thing and return result, and new name supply 
-- (number of names used = new_name_supply - 1)
initLbl :: LblM a -> Int -> (a,Int)
initLbl act ns = case (act ns) of
                  (result,new_ns) -> (result,new_ns-1)

-- make a LblM thing from a thing
returnLbl :: a -> LblM a 
returnLbl thing ns  = (thing,ns)
        
-- get a new name from the name supply
getNewLbl :: LblM Int 
getNewLbl ns = (ns,ns+1)

-- sequence two actions; the second uses the result of the first
thenLbl :: LblM a  -> (a -> LblM c ) -> LblM c 
thenLbl act1 act2 ns  
    = case (act1 ns ) of
           (result,newns) -> act2 result newns

-- sequence two actions; the second doesn't care about the result of
-- the first
thenLbl_ :: LblM a  -> LblM c  -> LblM c 
thenLbl_ act1 act2 ns 
    = case (act1 ns ) of
           (_,newns) -> act2 newns 


foldPostM :: (AbsTree a b -> c -> LblM c) -> c -> AbsTree a b -> LblM c

foldPostM f res re | isLeaf re = f re res
foldPostM f res re | isUn re = foldPostM f res (child re) `thenLbl` \res1 ->
                               f re res1
foldPostM f res re | isBin re = foldPostM f res (left re) `thenLbl` \res1 ->
                                foldPostM f res1 (right re) `thenLbl` \res2 ->
                                f re res2


foldPreM :: (AbsTree a b -> c -> LblM c) -> c -> AbsTree a b -> LblM c

foldPreM f res re | isLeaf re = f re res
foldPreM f res re | isUn re =  f re res `thenLbl` \res1 -> 
                               foldPreM f res1 (child re) 
foldPreM f res re | isBin re =  f re res `thenLbl` \res1 ->
				foldPreM f res1 (left re) `thenLbl` \res2 ->
                                foldPreM f res2 (right re) 


mapCM :: (AbsTree a b -> LblM (AbsTree a b)) -> AbsTree a b 
     -> LblM (AbsTree a b)
mapCM f t | isLeaf t = f t 
	 | isUn t = mapCM f (child t) `thenLbl` \res1 ->
                    f (updateChild res1 t)
         | isBin t = mapCM f (left t) `thenLbl` \res1 ->
                     mapCM f (right t) `thenLbl` \res2 ->
                     f (updateLeft res1 (updateRight res2 t))


foldrM :: (a -> b -> LblM b) -> b -> [a] -> LblM b
foldrM f a [] = returnLbl a
foldrM f a (x:xs) = foldrM f a xs `thenLbl` \res ->
                    f x res    



foldlM :: (a -> b -> LblM b) -> b -> [a] -> LblM b
foldlM f a [] = returnLbl a
foldlM f a (x:xs) = f x a `thenLbl` \res ->
                    foldlM f res xs 

