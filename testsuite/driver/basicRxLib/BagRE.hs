module BagRE where

{- 
	a Bag module, also offering minusBag, based on finite map.
-}

import FiniteMap
import OrdList

type BagRE a = FiniteMap a Int


emptyBagRE :: BagRE a 
emptyBagRE = emptyFM

unitBagRE :: elt -> BagRE elt
unitBagRE x = unitFM x 1

unionBagRE :: Ord elt => BagRE elt -> BagRE elt -> BagRE elt
unionBagRE b1 b2 = plusFM_C (+) b1 b2

listToBagRE :: Ord elt => [elt] -> BagRE elt
listToBagRE xs = foldr (\x fm -> addToFM_C (+) fm x 1) emptyFM xs 

bagREToList :: Ord elt => BagRE elt -> [elt]
bagREToList b = foldFM (\k el xs -> xs ++ take el (repeat k)) [] b

elemBagRE :: Ord elt => elt -> BagRE elt -> Bool 
elemBagRE e b = maybeToBool (lookupFM b e)
     where
          maybeToBool (Just x) = True
          maybeToBool Nothing = False

filterBagRE :: Ord elt => (elt -> Bool) -> BagRE elt -> BagRE elt
filterBagRE f b = filterFM (\ a b -> f a) b

minusBagRE :: Ord elt => BagRE elt -> BagRE elt -> BagRE elt
minusBagRE b1 b2 = (filterFM (\ a b -> b > 0)) (plusFM_C (-) b1 b2)

bagREToOL ::  Ord elt => BagRE elt -> OrdList elt
bagREToOL = (listToOL.keysFM)


