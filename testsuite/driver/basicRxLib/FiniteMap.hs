module FiniteMap where

type FiniteMap key elt = [(key,elt)]

-- Building
emptyFM :: FiniteMap key elt
emptyFM = []

unitFM :: key -> elt -> FiniteMap key elt
unitFM key elt = [(key,elt)]

listToFM :: [(key,elt)] -> FiniteMap key elt
listToFM l = l

-- Adding to 

addToFM :: Eq key => FiniteMap key elt -> key -> elt -> FiniteMap key elt
addToFM [] key elt = [(key,elt)]
addToFM ((k,e):xs) key elt = if (key == k) then 
                               (k,elt):xs
                             else
                               (k,e):(addToFM xs key elt)

addToFM_C :: Eq key => (elt -> elt -> elt) 
                  -> FiniteMap key elt -> key -> elt -> FiniteMap key elt

addToFM_C f [] key elt = [(key,elt)]
addToFM_C f ((k,e):xs) key elt = if key == k then 
                                   (k,f e elt):xs
                                 else
                                   (k,e):(addToFM_C f xs key elt)


-- Combining

	-- Bind right argument over left
plusFM :: Eq key => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
plusFM fm1 fm2 = foldr (\(k,e) fm -> addToFM fm k e) fm1 fm2
 
	-- Combines bindings for the same thing with given function
plusFM_C :: Eq key => (elt -> elt -> elt)
                 -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
plusFM_C f fm1 fm2 = foldr (\(k,e) fm -> addToFM_C f fm k e) fm1 fm2

--Interrogating

sizeFM :: FiniteMap key elt -> Int
sizeFM fm = length fm

isEmptyFM :: FiniteMap key elt -> Bool
isEmptyFM [] = True
isEmptyFM (x:xs) = False

elemFM :: Eq key => key -> FiniteMap key elt -> Bool
elemFM key fm = key `elem` (keysFM fm)

lookupFM :: Eq key => FiniteMap key elt -> key -> Maybe elt
lookupFM [] key = Nothing
lookupFM ((k,e):fm) key = if key == k then 
                            Just e
                          else
                            lookupFM fm key



--Mapping,Folding,Filtering

foldFM :: (key -> elt -> a -> a) -> a -> FiniteMap key elt -> a
foldFM f k fm = foldr (\(x,y)-> f x y) k fm 

mapFM :: (key -> elt1 -> elt2) -> FiniteMap key elt1 -> FiniteMap key elt2
mapFM f fm = map (\(x,y) -> (x,f x y)) fm

filterFM :: (key -> elt -> Bool) -> FiniteMap key elt 
         -> FiniteMap key elt
filterFM f fm = filter (\(a,b) -> f a b) fm

-- Listifying
fmToList :: FiniteMap key elt -> [(key,elt)]
fmToList xs = xs

keysFM :: FiniteMap key elt -> [key]
keysFM fm = map fst fm

eltsFM :: FiniteMap key elt -> [elt]
eltsFM fm = map snd fm

