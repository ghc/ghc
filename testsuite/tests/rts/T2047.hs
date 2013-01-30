module Main where

import qualified Data.Set as Set
import Control.Monad
import Data.List

---
---
---

data Direction = DirUp | DirLeft | DirRight | DirDown
    deriving (Eq,Ord,Show,Read)

directions = [DirUp,DirLeft,DirRight,DirDown]

coordOffset DirUp = (-1,0)
coordOffset DirLeft = (0,-1)
coordOffset DirRight = (0,1)
coordOffset DirDown = (1,0)

move (r,c) d = (r+dr,c+dc) where (dr,dc) = coordOffset d

sortPair (x,y) =
    case compare x y of
        EQ -> (x,y)
        LT -> (x,y)
        GT -> (y,x)
mapPair12 f (x,y) = (f x,f y)

cachedUsingList f = f'
    where
        list = map f [0..]
        f' i = list !! i

nubSorted [] = []
nubSorted (x:xs) = nubSorted' x xs
    where
        nubSorted' x [] = [x]
        nubSorted' x (y:ys)
            | x == y  = nubSorted' x ys
            | otherwise  = x : nubSorted' y ys

---
---
---

size = 21
largestExplicitlyEnumeratedArea = 7

type Cell = (Int,Int)
type Edge = (Cell,Cell)

mkEdge cell1 cell2 = sortPair (cell1,cell2)

cellsAround area = nubSorted $ sort $ 
    do
        cell <- area
        dir <- directions
        let cell2 = move cell dir
        guard $ cell2 `notElem` area
        return $ cell2

increaseAreas areas = nubSorted $ sort $ 
    do
        area <- areas
        cell2 <- cellsAround area
        return $ sort $ cell2 : area
getAreas :: Int -> [[Cell]]
getAreasRaw 1 = [[(0,0)]]
getAreasRaw n = areas
    where
        areas = increaseAreas $ getAreas $ n - 1
getAreas = cachedUsingList getAreasRaw

getEdges area = mapPair12 (map snd) $ partition fst $ nubSorted $ sort $ 
    do
        cell <- area
        dir <- directions
        let cell2 = move cell dir
        let isInternal = cell2 `elem` area
        return (isInternal,mkEdge cell cell2)

type SizedArea = (Int,((Set.Set Cell,Set.Set Cell),(Set.Set Edge,Set.Set Edge)))
getExtendedAreas n = 
    do
        area <- getAreas n
        let areaAround = cellsAround area
        let edgeInfo = getEdges area
        return ((Set.fromList area,Set.fromList areaAround),mapPair12 Set.fromList edgeInfo)        

getSizedAreasThrough :: Int -> [SizedArea]
getSizedAreasThrough n = 
    do
        n' <- [1 .. n]
        extendedArea <- getExtendedAreas n'
        return $ (n',extendedArea)

sizeForSizedArea (asize,_) = asize
allSizedAreas = getSizedAreasThrough largestExplicitlyEnumeratedArea

main = print $ allSizedAreas

