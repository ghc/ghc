








module GenUtils (

	trace,

      assocMaybe, assocMaybeErr,
      arrElem,
      arrCond,
      memoise,
      Maybe(..),
      MaybeErr(..),
      mapMaybe,
      mapMaybeFail,
      maybeToBool,
      maybeToObj,
      maybeMap,
      joinMaybe,
      mkClosure,
      foldb,

      mapAccumL,

      sortWith,
      sort,
      cjustify,
      ljustify,
      rjustify,
      space,
      copy,
      combinePairs,
      formatText ) where

import Data.Array -- 1.3
import Data.Ix    -- 1.3

import Debug.Trace ( trace )


-- -------------------------------------------------------------------------

-- Here are two defs that everyone seems to define ... 
-- HBC has it in one of its builtin modules

#if defined(__GLASGOW_HASKELL__) || defined(__GOFER__)

--in 1.3: data Maybe a = Nothing | Just a deriving (Eq,Ord,Text)

#endif

infix 1 =: -- 1.3
type Assoc a b = (a,b) -- 1.3
(=:) a b = (a,b)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (a:r) = case f a of
                      Nothing -> mapMaybe f r
                      Just b  -> b : mapMaybe f r

-- This version returns nothing, if *any* one fails.

mapMaybeFail f (x:xs) = case f x of
			Just x' -> case mapMaybeFail f xs of
				    Just xs' -> Just (x':xs')
				    Nothing -> Nothing
			Nothing -> Nothing
mapMaybeFail f [] = Just []

maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = True
maybeToBool _        = False

maybeToObj  :: Maybe a -> a
maybeToObj (Just a) = a
maybeToObj _        = error "Trying to extract object from a Nothing"

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just a) = Just (f a)
maybeMap f Nothing  = Nothing


joinMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a 
joinMaybe _ Nothing  Nothing  = Nothing
joinMaybe _ (Just g) Nothing  = Just g
joinMaybe _ Nothing  (Just g) = Just g
joinMaybe f (Just g) (Just h) = Just (f g h)

data MaybeErr a err = Succeeded a | Failed err deriving (Eq,Show{-was:Text-})

-- @mkClosure@ makes a closure, when given a comparison and iteration loop. 
-- Be careful, because if the functional always makes the object different, 
-- This will never terminate.

mkClosure :: (a -> a -> Bool) -> (a -> a) -> a -> a
mkClosure eq f = match . iterate f
  where
      match (a:b:c) | a `eq` b = a
      match (_:c)              = match c

-- fold-binary.
-- It combines the element of the list argument in balanced mannerism.

foldb :: (a -> a -> a) -> [a] -> a
foldb f [] = error "can't reduce an empty list using foldb"
foldb f [x] = x
foldb f l  = foldb f (foldb' l)
   where 
      foldb' (x:y:x':y':xs) = f (f x y) (f x' y') : foldb' xs
      foldb' (x:y:xs) = f x y : foldb' xs
      foldb' xs = xs

-- Merge two ordered lists into one ordered list. 

mergeWith               :: (a -> a -> Bool) -> [a] -> [a] -> [a] 
mergeWith _ []     ys      = ys
mergeWith _ xs     []      = xs
mergeWith le (x:xs) (y:ys)
       | x `le` y  = x : mergeWith le xs (y:ys)
       | otherwise = y : mergeWith le (x:xs) ys

insertWith              :: (a -> a -> Bool) -> a -> [a] -> [a]
insertWith _ x []          = [x]
insertWith le x (y:ys)
       | x `le` y     = x:y:ys
       | otherwise    = y:insertWith le x ys

-- Sorting is something almost every program needs, and this is the
-- quickest sorting function I know of.

sortWith :: (a -> a -> Bool) -> [a] -> [a]
sortWith le [] = []
sortWith le lst = foldb (mergeWith le) (splitList lst)
  where
      splitList (a1:a2:a3:a4:a5:xs) = 
               insertWith le a1 
              (insertWith le a2 
              (insertWith le a3
              (insertWith le a4 [a5]))) : splitList xs
      splitList [] = []
      splitList (r:rs) = [foldr (insertWith le) [r] rs]

sort :: (Ord a) => [a] -> [a]
sort = sortWith (<=)

-- Gofer-like stuff:

cjustify, ljustify, rjustify :: Int -> String -> String
cjustify n s = space halfm ++ s ++ space (m - halfm)
               where m     = n - length s
                     halfm = m `div` 2
ljustify n s = s ++ space (max 0 (n - length s))
rjustify n s = space (max 0 (n - length s)) ++ s

space       :: Int -> String
space n      = copy n ' '

copy  :: Int -> a -> [a]      -- make list of n copies of x
copy n x = take n xs where xs = x:xs

combinePairs :: (Ord a) => [(a,b)] -> [(a,[b])]
combinePairs xs = 
	combine [ (a,[b]) | (a,b) <- sortWith (\ (a,_) (b,_) -> a <= b) xs]
 where
	combine [] = []
	combine ((a,b):(c,d):r) | a == c = combine ((a,b++d) : r)
	combine (a:r) = a : combine r

assocMaybe :: (Eq a) => [(a,b)] -> a -> Maybe b
assocMaybe env k = case [ val | (key,val) <- env, k == key] of
               [] -> Nothing
               (val:vs) -> Just val

assocMaybeErr :: (Eq a) => [(a,b)] -> a -> MaybeErr b String
assocMaybeErr env k = case [ val | (key,val) <- env, k == key] of
                       [] -> Failed "assoc: "
                       (val:vs) -> Succeeded val


deSucc (Succeeded e) = e

mapAccumL :: (a -> b -> (c,a)) -> a -> [b] -> ([c],a)
mapAccumL f s [] = ([],s)
mapAccumL f s (b:bs) = (c:cs,s'')
	where
		(c,s') = f s b
		(cs,s'') = mapAccumL f s' bs



-- Now some utilties involving arrays.
-- Here is a version of @elem@ that uses partual application
-- to optimise lookup.

arrElem :: (Ix a) => [a] -> a -> Bool
arrElem obj = \x -> inRange size x && arr ! x 
  where
      size = (maximum obj,minimum obj)
      arr = listArray size [ i `elem` obj | i <- range size ]

-- Here is the functional version of a multi-way conditional,
-- again using arrays, of course. Remember @b@ can be a function !
-- Note again the use of partiual application.

arrCond :: (Ix a) 
        => (a,a)                      -- the bounds
        -> [(Assoc [a] b)]            -- the simple lookups
        -> [(Assoc (a -> Bool) b)]    -- the functional lookups
        -> b                          -- the default
        -> a -> b                     -- the (functional) result

arrCond bds pairs fnPairs def = (!) arr'
  where
      arr' = array bds [ t =: head
                      ([ r | (p, r) <- pairs, elem t p ] ++
                       [ r | (f, r) <- fnPairs, f t ] ++
                       [ def ])
              | t <- range bds ]

memoise :: (Ix a) => (a,a) -> (a -> b) -> a -> b
memoise bds f = (!) arr
  where arr = array bds [ t =: f t | t <- range bds ]

-- Quite neat this. Formats text to fit in a column.

formatText :: Int -> [String] -> [String]
formatText n = map unwords . cutAt n []
  where
	cutAt :: Int -> [String] -> [String] -> [[String]]
	cutAt m wds [] = [reverse wds]
	cutAt m wds (wd:rest) = if len <= m || null wds
			        then cutAt (m-(len+1)) (wd:wds) rest 
				else reverse wds : cutAt n [] (wd:rest)
	  where	len = length wd



