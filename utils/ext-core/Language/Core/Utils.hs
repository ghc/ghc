module Language.Core.Utils
         (everywhereExcept, everywhereExceptM, noNames, notNull,
             expectJust, fixedPointBy, applyPasses, varsIn, dupsBy,
             everywhere'Except, everywhere'But, wordsBy) where

import Data.Generics
import Data.List
import Data.Maybe
import qualified Data.Set as S

everywhereExcept :: Data a => GenericT -> a -> a
everywhereExcept = everywhereBut (mkQ False (\ (_::String) -> True))

everywhere'Except :: Data a => GenericT -> a -> a
everywhere'Except = everywhere'But (mkQ False (\ (_::String) -> True))

everywhereExceptM :: (Data a, Monad m) => GenericM m -> a -> m a
everywhereExceptM = everywhereButM (mkQ False (\ (_::String) -> True))


noNames :: Data a => r -> (r -> r -> r) -> GenericQ r -> a -> r
noNames e c = everythingBut e c (mkQ False (\ (_::String) -> True))

everythingBut :: r -> (r -> r -> r) -> GenericQ Bool
              -> GenericQ r -> GenericQ r
everythingBut empty combine q q1 x
  | q x         = empty
  | otherwise   = q1 x `combine` 
     (foldl' combine empty
       (gmapQ (everythingBut empty combine q q1) x))

everywhere'But :: GenericQ Bool -> GenericT -> GenericT
-- Guarded to let traversal cease if predicate q holds for x
everywhere'But q f x
    | q x       = x
    | otherwise = let top = gmapT f x in
                    top `seq` (gmapT (everywhere'But q f) top)

everywhereButM :: Monad m => GenericQ Bool -> GenericM m -> GenericM m
everywhereButM q f x
    | q x       = return x
    | otherwise = (gmapM (everywhereButM q f) x) >>= f

notNull :: [a] -> Bool
notNull = not . null

expectJust :: String -> Maybe a -> a
expectJust s = fromMaybe (error s)

fixedPointBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixedPointBy done trans start = go start
  where go v = 
          let next = trans v in
           if done v next then
             next
           else
             go next

applyPasses :: [a -> a] -> a -> a
applyPasses passes p = -- trace ("p = " ++ show p) $ 
  foldl' (\ p' nextF -> nextF p') p passes

varsIn :: (Ord b, Typeable b, Data a) => a -> S.Set b
varsIn = noNames S.empty S.union 
           (mkQ S.empty (\ v -> S.singleton v))

dupsBy :: (a -> a -> Bool) -> [a] -> [a]
dupsBy (~=) xs = filter (\ x -> length (filter (~= x) xs) > 1) xs

wordsBy :: Eq a => a -> [a] -> [[a]]
wordsBy _ []              = [[]]
wordsBy y (x:xs) | y == x = [x]:(wordsBy y xs)
wordsBy y (x:xs)          = 
  case wordsBy y xs of
    (z:zs) -> (x:z):zs
    []     -> [[y]]
