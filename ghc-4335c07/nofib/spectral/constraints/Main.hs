{- Andrew Tolmach and Thomas Nordin's contraint solver

	See Proceedings of WAAAPL '99
-}

import Prelude hiding (Maybe(Just,Nothing))
import Data.List
import System.Environment

-----------------------------
-- The main program
-----------------------------

main = do
 [arg] <- getArgs
 let
	n = read arg :: Int
   	try algorithm = print (length (search algorithm (queens n)))
 sequence_ (map try [bt, bm, bjbt, bjbt', fc])

-----------------------------
-- Figure 1. CSPs in Haskell.
-----------------------------

type Var = Int
type Value = Int

data Assign = Var := Value deriving (Eq, Ord, Show)

type Relation = Assign -> Assign -> Bool

data CSP = CSP { vars, vals :: Int, rel :: Relation } 

type State = [Assign]

level :: Assign -> Var
level (var := val) = var

value :: Assign -> Value
value (var := val) = val

maxLevel :: State -> Var
maxLevel [] = 0
maxLevel ((var := val):_) = var

complete :: CSP -> State -> Bool
complete CSP{vars=vars} s = maxLevel s == vars

generate :: CSP -> [State]
generate CSP{vals=vals,vars=vars} = g vars
  where g 0 = [[]]
        g var = [ (var := val):st | val <- [1..vals], st <- g (var-1) ]

inconsistencies :: CSP -> State -> [(Var,Var)]
inconsistencies CSP{rel=rel} as =  [ (level a, level b) | a <- as, b <- reverse as, a > b, not (rel a b) ]

consistent :: CSP -> State -> Bool
consistent csp = null . (inconsistencies csp)

test :: CSP -> [State] -> [State]
test csp = filter (consistent csp)

solver :: CSP -> [State]
solver csp  = test csp candidates
  where candidates = generate csp

queens :: Int -> CSP
queens n = CSP {vars = n, vals = n, rel = safe}
  where safe (i := m) (j := n) = (m /= n) && abs (i - j) /= abs (m - n)

-------------------------------
-- Figure 2.  Trees in Haskell.
-------------------------------

data Tree a = Node a [Tree a]

label :: Tree a -> a
label (Node lab _) = lab

type Transform a b = Tree a -> Tree b

mapTree  :: (a -> b) -> Transform a b
mapTree f (Node a cs) = Node (f a) (map (mapTree f) cs)

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a cs) = f a (map (foldTree f) cs)

filterTree :: (a -> Bool) -> Transform a a
filterTree p = foldTree f
  where f a cs = Node a (filter (p . label) cs)

prune :: (a -> Bool) -> Transform a a
prune p = filterTree (not . p)

leaves :: Tree a -> [a]
leaves (Node leaf []) = [leaf]
leaves (Node _ cs) = concat (map leaves cs)

initTree :: (a -> [a]) -> a -> Tree a
initTree f a = Node a (map (initTree f) (f a))

--------------------------------------------------
-- Figure 3.  Simple backtracking solver for CSPs.
--------------------------------------------------

mkTree :: CSP -> Tree State
mkTree CSP{vars=vars,vals=vals} = initTree next []
  where next ss = [ ((maxLevel ss + 1) := j):ss | maxLevel ss < vars, j <- [1..vals] ]

data Maybe a = Just a | Nothing deriving Eq

earliestInconsistency :: CSP -> State -> Maybe (Var,Var)
earliestInconsistency CSP{rel=rel} [] = Nothing
earliestInconsistency CSP{rel=rel} (a:as) = 
        case filter (not . rel a) (reverse as) of
          [] -> Nothing
          (b:_) -> Just (level a, level b)

labelInconsistencies :: CSP -> Transform State (State,Maybe (Var,Var))
labelInconsistencies csp = mapTree f 
    where f s = (s,earliestInconsistency csp s)

btsolver0 :: CSP -> [State]
btsolver0 csp =
  (filter (complete csp) . leaves . (mapTree fst) . prune ((/= Nothing) . snd) 
                                            . (labelInconsistencies csp) .  mkTree) csp

-----------------------------------------------
-- Figure 6. Conflict-directed solving of CSPs.
-----------------------------------------------

data ConflictSet = Known [Var] | Unknown deriving Eq

knownConflict :: ConflictSet -> Bool
knownConflict (Known (a:as)) = True
knownConflict _              = False

knownSolution :: ConflictSet -> Bool
knownSolution (Known []) = True
knownSolution _          = False

checkComplete :: CSP -> State -> ConflictSet
checkComplete csp s = if complete csp s then Known [] else Unknown

type Labeler = CSP -> Transform State (State, ConflictSet)

search :: Labeler -> CSP -> [State]
search labeler csp =
  (map fst . filter (knownSolution . snd) . leaves . prune (knownConflict . snd) . labeler csp . mkTree) csp

bt :: Labeler
bt csp = mapTree f
      where f s = (s, 
                   case earliestInconsistency csp s of
                     Nothing    -> checkComplete csp s
                     Just (a,b) -> Known [a,b])

btsolver :: CSP -> [State]
btsolver = search bt

-------------------------------------
-- Figure 7. Randomization heuristic.
-------------------------------------

hrandom :: Int -> Transform a a
hrandom seed (Node a cs) = Node a (randomList seed' (zipWith hrandom (randoms seed') cs))
  where seed' = random seed

btr :: Int -> Labeler
btr seed csp = bt csp . hrandom seed

---------------------------------------------
-- Support for random numbers (not in paper).
---------------------------------------------

random2 :: Int -> Int
random2 n = if test > 0 then test else test + 2147483647
  where test = 16807 * lo - 2836 * hi
        hi   = n `div` 127773
        lo   = n `rem` 127773

randoms :: Int -> [Int]
randoms = iterate random2

random :: Int -> Int
random n = (a * n + c) -- mod m
  where a = 994108973
        c = a

randomList :: Int -> [a] -> [a]
randomList i as = map snd (sortBy (\(a,b) (c,d) -> compare a c) (zip (randoms i) as))

-------------------------
-- Figure 8. Backmarking.
-------------------------

type Table = [Row]       -- indexed by Var
type Row = [ConflictSet] -- indexed by Value

bm :: Labeler
bm csp = mapTree fst . lookupCache csp . cacheChecks csp (emptyTable csp)

emptyTable :: CSP -> Table
emptyTable CSP{vars=vars,vals=vals} = []:[[Unknown | m <- [1..vals]] | n <- [1..vars]]

cacheChecks :: CSP -> Table -> Transform State (State, Table)
cacheChecks csp tbl (Node s cs) =
  Node (s, tbl) (map (cacheChecks csp (fillTable s csp (tail tbl))) cs)

fillTable :: State -> CSP -> Table -> Table
fillTable [] csp tbl = tbl
fillTable ((var' := val'):as) CSP{vars=vars,vals=vals,rel=rel} tbl =
    zipWith (zipWith f) tbl [[(var,val) | val <- [1..vals]] | var <- [var'+1..vars]]
          where f cs (var,val) = if cs == Unknown && not (rel (var' := val') (var := val)) then 
                                   Known [var',var] 
                                 else cs

lookupCache :: CSP -> Transform (State, Table) ((State, ConflictSet), Table)
lookupCache csp t = mapTree f t
  where f ([], tbl)      = (([], Unknown), tbl)
        f (s@(a:_), tbl) = ((s, cs), tbl) 
	     where cs = if tableEntry == Unknown then checkComplete csp s else tableEntry
                   tableEntry = (head tbl)!!(value a-1)

--------------------------------------------
-- Figure 10. Conflict-directed backjumping.
--------------------------------------------

bjbt :: Labeler
bjbt csp = bj csp . bt csp

bjbt' :: Labeler
bjbt' csp = bj' csp . bt csp

bj :: CSP -> Transform (State, ConflictSet) (State, ConflictSet)
bj csp = foldTree f 
  where f (a, Known cs) chs = Node (a,Known cs) chs
        f (a, Unknown)  chs = Node (a,Known cs') chs
          where cs' = combine (map label chs) []

combine :: [(State, ConflictSet)] -> [Var] -> [Var]
combine []                 acc = acc 
combine ((s, Known cs):css) acc =
  if maxLevel s `notElem` cs then cs else combine css (cs `union` acc)

bj' :: CSP -> Transform (State, ConflictSet) (State, ConflictSet)
bj' csp = foldTree f 
  where f (a, Known cs) chs = Node (a,Known cs) chs
        f (a, Unknown) chs = if knownConflict cs' then Node (a,cs') [] else Node (a,cs') chs
           where cs' = Known (combine (map label chs) [])

-------------------------------
-- Figure 11. Forward checking.
-------------------------------

fc :: Labeler
fc csp = domainWipeOut csp . lookupCache csp . cacheChecks csp (emptyTable csp)

collect :: [ConflictSet] -> [Var]
collect [] = []
collect (Known cs:css) = cs `union` (collect css)

domainWipeOut :: CSP -> Transform ((State, ConflictSet), Table) (State, ConflictSet)
domainWipeOut CSP{vars=vars} t = mapTree f t
  where f ((as, cs), tbl) = (as, cs')  
          where wipedDomains = ([vs | vs <- tbl, all (knownConflict) vs]) 
                cs' = if null wipedDomains then cs else Known (collect (head wipedDomains))




