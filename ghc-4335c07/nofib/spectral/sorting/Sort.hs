module Sort where
import Data.List (partition)
-- trying various sorts

quickSort :: Ord a => [a] -> [a]
quickSort2 :: Ord a => [a] -> [a]
quickerSort :: Ord a => [a] -> [a]
insertSort :: Ord a => [a] -> [a]
treeSort :: Ord a => [a] -> [a]
treeSort2 :: Ord a => [a] -> [a]
heapSort :: Ord a => [a] -> [a]
mergeSort :: Ord a => [a] -> [a]

quickSort []	 = []
quickSort (x:xs) = (quickSort lo) ++ (x : quickSort hi)
    where
	lo = [ y | y <- xs, y <= x ]
	hi = [ y | y <- xs, y >  x ]

-- the same thing, w/ "partition" [whose implementation I don't trust]
quickSort2 []	 = []
quickSort2 (x:xs) = (quickSort2 lo) ++ (x : quickSort2 hi)
    where
	(lo, hi) = partition ((>=) x) xs

-- tail-recursive, etc., "quicker sort" [as per Meira thesis]
quickerSort []	    = []
quickerSort [x]	    = [x]
quickerSort (x:xs)  = split x [] [] xs
  where
    split x lo hi []	 = quickerSort lo ++ (x : quickerSort hi)
    split x lo hi (y:ys) | y <= x  = split x (y:lo) hi ys
			 | True	   = split x lo (y:hi) ys

-------------------------------------------------------------
-- as per Meira thesis

insertSort []	    = []
insertSort (x:xs)   = trins [] [x] xs
  where
    trins :: Ord a => [a] -> [a] -> [a] -> [a]

    trins rev []     (y:ys)	    = trins [] ((reverse rev) ++ [y]) ys
    trins rev xs     []		    = (reverse rev) ++ xs
    trins rev (x:xs) (y:ys) | x < y = trins (x:rev) xs (y:ys)
			    | True  = trins [] (reverse rev ++ (y:x:xs)) ys

-------------------------------------------------------------
-- again, as per Meira thesis

data Tree a = Tip | Branch a (Tree a) (Tree a) deriving ()

treeSort = readTree . mkTree
  where
    mkTree :: Ord a => [a] -> Tree a
    mkTree = foldr to_tree Tip
      where
	to_tree :: Ord a => a -> Tree a -> Tree a
	to_tree x Tip			   = Branch x Tip Tip
    	to_tree x (Branch y l r) | x <= y  = Branch y (to_tree x l) r
				 | True	   = Branch y l (to_tree x r)

    readTree :: Ord a => Tree a -> [a]
    readTree Tip	    = []
    readTree (Branch x l r) = readTree l ++ (x : readTree r)

-- try it w/ bushier trees

data Tree2 a = Tip2 | Twig2 a | Branch2 a (Tree2 a) (Tree2 a) deriving ()

treeSort2 = readTree . mkTree
  where
    mkTree :: Ord a => [a] -> Tree2 a
    mkTree = foldr to_tree Tip2
      where
	to_tree :: Ord a => a -> Tree2 a -> Tree2 a
	to_tree x Tip2			   = Twig2 x
    	to_tree x (Twig2 y)	  | x <= y = Branch2 y (Twig2 x) Tip2
				  | True   = Branch2 y Tip2 (Twig2 x)
    	to_tree x (Branch2 y l r) | x <= y = Branch2 y (to_tree x l) r
				  | True   = Branch2 y l (to_tree x r)

    readTree :: Ord a => Tree2 a -> [a]
    readTree Tip2	     = []
    readTree (Twig2 x)	     = [x]
    readTree (Branch2 x l r) = readTree l ++ (x : readTree r)

-------------------------------------------------------------
-- ditto, Meira thesis

heapSort xs = clear (heap (0::Int) xs)
  where
    heap :: Ord a => Int -> [a] -> Tree a
    heap k [] = Tip
    heap k (x:xs) = to_heap k x (heap (k+(1::Int)) xs)

    to_heap :: Ord a => Int -> a -> Tree a -> Tree a
    to_heap k x Tip				 = Branch x Tip Tip
    to_heap k x (Branch y l r) | x <= y && odd k = Branch x (to_heap (div2 k) y l) r
			       | x <= y		 = Branch x l (to_heap (div2 k) y r)
			       | odd k		 = Branch y (to_heap (div2 k) x l) r
			       | True		 = Branch y l (to_heap (div2 k) x r)

    clear :: Ord a => Tree a -> [a]
    clear Tip	     = []
    clear (Branch x l r) = x : clear (mix l r)

    mix :: Ord a => Tree a -> Tree a -> Tree a
    mix Tip r = r
    mix l Tip = l
    mix t1@(Branch x l1 r1) t2@(Branch y l2 r2) | x <= y = Branch x (mix l1 r1) t2
						| True   = Branch y t1 (mix l2 r2)

    div2 :: Int -> Int
    div2 k = k `div` 2

-------------------------------------------------------------
-- ditto, Meira thesis

mergeSort = merge_lists . (runsplit [])
  where
    runsplit :: Ord a => [a] -> [a] -> [[a]]
    runsplit []     []	    = []
    runsplit run    []	    = [run]
    runsplit []     (x:xs)  = runsplit [x] xs
    runsplit [r]       (x:xs) | x >  r = runsplit [r,x] xs
    runsplit rl@(r:rs) (x:xs) | x <= r = runsplit (x:rl) xs
			      | True   = rl : (runsplit [x] xs)

    merge_lists :: Ord a => [[a]] -> [a]
    merge_lists []	 = []
    merge_lists (x:xs)   = merge x (merge_lists xs)

    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge xl@(x:xs) yl@(y:ys) | x == y = x : y : (merge xs ys)
			      | x <  y = x : (merge xs yl)
			      | True   = y : (merge xl ys)

