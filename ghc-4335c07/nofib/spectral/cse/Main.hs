-- This is a program to illustrate a simple form of common subexpression
-- elimination ... essentially turning trees into DAGs.  Uses two state
-- monads (more precisely, same monad but different state types).
-- This program doesn't use constructor classes, although it could
-- obviously be modified to fit into that framework.
--
-- This programs should be loaded after `stateMonad':  For example:
--  ? :l stateMonad.gs csexpr.gs
--  ? test
--
-- The output for this `test' is included at the end of the file.
--
-- Mark P. Jones, 1992
--

module Main (main) where

import StateMonad

-- partain: I think this has to be here
infix +=>      -- overide function at single point

-- Data type definitions: ----------------------------------------------------

data GenTree a  = Node a [GenTree a]
type LabGraph a = [ (Label, a, [Label]) ]
type Label      = Int

-- Add distinct (integer) labels to each node of a tree: ---------------------

labelTree   :: GenTree a -> GenTree (Label,a)
labelTree t  = label t `startingWith` 0
               where label (Node x xs) = incr           `bind` \n  ->
                                         mmapl label xs `bind` \ts ->
                                         retURN (Node (n,x) ts)

-- Convert tree after labelling each node to a labelled graph: ---------------

ltGraph                :: GenTree (Label,a) -> LabGraph a
ltGraph (Node (n,x) xs) = (n, x, map labelOf xs) : concat (map ltGraph xs)
                          where labelOf (Node (n,x) xs) = n

-- Build tree from labelled graph: -------------------------------------------

unGraph              :: LabGraph a -> GenTree a
unGraph ((n,x,cs):ts) = Node x (map (unGraph . find) cs)
                        where find c = dropWhile (\(d,_,_) -> c/=d) ts


-- Build tree but avoid duplicating shared parts: ----------------------------

unGraph'     :: LabGraph String -> GenTree (Int,String)
unGraph' lg   = ung lg `startingWith` []
 where ung ((n,x,cs):ts) = mif (visited n)
                                 (retURN (Node (n,"<>") []))
                                 (mmapl (ung . find) cs `bind` \ts ->
                                  retURN (Node (n,x) ts))
                           where find c = dropWhile (\(d,_,_) -> c/=d) ts

visited      :: Label -> SM [Label] Bool
visited n     = fetch                               `bind` \us ->
                if n `elem` us then retURN True
                               else set (n:us)      `bind` \_ -> 
                                    retURN False

-- Find (and eliminate) repeated subtrees in a labelled graph: ---------------
-- Described as a transformation on labelled graphs:  During the calculation
-- we use a pair (r,lg) :: (Label->Label, LabGraph a) where lg contains the
-- simplified portion of the graph calculated so far and r is a renaming (or
-- replacement?) which maps node labels in the original graph to the approp.
-- labels in the new graph.

findCommon :: Eq a => LabGraph a -> LabGraph a
findCommon  = snd . foldr sim (id,[])
 where
   sim ::
     Eq a => (Label,a,[Label]) -> (Label -> Label, LabGraph a) ->
     (Label -> Label, LabGraph a)
   sim (n,s,cs) (r,lg) =
     if null ms then
       (r, [(n,s,rcs)] ++ lg)
     else
       ((n +=> head ms) r, lg)
         where
	   ms  = [m | (m,s',cs')<-lg, s==s', cs'==rcs]
           rcs = map r cs

(+=>) :: Eq a => a -> b -> (a -> b) -> (a -> b)
(+=>) x fx f y  = if x==y then fx else f y

-- Common subexpression elimination: -----------------------------------------

cse :: Eq a => GenTree a -> LabGraph a
cse  = findCommon . ltGraph . labelTree

-- Pretty printers: ----------------------------------------------------------

instance Show a => Show (GenTree a) where
    showsPrec d (Node x ts)
        | null ts   = shows x
        | otherwise = showChar '(' . shows x
                                   . showChar ' '
                                   . (foldr1 (\x y -> x . showChar ' ' . y)
                                             (map shows ts))
                                   . showChar ')'

copy            :: Int -> a -> [a]
copy  n x        = take n (repeat x)
space n          = copy n ' '

drawTree        :: GenTree String -> String
drawTree         = unlines . draw
draw (Node x ts) = grp (s1 ++ pad width x ++ "]") (space (width+3)) (stLoop ts)
 where stLoop []     = [""]
       stLoop [t]    = grp s2 "  " (draw t)
       stLoop (t:ts) = grp s3 s4 (draw t) ++ [s4] ++ rsLoop ts

       rsLoop [t]    = grp s5 "  " (draw t)
       rsLoop (t:ts) = grp s6 s4 (draw t) ++ [s4] ++ rsLoop ts

       grp fst rst   = zipWith (++) (fst:repeat rst)

       -- Define the strings used to print tree diagrams:
       [s1,s2,s3,s4,s5,s6] | pcGraphics = ["\196[", "\196\196", "\196\194",
                                           " \179", " \192",    " \195"]
                           | otherwise  = ["-[",    "--",       "-+",
                                           " |",    " `",       " +"]

       pad n x    = take n (x ++ repeat ' ')
       width      = 4
       pcGraphics = False

showGraph   :: Show a => LabGraph a -> String
showGraph [] = "[]\n"
showGraph xs = "[" ++ loop (map show xs)
               where loop [x]    = x ++ "]\n"
                     loop (x:xs) = x ++ ",\n " ++ loop xs

-- Examples: -----------------------------------------------------------------

plus x y = Node "+" [x,y]
mult x y = Node "*" [x,y]
prod xs  = Node "X" xs
zerO     = Node "0" []
a        = Node "a" []
b        = Node "b" []
c        = Node "c" []
d        = Node "d" []

examples = [example0, example1, example2, example3, example4, example5]
example0 = a
example1 = plus a a
example2 = plus (mult a b) (mult a b)
example3 = plus (mult (plus a b) c) (plus a b)
example4 = prod (scanl plus zerO [a,b,c,d])
example5 = prod (scanr plus zerO [a,b,c,d])

main  = putStr -- writeFile "csoutput"
         (unlines (map (\t -> let c = cse t
                              in  copy 78 '-'            ++
                                  "\nExpression:\n"      ++ show t      ++
                                  "\n\nTree:\n"          ++ drawTree t  ++
                                  "\nLabelled graph:\n"  ++ showGraph c ++
                                  "\nSimplified tree:\n" ++ showCse c)
                       examples))
        where
         showCse                  = drawTree
                                    . mapGenTree (\(n,s) -> show n++":"++s)
                                    . unGraph'
         mapGenTree f (Node x ts) = Node (f x) (map (mapGenTree f) ts)

{-----------------------------------------------------------------------------
Expression:
a

Tree:
-[a   ]

Labelled graph:
[(0,"a",[])]

Simplified tree:
-[0:a ]

------------------------------------------------------------------------------
Expression:
(+ a a)

Tree:
-[+   ]-+-[a   ]
        |
        `-[a   ]

Labelled graph:
[(0,"+",[2, 2]),
 (2,"a",[])]

Simplified tree:
-[0:+ ]-+-[2:a ]
        |
        `-[2:<>]

------------------------------------------------------------------------------
Expression:
(+ (* a b) (* a b))

Tree:
-[+   ]-+-[*   ]-+-[a   ]
        |        |
        |        `-[b   ]
        |
        `-[*   ]-+-[a   ]
                 |
                 `-[b   ]

Labelled graph:
[(0,"+",[4, 4]),
 (4,"*",[5, 6]),
 (5,"a",[]),
 (6,"b",[])]

Simplified tree:
-[0:+ ]-+-[4:* ]-+-[5:a ]
        |        |
        |        `-[6:b ]
        |
        `-[4:<>]

------------------------------------------------------------------------------
Expression:
(+ (* (+ a b) c) (+ a b))

Tree:
-[+   ]-+-[*   ]-+-[+   ]-+-[a   ]
        |        |        |
        |        |        `-[b   ]
        |        |
        |        `-[c   ]
        |
        `-[+   ]-+-[a   ]
                 |
                 `-[b   ]

Labelled graph:
[(0,"+",[1, 6]),
 (1,"*",[6, 5]),
 (5,"c",[]),
 (6,"+",[7, 8]),
 (7,"a",[]),
 (8,"b",[])]

Simplified tree:
-[0:+ ]-+-[1:* ]-+-[6:+ ]-+-[7:a ]
        |        |        |
        |        |        `-[8:b ]
        |        |
        |        `-[5:c ]
        |
        `-[6:<>]

------------------------------------------------------------------------------
Expression:
(X 0 (+ 0 a) (+ (+ 0 a) b) (+ (+ (+ 0 a) b) c) (+ (+ (+ (+ 0 a) b) c) d))

Tree:
-[X   ]-+-[0   ]
        |
        +-[+   ]-+-[0   ]
        |        |
        |        `-[a   ]
        |
        +-[+   ]-+-[+   ]-+-[0   ]
        |        |        |
        |        |        `-[a   ]
        |        |
        |        `-[b   ]
        |
        +-[+   ]-+-[+   ]-+-[+   ]-+-[0   ]
        |        |        |        |
        |        |        |        `-[a   ]
        |        |        |
        |        |        `-[b   ]
        |        |
        |        `-[c   ]
        |
        `-[+   ]-+-[+   ]-+-[+   ]-+-[+   ]-+-[0   ]
                 |        |        |        |
                 |        |        |        `-[a   ]
                 |        |        |
                 |        |        `-[b   ]
                 |        |
                 |        `-[c   ]
                 |
                 `-[d   ]

Labelled graph:
[(0,"X",[21, 20, 19, 18, 17]),
 (17,"+",[18, 25]),
 (18,"+",[19, 24]),
 (19,"+",[20, 23]),
 (20,"+",[21, 22]),
 (21,"0",[]),
 (22,"a",[]),
 (23,"b",[]),
 (24,"c",[]),
 (25,"d",[])]

Simplified tree:
-[0:X ]-+-[21:0]
        |
        +-[20:+]-+-[21:<]
        |        |
        |        `-[22:a]
        |
        +-[19:+]-+-[20:<]
        |        |
        |        `-[23:b]
        |
        +-[18:+]-+-[19:<]
        |        |
        |        `-[24:c]
        |
        `-[17:+]-+-[18:<]
                 |
                 `-[25:d]


------------------------------------------------------------------------------
Expression:
(X (+ a (+ b (+ c (+ d 0)))) (+ b (+ c (+ d 0))) (+ c (+ d 0)) (+ d 0) 0)

Tree:
-[X   ]-+-[+   ]-+-[a   ]
        |        |
        |        `-[+   ]-+-[b   ]
        |                 |
        |                 `-[+   ]-+-[c   ]
        |                          |
        |                          `-[+   ]-+-[d   ]
        |                                   |
        |                                   `-[0   ]
        |
        +-[+   ]-+-[b   ]
        |        |
        |        `-[+   ]-+-[c   ]
        |                 |
        |                 `-[+   ]-+-[d   ]
        |                          |
        |                          `-[0   ]
        |
        +-[+   ]-+-[c   ]
        |        |
        |        `-[+   ]-+-[d   ]
        |                 |
        |                 `-[0   ]
        |
        +-[+   ]-+-[d   ]
        |        |
        |        `-[0   ]
        |
        `-[0   ]

Labelled graph:
[(0,"X",[1, 10, 17, 22, 25]),
 (1,"+",[2, 10]),
 (2,"a",[]),
 (10,"+",[11, 17]),
 (11,"b",[]),
 (17,"+",[18, 22]),
 (18,"c",[]),
 (22,"+",[23, 25]),
 (23,"d",[]),
 (25,"0",[])]

Simplified tree:
-[0:X ]-+-[1:+ ]-+-[2:a ]
        |        |
        |        `-[10:+]-+-[11:b]
        |                 |
        |                 `-[17:+]-+-[18:c]
        |                          |
        |                          `-[22:+]-+-[23:d]
        |                                   |
        |                                   `-[25:0]
        |
        +-[10:<]
        |
        +-[17:<]
        |
        +-[22:<]
        |
        `-[25:<]

-}----------------------------------------------------------------------------
