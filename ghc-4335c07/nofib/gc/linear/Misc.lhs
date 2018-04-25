\section{Misc}

 ==============================================================
 ============= A BUNCH OF MISCELLANEOUS FUNCTIONS =============

     Written by Brian D. Moe (Summer 1990).
 ==============================================================

\begin{code}

module Misc where

takeuntil :: (a ->Bool) -> [a] -> [a]

    {- A variant of takewhile and dropwhile. -}


takeuntil p [] = []
takeuntil p (x:xs) 
                  | p x = [x]
                  | otherwise  = x:(takeuntil p xs)


inrange :: Int -> Int -> Int -> Bool
inrange low high x = (low<=x)&&(x<=high)

forceVec :: [Float] -> [Float]
forceVec v = 
    case (f v) of
    True -> v
    where
    f [] = True
    f (x:xs) = case (forceFloat x) of
                True -> case (f xs) of
                         True -> True

forceInt :: Int -> Bool
forceInt 0 = True
forceInt x = True

forceFloat :: Float -> Bool
forceFloat 0 = True
forceFloat x = True

forceMat :: [[Float]] -> [[Float]]
forceMat m = map forceVec m

\end{code}


 count :: * -> Num

    Evaluate a function and return 0.

    This is useful to determine just how much work it takes
    to fully evaluate an expression without going through the
    pain of watching it spew across the screen.

    Of course, this is kind of useless unless the /count
    switch is turned on.

   count f = seq (force f) 0


  ==================================================================

\begin{code}

type Runitem = ([Char],[Char])

mkrunitem :: [Char] -> [Char] -> Runitem
mkrunitem output label = (output,label)

run :: Runitem -> [Char]
run (output,label)
   = "START" ++ output ++ "\n" ++ "END"
    
\end{code}

  ==================================================================
