\begin{code}
module Main where

import Mandel
import PortablePixmap
import System.IO

main =  hSetBinaryMode stdout True >>
        getContents >>=                                 \ userInput      ->
        readNum "Enter min x  = " (lines userInput) $   \ minx input     ->
        readNum "Enter min y  = " input $               \ miny input     ->
        readNum "Enter max x  = " input $               \ maxx input     ->
        readNum "Enter max y  = " input $               \ maxy input     ->
        readNum "Screen width = " input $               \ screenX input  ->
        readNum "Screen height= " input $               \ screenY input  ->
        readNum "Screen depth = " input $               \ limit _        ->
        putStr (show (mandelset minx miny maxx maxy screenX screenY limit))

readNum::(Num a, Read a) => String -> [String] -> (a->[String]->IO ()) -> IO ()
readNum prompt inputLines succ
   = hPutStr stderr prompt >>
     case inputLines of
       (x:xs) -> case (reads x) of
                   [(y,"")] -> succ y xs
                   _        -> hPutStr stderr "Error - retype the number\n" >>
                               readNum prompt xs succ
       _        -> hPutStr stderr "Early EOF"

{-
Enter min x  = -1.5
Enter min y  = -1.0
Enter max x  = 0.5
Enter max y  = 1.0
-}
\end{code}


