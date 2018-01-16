\begin{code}
module Main where

import Mandel
import PortablePixmap
import System.IO
import System.Environment

main = 	do
  (minx_: miny_: maxx_: maxy_: screenX_ : screenY_ : limit_ : _) <- getArgs

  let [minx,miny,maxx,maxy] = map read [minx_,miny_,maxx_,maxy_]
      [screenX,screenY]     = map read [screenX_,screenY_]
      limit                 = read limit_

  hSetBinaryMode stdout True
  putStr (show (mandelset minx miny maxx maxy screenX screenY limit))

readNum::(Num a, Read a) => String -> [String] -> (a->[String]->IO ()) -> IO ()
readNum prompt inputLines succ
   = hPutStr stderr prompt >>
     case inputLines of
       (x:xs) -> case (reads x) of
		   [(y,"")] -> succ y xs
		   _	    -> hPutStr stderr "Error - retype the number\n" >>
			       readNum prompt xs succ
       _ 	-> hPutStr stderr "Early EOF"

{-
Enter min x  = -1.5
Enter min y  = -1.0
Enter max x  = 0.5
Enter max y  = 1.0
-}
\end{code}


