Main.hs for backprop simulation
Written by Sam Waugh
Date started: 10th November 1992.

This main module initialises, runs and gets results from the 
backpropagation functions and values.

> import BpGen
> import ReadLists (readWhiteList)
> import {-fool mkdependHS-}
>	 Printf

-------------------------------------------------------------------------------
|				Constant Values				      |
-------------------------------------------------------------------------------
The following constants set the training problem and parameters:
  name		- the name of the file
  dimensions	- the layered network topology
  eta		- the learning rate
  accepterr	- the level of error acceptable to stop training
  epochs	- the maximum number of epochs in training

> name		:: String
> name		= "xor"
> dimensions	:: Dimensions
> dimensions	= [2,2,1]
> eta,accepterr	:: Double
> eta		= 1.0
> accepterr	= 0.001
> epochs	:: Int
> epochs	= 10000


-------------------------------------------------------------------------------
|			IO and Main Program				      |
-------------------------------------------------------------------------------

> main = do
>   s <- readFile name
>   putStr (program s "")

> program :: String -> ShowS
> program s
>   = let egs	    = readegs s
> 	  ws	    = randweights dimensions
> 	  rs	    = selectegs (length egs)
> 	  (ws',res) = trainweights egs ws epochs accepterr eta rs
>     in
>     showString "Examples:\n"
>     . showegs egs
>     . showString "Classification:\n"
>     . showresults egs ws
>     . showString "Training Error:\n"
>     . showerr res
>     . showString "Trained Classification:\n"
>     . showresults egs ws'

> {- ORIG:
> program :: String -> String
> program s
>   = _scc_ "program" (
>     let egs	    = _scc_ "readegs" readegs s
> 	  ws	    = _scc_ "randweights" randweights dimensions
> 	  rs	    = _scc_ "selectegs" selectegs (length egs)
> 	  (ws',res) = _scc_ "trainweights" trainweights egs ws epochs accepterr eta rs
>     in "Examples:\n"
>     ++ _scc_ "showegs" showegs egs
>     ++ "Classification:\n"
>     ++ _scc_ "showresults" showresults egs ws
>     ++ "Training Error:\n"
>     ++ _scc_ "showerr" showerr res
>     ++ "Trained Classification:\n"
>     ++ _scc_ "showresults2" showresults egs ws'
>     )
> -}

-------------------------------------------------------------------------------
|				Show Functions				      |
-------------------------------------------------------------------------------

> showdouble :: Double -> ShowS
> showdouble v = showString (printf "%6.4f " [UDouble v])

> showdoubles :: [Double] -> ShowS
> showdoubles []     = showString ""
> showdoubles (v:vs) = showdouble v . showdoubles vs

> showegs :: Egs -> ShowS
> showegs [] = showString "\n"
> showegs ((x,t):egs)
> 	= showdoubles x . showString " " . showdoubles t . showString "\n" . showegs egs

> showresults :: Egs -> Weights -> ShowS
> showresults [] _ = showString "\n"
> showresults ((x,t):egs) ws
>   = let y = last (classeg ws x)
> 	  p = maxplace y
> 	  c = maxplace t
>     in shows p . showString "  " . showdouble (y!!p) . showString "    " .
>        shows c . showString "  " . showdouble (t!!c) . showString "\n" . showresults egs ws

> showerr :: [Double] -> ShowS
> showerr [] = showString ""
> showerr (x:xs) = showerr xs . showdouble x . showString "\n" 

> showweights :: Weights -> ShowS
> showweights [] = showString "\n"
> showweights (w:ws) = showweight w . showweights ws

> showweight, showl :: Weight -> ShowS
> showweight []     = showString "[]\n"
> showweight (x:xs) = showString "[" . showdoubles x . showl xs

> showl []     = showString "]\n"
> showl (x:xs) = showString "\n " . showdoubles x . showl xs

> {- ORIG:
> showdouble :: Double -> String
> showdouble v = printf "%6.4f " [UDouble v]

> showdoubles :: [Double] -> String
> showdoubles []     = ""
> showdoubles (v:vs) = showdouble v ++ showdoubles vs

> showegs :: Egs -> String
> showegs [] = "\n"
> showegs ((x,t):egs)
> 	= (showdoubles x) ++ " " ++ (showdoubles t) ++ "\n" ++ showegs egs

> showresults :: Egs -> Weights -> String
> showresults [] _ = "\n"
> showresults ((x,t):egs) ws
>   = let y = last (classeg ws x)
> 	  p = maxplace y
> 	  c = maxplace t
>     in show p ++ "  " ++ showdouble (y!!p) ++ "    " ++
>        show c ++ "  " ++ showdouble (t!!c) ++ "\n"   ++ showresults egs ws

> showerr :: [Double] -> String
> showerr [] = ""
> showerr (x:xs) = showerr xs ++ showdouble x ++ "\n" 

> showweights :: Weights -> String
> showweights [] = "\n"
> showweights (w:ws) = showweight w ++ showweights ws
> showweight, showl :: Weight -> String
> showweight []     = "[]\n"
> showweight (x:xs) = "["    ++ showdoubles x ++ showl xs
> showl []     = "]\n"
> showl (x:xs) = "\n " ++ showdoubles x ++ showl xs
> -}

-------------------------------------------------------------------------------
| 			Data Reading Functions				      |
-------------------------------------------------------------------------------

> readegs :: String -> Egs
> readegs s = readData (readWhiteList s)

> readData :: [Double] -> Egs
> readData [] = []
> readData bs = let (inp, bs')  = splitAt (head dimensions) bs
> 		    (out, bs'') = splitAt (last dimensions) bs'
> 	        in (inp,out) : (readData bs'')
