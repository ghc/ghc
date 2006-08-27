import GHC.Base (breakpoint, breakpointCond)

constant = ()

g :: Bool -> ()	
g i = let 
	j = False 
	in ()

h i = case i of
	False -> 0
	True  -> 1
	
j i = do 
	 return ()
