import GHC.Base (breakpoint, breakpointCond)

g :: Bool -> ()	
g i = let 
	j = False 
	in ()

h _ = breakpoint ()

