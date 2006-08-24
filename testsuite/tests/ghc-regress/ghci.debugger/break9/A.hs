import GHC.Base (breakpoint, breakpointCond)

f :: Bool -> ()
f i = breakpoint$ breakpoint$ ()

g :: Bool -> ()	
g i = breakpoint$ let j = False in ()

h _ = breakpoint ()

