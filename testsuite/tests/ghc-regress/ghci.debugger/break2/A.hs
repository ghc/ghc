import GHC.Base (breakpoint, breakpointCond)


g :: Int -> ()
g i = ()
      where a = False
 	    b = True
            c = breakpoint ()
     