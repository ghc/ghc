import GHC.Base (breakpoint, breakpointCond)


g :: Int -> ()
g i = let a = False
 	  b = True
          c = breakpoint ()
      in ()
