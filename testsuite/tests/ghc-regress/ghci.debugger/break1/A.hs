import GHC.Base (breakpoint, breakpointCond)


g i = let a = i + 1
 	  b = id
          c = ()
          d = (+)
      in breakpoint ()
