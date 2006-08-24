import GHC.Base (breakpoint, breakpointCond)

f i = breakpointCond (i>3) ()
g i = breakpoint ()
