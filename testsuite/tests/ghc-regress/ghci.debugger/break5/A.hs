import GHC.Base (breakpoint, breakpointCond)

f i = breakpointCond (i>3) ()

g :: Int -> ()
g i = breakpoint ()
