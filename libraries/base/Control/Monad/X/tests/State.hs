import Control.Monad.X.State


t1    = test "x" get ("x","x")
t2    = test "x" (put "y") ((),"y")
t3    = test "x"
       (do x <- get
           put "y"
           y <- get
           return (x,y)) (("x","y"),"y")

test s m e  = runStateS s m == e

main  = print $ and [t1,t2,t3]
