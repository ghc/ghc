import Control.Monad.X.Reader


t1    = test "x" ask "x"
t2    = test "x" (local ('a':) ask) "ax"
t3    = test "x"
       (do x <- ask 
           y <- local ('a':) ask
           z <- ask
           return (x,y,z)) ("x","ax","x")
t4    = test "x" (local ('a':) (local ('b':) ask)) "bax"

test r m e  = runReader r m == e

main  = print $ and [t1,t2,t3,t4]

