import Control.Monad.X.Writer


t1    = test (tell "x") ((),"x")
t2    = test (listen (tell "x")) (((),"x"),"")
t3    = test
       (do tell "x"
           (_,y) <- listen (tell "y")
           tell "z"
           return y) 
              ("y","xz")

t4    = test (listen (listen (tell "x"))) ((((),"x"),""),"")

test m e  = runWriter m == e

main  = print $ and [t1,t2,t3,t4]

