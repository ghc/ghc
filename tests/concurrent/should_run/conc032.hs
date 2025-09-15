-- !!! this test exposed a bug in the take/putMVar implementation in
-- !!! GHC 5.00.  It involves multiple blocking takes & puts on the
-- !!! same MVar.

import Control.Concurrent
import System.IO.Unsafe

awk True  True  z     = 1
awk False y     True  = 2
awk x     False False = 3

awk'1 True  True  z     = 1
awk'2 False y     True  = 2
awk'3 x     False False = 3

awk' x y z | ppm [a1'1,a1'2,a1'3] (x,y,z) = awk'1 x y z
           | ppm [a2'1,a2'2,a2'3] (x,y,z) = awk'2 x y z
           | ppm [a3'1,a3'2,a3'3] (x,y,z) = awk'3 x y z
           | otherwise                      = 0

a1'1 (True,y,z)  s = s True
a1'1 (x,y,z)     s = s False

a1'2 (x,True,z)  s = s True
a1'2 (x,y,z)     s = s False

a1'3 (x,y,z)     s = s True

a2'1 (False,y,z) s = s True
a2'1 (x,y,z)     s = s False

a2'2 (x,y,z)     s = s True

a2'3 (x,y,True)  s = s True
a2'3 (x,y,z)     s = s False

a3'1 (x,y,z)     s = s True

a3'2 (x,False,z) s = s True
a3'2 (x,y,z)     s = s False

a3'3 (x,y,False) s = s True
a3'3 (x,y,z)     s = s False

ppm fs as = unsafePerformIO (ppm' fs as)

ppm' fs as = do m <- newEmptyMVar
                let s = putMVar m
                hs <- sequence [forkIO (f as s)|f <- fs]
                result <- assess (length fs) m
                sequence (map killThread hs)
                return result
                  where assess 0 m = return True
                        assess n m = do h <- takeMVar m
                                        if h then (assess (n-1) m)
                                             else return False

main = do sequence [putStrLn (show (awk' x y z))|(x,y,z) <- args]
            where args = [
                          (t,t,t),
                          (t,t,f),
                          (t,f,t),
                          (t,f,f),
                          (f,t,t),
                          (f,t,f),
                          (f,f,t),
                          (f,f,f),
                          (t,t,n)
                          --(f,n,t),
                          --(n,f,f),
                         ]
                  t    = True
                  f    = False
                  n    = odd (last [1..])
