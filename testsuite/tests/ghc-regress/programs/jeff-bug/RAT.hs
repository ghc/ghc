module RAT where

import LazyST

import Prelude hiding (read)
import Hawk
import DLX
import qualified Trans
import Utils

new      :: Register a => ST s (RAT s a b)
write    :: Register a => RAT s a b -> a -> b -> ST s ()
remove   :: Register a => RAT s a b -> a -> ST s ()
read     :: Register a => RAT s a b -> a -> ST s (Maybe b)  
clear    :: Register a => RAT s a b -> ST s ()

type RAT s a b = (STArray s a (Maybe b),a,a)

clear (xs,x1,x2)
  = do { mapM (\x -> writeSTArray xs x Nothing) [x1 .. x2]
       ; return ()
       }

replace rat (Reg r x) 
  = do { a <- read rat r
       ; let res = do { v <- a
                      ; return (Reg (Virtual v (Just r)) x) 
                      } 
                   `catchEx` Reg (Real r) x
       ; return res
       }
replace rat x = return $ convert x


new
  = do { x <- newSTArray (minBound,maxBound) Nothing
       ; return (x,minBound,maxBound)
       }

write (xy,_,_) x y = writeSTArray xy x (return y)
remove (xy,_,_) x = writeSTArray xy x Nothing
read  (xy,_,_) x = readSTArray xy x
