module BoundedSet
  ( new
  , readBound
  , readSize
  , read  
  , clear
  , insert
  , spaceAvail
  , rmSuch 
  , rmSuchN
  , BoundedSet
  , iterateSet
  ) where

import LazyST
import Prelude hiding (read)
import List    hiding (insert)


new        :: Int -> ST s (BoundedSet s a)
readBound  :: BoundedSet s a -> ST s Int
readSize   :: BoundedSet s a -> ST s Int
read       :: BoundedSet s a -> ST s [a]
clear      :: BoundedSet s a -> ST s [a]
insert     :: BoundedSet s a -> [a] -> ST s ()
spaceAvail :: BoundedSet s a -> ST s Int
rmSuch     :: BoundedSet s a -> (a -> Bool) -> ST s [a]
rmSuchN    :: BoundedSet s a -> Int -> (a -> Bool) -> ST s [a]
iterateSet :: BoundedSet s a -> (a -> a) -> ST s ()


-- Implementation ----------------------------------------------------
type BoundedSet s a = (STRef s [a],Int)


iterateSet s f =
   do { set <- read s
      ; write s (map f set)
      }

read (s,n) = readSTRef s

rmSuch s f
  = do { set <- read s
       ; let (yes,no) = partition f set
       ; write s no
       ; return yes
       }

rmSuchN s n f 
  = do { such <- rmSuch s f
       ; let (big,small) = splitAt n such
       ; insert s small
       ; return big
       }

write    :: BoundedSet s a -> [a] -> ST s ()
write (s,n) x = writeSTRef s x


readBound (s,n) = return n

new n 
  = do { set <- newSTRef []
       ; return (set,n)
       }

clear s =
  do { set <- read s
     ; write s []
     ; return set
     }

readSize s =
  do { set <- read s
     ; return ( length set)
     }
	
spaceAvail s
  = do { bnd <- readBound s
       ; sz  <- readSize s
       ; return (bnd - sz)
       }
      

insert s l
  = do { set <- read s
       ; n <- readBound s
       ; write s $ take n (set ++ l)
       }



