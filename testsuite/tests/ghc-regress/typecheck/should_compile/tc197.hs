{-# OPTIONS -fglasgow-exts #-}

-- Another dependency analysis test
-- Notice that 'a' and 'b' are mutually recursive, 
-- but have different contexts. 
--
-- This is the program submitted by Robert van Herk [rherk@cs.uu.nl]
-- to motivate the refined dependency analysis.

module ShouldCompile where
import Data.IORef

class MyReader r v | r -> v where
  myRead :: r -> IO v

data R v = R (IORef v)
instance MyReader (R v) v where
  myRead (R v) =
    do v <- readIORef v
       return v


a :: IO ()
a =
  do r <- createReader
     b r

b :: MyReader r Int => r -> IO ()
b r =
  do i <- myRead r
     if i > 10
       then a
       else putStrLn (show i)

createReader :: IO (R Int)
createReader =
  do ref <- newIORef 0
     return (R ref)
 
