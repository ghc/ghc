--Immutable array with IORefs to lists

module Store1 where
import Data.IORef
import Data.Array

type Store = Array Int (IORef [Int])

mkStore :: IO Store
mkStore =
  do buckets <- sequence ( take 10 $ repeat (newIORef []) )
     let myArray = listArray (0, 9) buckets
     return $! myArray

addElemToBucket :: Store -> Int -> Int -> IO ()
addElemToBucket a k e =
  do buc <- readIORef (a!k)
     writeIORef (a!k) (e : buc)
