--IOArray with lists

module Store2 where
import Data.IORef
import Data.Array.IO

type Store = IOArray Int [Int]

mkStore :: IO Store
mkStore = newArray (0, 9) []

addElemToBucket :: Store -> Int -> Int -> IO ()
addElemToBucket a k e =
  do buc <- readArray a k
     writeArray a k (e : buc)
