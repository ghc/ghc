module D where

-- data types and an instance
data D a = A Int | B Float deriving Eq
newtype N a = N Double
type T a = (Int,Double)

-- a class
class C a where c :: a -> Int

-- a function
d :: Float -> Float
d x = x / 3
