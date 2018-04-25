{-# LANGUAGE DatatypeContexts #-}
module ShouldFail where

infixr 1 `rangeOf`

data Ord a => SubRange a = SubRange (a, a) a

type IntSubRange = SubRange Int 


subRangeValue :: SubRange a -> a
subRangeValue (SubRange (lower, upper) value) = value

subRange :: SubRange a -> (a, a)
subRange (SubRange r value) = r

newRange :: (Ord a, Show a) => (a, a) -> a -> SubRange a
newRange r value = checkRange (SubRange r value)


checkRange :: (Ord a, Show a) => SubRange a -> SubRange a
checkRange (SubRange (lower, upper) value)
  = if (value < lower) || (value > upper) then
      error ("### sub range error. range = " ++ show lower ++ 
              ".." ++ show upper ++ " value = " ++ show value ++ "\n")
    else
      SubRange (lower, upper) value


instance Eq a => Eq (SubRange a) where
  (==) a b = subRangeValue a == subRangeValue  b

instance (Ord a) => Ord (SubRange a) where
  (<)  = relOp (<)
  (<=) = relOp (<=)
  (>=) = relOp (>=)
  (>)  = relOp (>)

relOp :: Ord a => (a->a->Bool) -> SubRange a -> SubRange a -> Bool
relOp op a b = (subRangeValue a) `op` (subRangeValue b)

rangeOf :: (Ord a, Show a) => SubRange a -> SubRange a -> SubRange a
rangeOf a b = checkRange (SubRange (subRange b) (subRangeValue a))

showRange :: Show a => SubRange a -> String
showRange (SubRange (lower, upper) value)
  = show value ++ " :" ++ show lower ++ ".." ++ show upper

showRangePair :: (Show a, Show b) => (SubRange a, SubRange b) -> String
showRangePair (a, b)
  = "(" ++ showRange a ++ ", " ++ showRange b ++ ")"

showRangeTriple :: (Show a, Show b, Show c) =>
                   (SubRange a, SubRange b, SubRange c) -> String
showRangeTriple (a, b, c) 
  = "(" ++ showRange a ++ ", " ++ showRange b ++ ", " ++ showRange c ++ ")"



instance Num a => Num (SubRange a) where
  negate = numSubRangeNegate
  (+) = numSubRangeAdd
  (-) = numSubRangeSubtract
  (*) = numSubRangeMultiply
  fromInteger a = SubRange (fromInteger a, fromInteger a) (fromInteger a)

numSubRangeNegate :: (Ord a, Show a, Num a) => SubRange a -> SubRange a
numSubRangeNegate (SubRange (lower, upper) value)
  = checkRange (SubRange (lower, upper) (-value))

numSubRangeBinOp :: Num a => (a -> a -> a) -> 
                    SubRange a -> SubRange a -> SubRange a
numSubRangeBinOp op a b
  = SubRange (result, result) result
    where
    result = (subRangeValue a) `op` (subRangeValue b)

-- partain:
numSubRangeAdd, numSubRangeSubtract, numSubRangeMultiply :: Num a => SubRange a -> SubRange a -> SubRange a

numSubRangeAdd = numSubRangeBinOp (+)
numSubRangeSubtract = numSubRangeBinOp (-)
numSubRangeMultiply = numSubRangeBinOp (*)

unsignedBits :: Int -> (Int, Int)
unsignedBits n = (0, 2^n-1)

signedBits :: Int -> (Int, Int)
signedBits n = (-2^(n-1), 2^(n-1)-1)   


si_n :: Int -> Int -> IntSubRange
si_n bits value = SubRange (signedBits bits) value

si8, si10, si16 :: Int -> IntSubRange
si8  = si_n 8
si10 = si_n 10
si16 = si_n 16
