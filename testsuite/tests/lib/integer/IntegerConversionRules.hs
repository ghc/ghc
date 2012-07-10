
module IntegerConversionRules where

import Data.Word

f1 :: Int -> Double
f1 = fi

f2 :: Int -> Float
f2 = fi

f3 :: Int -> Int
f3 = fi

f4 :: Int -> Word
f4 = fi

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

