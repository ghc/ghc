{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import qualified Data.List as List
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Utils.Containers.Internal.BitUtil (wordSize)
import Utils.Containers.Internal.BitQueue
    ( BitQueue
    , emptyQB
    , snocQB
    , buildQ
    , toListQ )

default (Int)

main :: IO ()
main = defaultMain $ map testNum [0..(wordSize - 2)]

testNum :: Int -> Test
testNum n = testProperty ("Size "++show n) (prop_n n)

prop_n :: Int -> Gen Bool
prop_n n = checkList <$> vectorOf n (arbitrary :: Gen Bool)
  where
    checkList :: [Bool] -> Bool
    checkList values = toListQ q == values
      where
        q :: BitQueue
        !q = buildQ $ List.foldl' snocQB emptyQB values
