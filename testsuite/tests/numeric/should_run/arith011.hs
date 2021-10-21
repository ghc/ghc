-- !!! Testing Int and Word
module Main(main) where
import Data.Int
import Data.Word
import Data.Bits
import Data.Ix -- added SOF
import Control.Exception
import Control.Monad
import Numeric.Natural

main :: IO ()
main = test

test :: IO ()
test = do
   testIntlike "Int"    (0::Int)
   testIntlike "Int8"   (0::Int8)
   testIntlike "Int16"  (0::Int16)
   testIntlike "Int32"  (0::Int32)
   testIntlike "Int64"  (0::Int64)
   testIntlike "Word8"  (0::Word8)
   testIntlike "Word16" (0::Word16)
   testIntlike "Word32" (0::Word32)
   testIntlike "Word64" (0::Word64)
   testInteger
   testNatural

testIntlike :: (Bounded a, Integral a, Ix a, Show a, Read a, Bits a) => String -> a -> IO ()
testIntlike name zero = do
  putStrLn $ "--------------------------------"
  putStrLn $ "--Testing " ++ name
  putStrLn $ "--------------------------------"
  testBounded  zero
  testEnum     zero
  testReadShow zero
  testEq       zero
  testOrd      zero
  testNum      zero
  testReal     zero
  testIntegral zero
  testConversions zero
  testBits     zero True

testInteger  = do
  let zero = 0 :: Integer
  putStrLn $ "--------------------------------"
  putStrLn $ "--Testing Integer"
  putStrLn $ "--------------------------------"
  testEnum     zero
  testReadShow zero
  testEq       zero
  testOrd      zero
  testNum      zero
  testReal     zero
  testIntegral zero
  testBits     zero False

testNatural  = do
  let zero = 0 :: Natural
  putStrLn $ "--------------------------------"
  putStrLn $ "--Testing Natural"
  putStrLn $ "--------------------------------"
  testEnum     zero
  testReadShow zero
  testEq       zero
  testOrd      zero
  testNum      zero
  testReal     zero
  testIntegral zero
  testBits     zero False

-- In all these tests, zero is a dummy element used to get
-- the overloading to work

testBounded zero = do
  putStrLn "testBounded"
  print $ (minBound-1, minBound, minBound+1) `asTypeOf` (zero,zero,zero)
  print $ (maxBound-1, maxBound, maxBound+1) `asTypeOf` (zero,zero,zero)

testEnum zero = do
  putStrLn "testEnum"
  print $ take 10 [zero .. ]           -- enumFrom
  print $ take 10 [zero, toEnum 2 .. ] -- enumFromThen
  print [zero .. toEnum 20]            -- enumFromTo
  print [zero, toEnum 2 .. toEnum 20]  -- enumFromThenTo

testConversions zero = do
  putStrLn "testConversions"
  putStr "Integer : " >> print (map fromIntegral numbers :: [Integer])
  putStr "Int     : " >> print (map fromIntegral numbers :: [Int])
  putStr "Int8    : " >> print (map fromIntegral numbers :: [Int8])
  putStr "Int16   : " >> print (map fromIntegral numbers :: [Int16])
  putStr "Int32   : " >> print (map fromIntegral numbers :: [Int32])
  putStr "Int64   : " >> print (map fromIntegral numbers :: [Int64])
  putStr "Word8   : " >> print (map fromIntegral numbers :: [Word8])
  putStr "Word16  : " >> print (map fromIntegral numbers :: [Word16])
  putStr "Word32  : " >> print (map fromIntegral numbers :: [Word32])
  putStr "Word64  : " >> print (map fromIntegral numbers :: [Word64])
  where numbers = [minBound, 0, maxBound] `asTypeOf` [zero]

isNatural :: (Bits n) => n -> Bool
isNatural zero = not (isSigned zero) && bitSizeMaybe zero == Nothing

samples :: (Bits a, Num a) => a -> [a]
samples zero
  | isNatural zero = map fromInteger [0 .. 3]
  | otherwise      = map fromInteger ([-3 .. -1]++[0 .. 3])

table1 :: (Show a, Show b) => String -> (a -> b) -> [a] -> IO ()
table1 nm f xs = do
  sequence [ f' x | x <- xs ]
  putStrLn "#"
 where
  f' x = putStrLn (nm ++ " " ++ show x ++ " = " ++ show (f x))

table2 :: (Show a, Show b, Show c) => String -> (a -> b -> c) -> [a] -> [b] -> IO ()
table2 nm op xs ys = do
  sequence [ sequence [ op' x y | y <- ys ] >> putStrLn " "
           | x <- xs
           ]
  putStrLn "#"
 where
  op' x y = do s <- Control.Exception.catch
                        (evaluate (show (op x y)))
                        (\e -> return (show (e :: SomeExceptionWithLocation)))
               putStrLn (show x ++ " " ++ nm ++ " " ++ show y ++ " = " ++ s)

testReadShow zero = do
  putStrLn "testReadShow"
  print xs
  print (map read_show xs)
 where
  xs = samples zero
  read_show x = (read (show x) `asTypeOf` zero)

testEq zero = do
  putStrLn "testEq"
  table2 "==" (==) xs xs
  table2 "/=" (/=) xs xs
 where
  xs = samples zero

testOrd zero = do
  putStrLn "testOrd"
  table2 "<="       (<=)    xs xs
  table2 "< "       (<)     xs xs
  table2 "> "       (>)     xs xs
  table2 ">="       (>=)    xs xs
  table2 "`compare`" compare xs xs
 where
  xs = samples zero

testNum zero = do
  putStrLn "testNum"
  table2 "+"      (+)    xs xs
  table2 "-"      (-)    xs xs
  table2 "*"      (*)    xs xs
  if (isNatural zero)
  then table1 "negate" negate [0 `asTypeOf` zero]
  else table1 "negate" negate xs
 where
  xs = samples zero

testReal zero = do
  putStrLn "testReal"
  table1 "toRational" toRational xs
 where
  xs = samples zero

testIntegral zero = do
  putStrLn "testIntegral"
  table2 "`divMod` " divMod  xs xs
  table2 "`div`    " div     xs xs
  table2 "`mod`    " mod     xs xs
  table2 "`quotRem`" quotRem xs xs
  table2 "`quot`   " quot    xs xs
  table2 "`rem`    " rem     xs xs
 where
  xs = samples zero

testBits zero do_bitsize = do
  putStrLn "testBits"
  table2 ".&.  "            (.&.)         xs xs
  table2 ".|.  "            (.|.)         xs xs
  table2 "`xor`"            xor           xs xs
  unless (isNatural zero) $
    table1 "complement"       complement    xs
  table2 "`shiftL`"         shiftL        xs ([0..3] ++ [32,64])
  table2 "`shiftR`"         shiftR        xs ([0..3] ++ [32,64])
  table2 "`rotate`"         rotate        xs ([-3..3] ++ [-64,-32,32,64])
  table1 "bit"              (\ x -> (bit x) `asTypeOf` zero)   [(0::Int)..3]
  table2 "`setBit`"         setBit        xs ([0..3] ++ [32,64])
  table2 "`clearBit`"       clearBit      xs ([0..3] ++ [32,64])
  table2 "`complementBit`"  complementBit xs ([0..3] ++ [32,64])
  table2 "`testBit`"        testBit       xs ([0..3] ++ [32,64])
  if do_bitsize then table1 "bitSize" bitSize xs else return ()
  table1 "isSigned"         isSigned      xs
 where
  xs = samples zero
