-- !!! Testing Int and Word
module Main(main) where
import Int
import Word
import Bits
import Ix -- added SOF

main :: IO ()
main = test

test :: IO ()
test = do
   testIntlike "Int8"   (0::Int8)     
   testIntlike "Int16"  (0::Int16)    
   testIntlike "Int32"  (0::Int32)    
   testIntlike "Word8"  (0::Word8)    
   testIntlike "Word16" (0::Word16)   
   testIntlike "Word32" (0::Word32)   

testIntlike :: (Bounded a, Integral a, Ix a, Read a, Bits a) => String -> a -> IO ()
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
  testBits     zero
  putStrLn $ "--------------------------------"

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

samples :: (Num a, Enum a) => a -> ([a], [a])
samples zero = ([-3 .. -1]++[0 .. 3], [-3 .. -1]++[1 .. 3])
  
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
  op' x y = putStrLn (show x ++ " " ++ nm ++ " " ++ show y 
                      ++ " = " ++ show (op x y))

testReadShow zero = do
  putStrLn "testReadShow"
  print xs
  print (map read_show xs)
 where
  (xs,zs) = samples zero
  read_show x = (read (show x) `asTypeOf` zero)

testEq zero = do
  putStrLn "testEq"
  table2 "==" (==) xs xs
  table2 "/=" (/=) xs xs
 where
  (xs,ys) = samples zero

testOrd zero = do
  putStrLn "testOrd"
  table2 "<="  	    (<=)    xs xs
  table2 "< "  	    (<)     xs xs
  table2 "> "  	    (>)     xs xs
  table2 ">="  	    (>=)    xs xs
  table2 "`compare`" compare xs xs
 where
  (xs,ys) = samples zero

testNum zero = do
  putStrLn "testNum"
  table2 "+"  	  (+)    xs xs
  table2 "-"  	  (-)    xs xs
  table2 "*"  	  (*)    xs xs
  table1 "negate" negate xs
 where
  (xs,ys) = samples zero

testReal zero = do
  putStrLn "testReal"
  table1 "toRational" toRational xs
 where
  (xs,ys) = samples zero

testIntegral zero = do
  putStrLn "testIntegral"
  table2 "`divMod` " divMod  xs ys
  table2 "`div`    " div     xs ys
  table2 "`mod`    " mod     xs ys
  table2 "`quotRem`" quotRem xs ys
  table2 "`quot`   " quot    xs ys
  table2 "`rem`    " rem     xs ys
 where
  (xs,ys) = samples zero

testBits zero = do
  putStrLn "testBits"
  table2 ".&.  "            (.&.)         xs ys
  table2 ".|.  "            (.|.)         xs ys
  table2 "`xor`"            xor           xs ys
  table1 "complement"       complement    xs
  table2 "`shiftL`"         shiftL        xs [0..3] 
  table2 "`shiftR`"         shiftR        xs [0..3] 
  table2 "`rotate`"         rotate        xs ([-3..3])
  table1 "bit"              (\ x -> (bit x) `asTypeOf` zero)   [(0::Int)..3]
  table2 "`setBit`"         setBit        xs [0..3]
  table2 "`clearBit`"       clearBit      xs [0..3]
  table2 "`complementBit`"  complementBit xs [0..3]
  table2 "`testBit`"        testBit       xs [0..3]
  table1 "bitSize"          bitSize       xs
  table1 "isSigned"         isSigned      xs
 where
  (xs,ys) = samples zero
