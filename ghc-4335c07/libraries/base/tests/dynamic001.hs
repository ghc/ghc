-- !!! Dynamic library regression tests
module Main(main) where

import Data.Dynamic

main :: IO ()
main = do
   test   "toDyn"   toDyn_list
   testIO "fromDyn" fromDyn_test

toDyn_list :: [Dynamic]
toDyn_list =
    [ toDyn (1::Int)
    , toDyn ('a')
    , toDyn False
    , toDyn ((-1.0)::Float)
    , toDyn (0.0::Double)
    , toDyn (1394::Integer)
    , toDyn (print "hello")
    , toDyn toDyn_list
    , toDyn ([]::[Int])
    , toDyn (Nothing  :: Maybe Int)
    , toDyn ((Just 2) :: Maybe Int)
    , toDyn ((Just 2) :: Maybe Int)
    , toDyn ((Left 3) :: Either Int Bool)
    , toDyn ((Right 3) :: Either Char Int)
    , toDyn ()
    , toDyn LT
    , toDyn ((),2::Int)
    , toDyn ((),2::Int,'a')
    , toDyn ((),2::Int,'a',1.0::Double)
    , toDyn ((),2::Int,'a',1.0::Double,Nothing::Maybe Bool)
    , toDyn ((+) :: Int -> Int -> Int)
    , toDyn ((+) :: Integer -> Integer -> Integer)
    , toDyn ((++) :: [Char] -> [Char] -> [Char])
    ]

-- Testing the conversion from Dynamic values:
fromDyn_test :: IO ()
fromDyn_test = do
   print (fromDyn (toDyn (1::Int)) (0::Int))
   print (fromDyn (toDyn ('a'::Char)) (0::Int))
   print (fromDyn (toDyn 'a') 'b')
   print (fromDyn (toDyn (1::Float)) (0::Float))
   print (fromDyn (toDyn (2::Float)) (0::Int))
   print (fromDyn (toDyn (3::Double)) (0::Double))
   print (fromDyn (toDyn (4::Double)) (0::Int))
   print (fromDyn (toDyn (5::Integer)) (0::Integer))
   print (fromDyn (toDyn (6::Integer)) False)
   print (fromDyn (toDyn [1,3,5::Integer]) ([]::[Integer]))
   print (fromDyn (toDyn (Just True)) (Nothing::Maybe Bool))
   print (fromDyn (toDyn (Left True::Either Bool Bool)) (Right False :: Either Bool Bool))
   print (fromDyn (toDyn LT) GT)
   print (fromDyn (toDyn ((+1)::Int->Int)) False)
   print ((fromDyn (toDyn ((+1)::Int->Int)) ((+2)::Int->Int)) 3)
   print ((fromDyn (toDyn ((++)::[Int]->[Int]->[Int])) ((undefined)::[Int]->[Int]->[Int])) [1] [2])

    
-- Misc test utilities:
test :: Show a => String -> [a] -> IO ()
test str ls = do
  putStrLn ("*** Testing: " ++ str ++ " ***")
  putStrLn (showListLn ls)

testIO :: String -> IO () -> IO ()
testIO str tst = do
  putStrLn ("*** Testing: " ++ str ++ " ***")
  tst


-- showListLn presents a list in a diff-friendly format.
-- showListLn [a1,..an]
--  =>
--      [ a1
--      , a2
--      ..
--      , an
--      ]
--   
showListLn :: Show a => [a] -> String
showListLn [] = ""
showListLn ls = '[' : ' ' : go ls
 where
   go    [x] = show x ++ "\n]"
   go (x:xs) = show x ++ '\n':',':' ':go xs

{-
test8 = toDyn (mkAppTy listTc)
test9 :: Float
test9 = fromDyn test8 0

printf :: String -> [Dynamic] -> IO ()
printf str args = putStr (decode str args)
 where
  decode [] [] = []
  decode ('%':'n':cs) (d:ds) =
    (\ v -> show v++decode cs ds) (fromDyn  d (0::Int))
  decode ('%':'c':cs) (d:ds) =
    (\ v -> show v++decode cs ds) (fromDyn  d ('\0'))
  decode ('%':'b':cs) (d:ds) =
    (\ v -> show v++decode cs ds) (fromDyn  d (False::Bool))
  decode (x:xs) ds = x:decode xs ds

test10 :: IO ()
test10 = printf "%n = %c, that much is %b\n" [toDyn (3::Int),toDyn 'a', toDyn False]

-}
