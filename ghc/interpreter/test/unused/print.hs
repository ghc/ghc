--!!! Testing top level printer (note that this doesn't necessarily test show)

-- Test things of type String

test1, test2, test3 :: String

test1 = "abcd"
test2 = ""
test3 = "abcd\0efgh\0"
test4 = "abc" ++ error "def" ++ "hij"
test5 = "abc" ++ [error "def"] ++ "hij"
test6 = 'a' : 'b' : 'c' : error "foo"
test7 = 'a' : 'b' : 'c' : error "foo" : []
test8 = show (error "foo"::String)

test11, test12 :: String
test11 = case (error "foo") of _ -> "abcd"
test12 = case (error "foo") of [] -> "abcd"

test13, test14 :: String
test13 = error (error "foo")
test14 = error test14



-- Test things of type IO ()

{- can't include this in backwards compatability tests

-- Normal

test101, test102, test103 :: IO ()
test101 = putStr "abcd"
test102 = return ()
test103 = putChar 'a'

-- Errors

test111, test112, test113, test114 :: IO ()
test111 = error "foo"
test112 = putStr (error "foo")
test113 = putStr "abcd" >> putStr (error "foo") >> putStr "efgh"
test114 = putStr "abcd" >> error "foo" >> putStr "efgh"

test123, test124, test125 :: IO ()
test123 = error (error "foo")
test124 = error x where x = error x
test125 = error x where x = 'a' : error x

-}

-- Test things of type a

-- Unit

test241, test242 :: ()
test241 = ()
test242 = error "foo"

-- Ints

test251, test252 :: Int
test251 = 10
test252 = -10

test253, test254 :: Int
test253 = 42 + error "foo"
test254 = error "foo" + 42

-- Integers

test261, test262 :: Integer
test261 = 10
test262 = 10

-- Floats

test271, test272 :: Float
test271 = 10
test272 = -10

-- Doubles

test281, test282 :: Double
test281 = 10
test282 = -10

-- Char

test291, test292, test293 :: Char
test291 = 'a'
test292 = '\0'
test293 = '\DEL'

-- Lists

test301, test302 :: [Int]
test301 = []
test302 = [1]

-- Bool

test311 = True
test312 = False

-- Tuples

test321 = ('a','b')
test322 = ('a','b','c')

test323 :: (Int,Int, Int)
test323 = (1, error "foo", 3)

-- Datatypes

data E a b = L a | R b
test331 = R (1::Int)
test332 = L 'a'

data M a = N | J a
test333 = J True
test334 = N

-- No dialogue tests in this file
