-- Test infix operators of 'Functor'

import Data.Functor

main :: IO ()
main = do
        testInfixFmap
        testFlippedInfixFmap
        testInfixReplace
        testFlippedInfixReplace

testInfixFmap :: IO ()
testInfixFmap = do
        print "<$> tests:"
        print $ (+ 1) <$> Just 2 -- => Just 3
        print (((+ 1) <$> Right 3) :: Either Int Int) -- => Right 4
        print $ (+ 1) <$> [1, 2, 3] -- => [2,3,4]

testFlippedInfixFmap :: IO ()
testFlippedInfixFmap = do
        print "<&> tests:"
        print $ Just 2 <&> (+ 1) -- => Just 3
        print ((Right 3 <&> (+ 1)) :: Either Int Int) -- => Right 4
        print $ [1, 2, 3] <&> (+ 1) -- => [2,3,4]

testInfixReplace :: IO ()
testInfixReplace = do
        print "<$ tests:"
        print $ 42 <$ Just 1 -- => Just 42
        print ((42 <$ Right 1) :: Either Int Int) -- => Right 42
        print $ 42 <$ [1, 2, 3] -- => [42,42,42]

testFlippedInfixReplace :: IO ()
testFlippedInfixReplace = do
        print "$> tests:"
        print $ Just 1 $> 42 -- => Just 42
        print ((Right 1 $> 42) :: Either Int Int) -- => Right 42
        print $ [1, 2, 3] $> 42 -- => [42,42,42]
