{-# OPTIONS -XRecursiveDo #-}

-- test let bindings, polymorphism is ok provided they are not 
-- isolated in a recursive segment
-- NB. this is not what Hugs does!

module Main (main) where

import Control.Monad.Fix 

t :: IO (Int, Int)
t = mdo let l [] = 0
            l (x:xs) = 1 + l xs
	return (l "1", l [1,2,3])

main :: IO ()
main = t >>= print
