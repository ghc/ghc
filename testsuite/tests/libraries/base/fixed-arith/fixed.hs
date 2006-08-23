{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Fixed

for :: (Monad m) => (a -> m ()) -> [a] -> m ()
for _ [] = return ()
for f (x:xs) = f x >> for f xs

nums :: (Fractional a) => [a]
nums = [0,7,7.1,7.01,7.9,7.09,5 + 7,3.2 - 7.8,5.75 * (-2)]

micronums :: [Micro]
micronums = nums

piconums :: [Pico]
piconums = nums

main :: IO ()
main = do
	for (\f -> for (for (putStrLn . f)) [micronums,fmap negate micronums]
		) [show,showFixed True]
	for (\f -> for (for (putStrLn . f)) [piconums,fmap negate piconums]
		) [show,showFixed True]
