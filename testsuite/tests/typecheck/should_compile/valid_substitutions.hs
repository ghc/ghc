module Foo where

import Prelude hiding (putStr, putStrLn)
import qualified System.IO (putStr, putStrLn)
import Data.Maybe
import qualified ValidSubs

ps :: String -> IO ()
ps = putStrLn

a :: Int -> IO Int
a _ = return 1
b :: Int -> IO Int
b _ = return 2
c :: Int -> IO Int
c _ = do { x <- a 0
         ; y <- _ x
         ; return y }

test :: [Maybe a] -> [a]
test = _

test2 :: Integer -> ValidSubs.Moo
test2 = _

k :: Maybe Integer
k = _ 2

f :: String
f = show _


h :: String
h = show (_ (_ :: Bool))

main :: IO ()
main = _ "hello, world"
