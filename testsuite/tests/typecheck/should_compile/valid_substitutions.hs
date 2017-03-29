module Foo where

import Prelude hiding (putStr)
import qualified System.IO
import Data.Maybe
import qualified ValidSubs

ps :: String -> IO ()
ps = putStrLn


test :: [Maybe a] -> [a]
test = _

test2 :: Integer -> ValidSubs.Moo
test2 = _

main :: IO ()
main = _ "hello, world"
