{-# LANGUAGE ViewPatterns, BangPatterns #-}
module Main where

import Control.Exception
main :: IO ()
main = do print (f False)
          print (f True)
          print (g undefined) `catchE` \_ -> putStrLn "g exception"
          print (h undefined) `catchE` \_ -> putStrLn "h exception"
          print (i undefined) `catchE` \_ -> putStrLn "i exception"
          putStrLn "Done"

catchE :: IO a -> (ErrorCall -> IO a) -> IO a
catchE = catch

f :: Bool -> String
f (view -> Nothing) = "Got Nothing"
f (view -> Just x)  = "Got Just " ++ show x

g :: Bool -> String
g (view -> x) = "g Got something"

h :: Bool -> String
h (view -> !x) = "h Got something"

i :: Bool -> String
i !(view -> x) = "i Got something"

view :: Bool -> Maybe Int
view False = Nothing
view True = Just 5

