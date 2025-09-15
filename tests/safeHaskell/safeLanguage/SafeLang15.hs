{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
-- | This shows how to use typeable to perform unsafe
-- casts. Basically its an example of what Safe Haskell
-- should disallow. SafeLang14 will do that.
module Main where

import SafeLang15_A
import Data.Typeable

data H = H String deriving (Typeable, Show)

data G = G Int deriving (Show)

deriving instance Typeable G

deriving instance Typeable P

main = do
    let h = H "Hello World"
        g = G 1
--        Just h' = (cast h) :: Maybe G
        Just p' = (cast p) :: Maybe G
        Just px = (cast $ incrG p') :: Maybe P
    putStrLn $ show h
    putStrLn $ show g
--    putStrLn $ show h'
    putStrLn $ showP p
    putStrLn $ show p'
    putStrLn $ showP px

incrG :: G -> G
incrG (G n) = G $ n + 1

