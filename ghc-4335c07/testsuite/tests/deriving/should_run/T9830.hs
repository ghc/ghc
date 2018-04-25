{-# LANGUAGE StandaloneDeriving #-}
module Main where

import T9830a

deriving instance (Show a, Show b) => Show (ADT a b)

main :: IO ()
main = do
    putStrLn $ "Prec 6:  " ++ showsPrec 6  ("test" :?: "show") ""
    putStrLn $ "Prec 7:  " ++ showsPrec 7  ("test" :?: "show") ""
    putStrLn $ "Prec 9:  " ++ showsPrec 9  ("test" :?: "show") ""
    putStrLn $ "Prec 10: " ++ showsPrec 10 ("test" :?: "show") ""
