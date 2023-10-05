{-# LANGUAGE PatternSynonyms #-}
module Main where

pattern P x y <- [x, y]

f (P True True) = True
f _             = False

g [True, True] = True
g _            = False


main = do
    mapM_ (print . f) tests
    putStrLn ""
    mapM_ (print . g) tests
  where
    tests = [ [True, True]
            , [True, False]
            , [True, True, True]
            -- , False:undefined
            ]
