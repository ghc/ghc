{-# LANGUAGE LambdaCase #-}

module Main where

f = curry $ \case (Just x,  Left y)            -> Just (x, y)
                  (Nothing, Right y) | y == 99 -> Just (0, "99")
                  _                            -> Nothing

main = print $ [ f (Just 1) (Left "Y") == Just (1, "Y")
               , f (Just 1) (Right 99) == Nothing
               , f Nothing  (Right 99) == Just (0, "99")
               , f Nothing  (Right 9)  == Nothing
               , f Nothing  (Left "Y") == Nothing ]

