-- This gave a Lint error in HEAD (Jun 24)
module T24623 where

{-# NOINLINE app #-}
app :: Int -> (Int -> (Int,Int)) -> (Int,Int)
app x f = if x>0 then f x else (0,0)

foo :: String -> Bool -> Bool -> Int -> (Int,Int)
foo s b b2 y = app y (let {-# NOINLINE j #-}
                          j :: Int -> (Int,Int)
                          j = \z -> error s
                      in case b of
                         True -> j
                         False -> case b2 of
                                    True -> \x -> (x-1, x+1)
                                    False -> j)
