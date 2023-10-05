-- !!! Tickled a bug in core2stg 
-- !!! (CoreSyn.Coerce constructors were not peeled off
-- !!! when converting CoreSyn.App)

module Main where

getData :: String -> IO ()
getData filename = case leng filename of {0 -> return ()}
leng :: String -> Int
leng [] = 0 --case ls of {[] -> 0 ; (_:xs) -> 1 + leng xs }
leng ls = leng ls

f [] [] = []
f xs ys = f xs ys

main =
     return ()  >>= \ _ ->
     case f [] [] of { [] -> getData [] }
