module RecompCompleteFixityB where

import RecompCompleteFixityA

main :: IO ()
main = do
    let lst = (1 :: Int) :! 2 :! 3 :! []
    case lst of
        a :! b :! c :! [] -> print (a, b, c)
        _ -> putStrLn "No match"
