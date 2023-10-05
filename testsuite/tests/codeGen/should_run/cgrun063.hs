
{-
Check that we aren't making gcc misinterpret our strings as trigraphs.
#2968.
http://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html
-}

module Main where

main :: IO ()
main = do putStrLn "??("
          putStrLn "??)"
          putStrLn "??<"
          putStrLn "??>"
          putStrLn "??="
          putStrLn "??/"
          putStrLn "??'"
          putStrLn "??!"
          putStrLn "??-"

