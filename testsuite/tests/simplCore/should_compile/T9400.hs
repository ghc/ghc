module T9400 (main) where
import GHC.Base

str = "defg"

main :: IO ()
main = do
    case "abc" of
        (x:y:xs) -> putStrLn xs
    case "" of
        [] -> putStrLn "x"
    case "ab" of
        [] -> putStrLn "y"
        (x:y:[]) -> putStrLn "z"
    case str of
        (x:xs) -> putStrLn xs
    case "ab" of
        "" -> putStrLn "fail"
