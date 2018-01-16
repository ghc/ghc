module Main where
import qualified PDupe
import qualified QDupe

main = putStrLn (PDupe.pkg ++ " " ++ QDupe.pkg)
