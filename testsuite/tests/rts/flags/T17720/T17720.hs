import System.Environment

main :: IO ()
main = getArgs >>= putStr . show