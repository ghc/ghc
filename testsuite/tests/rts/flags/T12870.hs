--We check if RTS arguments are properly filtered/passed along
--by outputting them to stdout.

import System.Environment

main :: IO ()
main = getArgs >>= putStr . show
