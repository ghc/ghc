
import System.Posix.Process

main :: IO ()
main = executeFile "echo" True ["arg1", "ar   g2"] Nothing

