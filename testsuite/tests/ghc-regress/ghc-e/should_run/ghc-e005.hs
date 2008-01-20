
import System.Environment
import System.IO

main :: IO ()
main = error "main got called"

foo :: IO ()
foo = do putStrLn "This is foo"
         getArgs >>= print
         hFlush stdout
         error "foo"

