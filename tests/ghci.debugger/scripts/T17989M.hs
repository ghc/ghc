import qualified T17989A
import qualified T17989B as B
import           T17989C

main :: IO ()
main = putStrLn (T17989A.foo 3 <> B.foo 5 <> foo 7)
