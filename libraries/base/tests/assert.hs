
import Control.Exception

-- We want to get the assertion failure, not the overflow exception.
-- trac #5561.

main :: IO ()
main = let e1 i = throw Overflow
       in assert False (e1 5)
