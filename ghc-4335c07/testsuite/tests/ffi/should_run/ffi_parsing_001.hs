
import Foreign.C

-- This should not be parsed as "static foo", importing "foo"
foreign import ccall "staticfoo" x :: CInt

main :: IO ()
main = print x
