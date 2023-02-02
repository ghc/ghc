import Control.Exception

-- test that the out-of-band data in an ErrorCall exception is printed

main = throw (ErrorCallWithLocation "error" "out-of-band")
