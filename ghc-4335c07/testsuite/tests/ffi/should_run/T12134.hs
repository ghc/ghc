import Foreign.C.Types

foreign import ccall "many_floats" many :: CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CDouble -> IO ()

main = many 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5 11.5 12.5 13.5 14.5 15.5
