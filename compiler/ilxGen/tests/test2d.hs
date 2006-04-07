foreign import ccall "libHS_cbits.so" "get_prog_argc" unsafe primArgc :: Int

foreign import "ilxHello" unsafe ilxHello :: IO ()
foreign import "ilxBad" unsafe ilxBad :: IO ()


main = if (primArgc == 0) then ilxHello else ilxBad
