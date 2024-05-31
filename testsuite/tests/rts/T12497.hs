import Foreign.C.String

foreign import ccall "_strdup" strdup :: CString -> IO CString
foreign import ccall "strdup" strdup2 :: CString -> IO CString

dupString :: String -> IO String
dupString str = newCString str >>= strdup >>= peekCString

dupString2 :: String -> IO String
dupString2 str = newCString str >>= strdup2 >>= peekCString

main =
 do print =<< dupString  "Hello World!"
    print =<< dupString2 "Hello Again World!"
