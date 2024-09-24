import Foreign.C.String

main :: IO ()
main = withCString "Some string" foo

foreign import javascript "foo" foo :: CString -> IO ()
