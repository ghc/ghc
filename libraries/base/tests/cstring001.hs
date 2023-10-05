import Control.Monad
import Foreign.C.String

test_strings = ["Hello World", replicate 10000 'a']

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual x y = if x == y then return () else error $ "assertEqual: " ++ show x ++ " /= " ++ show y

main = do
    -- Try roundtripping some ASCII strings through the locale encoding
    forM test_strings $ \try_str -> do
        got_str <- withCString try_str peekCString
        got_str `assertEqual` try_str

    -- Try roundtripping some ASCII strings with lengths through the locale encoding
    forM test_strings $ \try_str -> do
        got_str <- withCStringLen try_str peekCStringLen
        got_str `assertEqual` try_str
