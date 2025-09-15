import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Data.Char
import Foreign.Marshal.Alloc
import System.IO

main :: IO ()
main = do
    -- avoid C and Haskell prints to stdout to be intermingled due to buffering on the Haskell side
    hSetBuffering stdout NoBuffering

    -- test sending int values back and forth
    -- test printing on stdout in C
    print =<< hello_c =<< hello_c =<< hello_c 17

    -- test printing an Haskell string in C
    withCString "Hello from Haskell" write_c

    -- test allocating a CString in C and printing it in Haskell
    c_str <- alloc_c
    print =<< peekCString c_str
    free c_str -- not really needed. The CString lives as an array on the JS heap and will be collected

    -- test modifying Haskell allocated bytes in C
    withCString "Hello from Haskell" $ \c_str -> do
        modify_c c_str
        print =<< peekCString c_str

    -- test calling back into Haskell from C
    let to_upper c = fromIntegral (ord (toUpper (chr (fromIntegral c))))
    cb <- mkCallback to_upper
    withCString "Hello from Haskell 1234" $ \c_str -> do
        callback_c c_str cb
        print =<< peekCString c_str
    freeHaskellFunPtr cb



foreign import javascript "hello_c_wrapper"    hello_c    :: Int -> IO Int
foreign import javascript "write_c_wrapper"    write_c    :: CString -> IO ()
foreign import javascript "alloc_c_wrapper"    alloc_c    :: IO CString
foreign import javascript "modify_c_wrapper"   modify_c   :: CString -> IO ()
foreign import javascript "callback_c_wrapper" callback_c :: CString -> (FunPtr (CChar -> CChar)) -> IO ()

foreign import ccall "wrapper" mkCallback :: (CChar -> CChar) -> IO (FunPtr (CChar -> CChar))

