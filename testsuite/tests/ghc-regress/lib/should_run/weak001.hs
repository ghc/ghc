import Addr
import Weak

kill:: Addr -> IO ()
kill a = do
    w <- mkWeakPtr a Nothing
    addFinalizer a $
        deRefWeak w >> return ()

main:: IO ()
main = sequence_ . take 10000 . repeat $
    malloc 100 >>= kill >> return ()

foreign import malloc :: Int -> IO Addr
