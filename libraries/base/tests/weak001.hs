import Foreign
import System.Mem.Weak

kill:: Ptr a -> IO ()
kill a = do
    w <- mkWeakPtr a Nothing
    addFinalizer a $
        deRefWeak w >> return ()

main:: IO ()
main = sequence_ . take 10000 . repeat $
    mallocBytes 100 >>= kill >> return ()
