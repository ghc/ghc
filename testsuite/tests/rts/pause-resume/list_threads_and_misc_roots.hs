
foreign import ccall safe "list_threads_and_misc_roots_c.h checkGcRoots"
    checkGcRoots :: IO ()

main :: IO ()
main = checkGcRoots
