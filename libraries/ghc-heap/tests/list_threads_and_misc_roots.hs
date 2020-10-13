
foreign import ccall safe "list_threads_and_misc_roots_c.h check_tso_and_misc_roots"
    check_tso_and_misc_roots :: IO ()

main :: IO ()
main = check_tso_and_misc_roots
