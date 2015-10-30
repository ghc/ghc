{-# LANGUAGE CPP #-}
main = do
#ifndef VERSION_containers
    putStrLn "OK"
#endif
#ifndef MIN_VERSION_base
    putStrLn "OK"
#endif
