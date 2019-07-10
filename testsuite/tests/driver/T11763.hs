{-# LANGUAGE CPP #-}
main = do
#if !defined(VERSION_containers)
    putStrLn "OK"
#endif
