{-# LANGUAGE CPP #-}
main = do
#ifndef VERSION_containers
    putStrLn "OK"
#endif
