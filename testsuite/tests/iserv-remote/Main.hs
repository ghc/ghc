{-# LANGUAGE TemplateHaskell #-}

import Lib (x)

main = putStrLn "Hello World" >> print $(x 10)
