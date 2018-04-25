module Main where
import Control.Concurrent
import Control.Exception

main = forkIO (error "Successful exception") >> threadDelay 100000
