{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Data.IORef

main :: IO ()
main = do { (r :: IORef (t -> t)) <- newIORef id
             -- r <- newIORef i -- => Type-check error

          ; writeIORef r (\v -> if v then False else True)

          ; c <- readIORef r

          ; print $ c True
          ; print $ c 1234 }
