module Main where

import Control.Exception

data WorldExploded = WorldExploded
  deriving (Show)

instance Exception WorldExploded

main :: IO ()
main = do
  tid <- fork $ catch handler $ threadDelay 1000*1000*1000
  throwTo WorldExploded tid

handler :: ExceptionWithContext WorldExploded -> IO ()
handler (ExceptionWithContext ann WorldExploded) = do
  putStrLn "killed from"
  print ann

