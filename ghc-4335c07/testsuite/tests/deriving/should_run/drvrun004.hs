module Main where

data Hash = Hash{ (#) :: Int }
 deriving (Show, Read)

main =
  do print s
     print (read s :: Hash)
 where
  s = show (Hash 3)
